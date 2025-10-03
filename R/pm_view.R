#' Quick viewer for PMTiles archives
#'
#' @description
#' Quickly visualize a PMTiles archive on an interactive map using mapgl.
#' Automatically detects the tile type and applies appropriate styling.
#' For local files, automatically starts a background server.
#'
#' @param input Path to a local PMTiles file or URL to a remote archive.
#' @param source_layer Name of the source layer to display. If `NULL` (default),
#'   automatically uses the first layer found in the metadata.
#' @param style Base map style. Can be a mapgl style function like
#'   `mapgl::openfreemap_style("positron")` or a style name. Default is `"positron"`.
#' @param port Port for local server (only used for local files). Default is 8080.
#' @param layer_type Type of layer to add: `"fill"`, `"line"`, `"circle"`, or `"auto"`.
#'   Default is `"auto"` which detects based on geometry type.
#' @param fill_color Fill color for polygons. Default is `"#088"`.
#' @param fill_opacity Fill opacity. Default is 0.5.
#' @param line_color Line color. Default is `"#088"`.
#' @param line_width Line width. Default is 1.
#' @param circle_color Circle color for points. Default is `"#088"`.
#' @param circle_radius Circle radius for points. Default is 5.
#' @param inspect_features Logical. If `TRUE`, adds hover effects and tooltips showing
#'   feature attributes. Default is `FALSE`.
#' @param ... Additional arguments passed to the layer function.
#'
#' @return A mapgl map object.
#'
#' @details
#' # Local vs Remote Files
#'
#' **Local files**: If `input` is a local file path, `pm_view()` automatically:
#' 1. Starts a background PMTiles server via `pm_serve()`
#' 2. Extracts metadata to determine source layer and geometry type
#' 3. Creates an appropriate map visualization
#'
#' The server runs in the background and will be automatically stopped when the
#' R session ends. You can manually stop it with `pm_stop_server()`.
#'
#' **Remote files**: If `input` is a URL (starts with `http://` or `https://`),
#' uses the URL directly without starting a local server.
#'
#' # Geometry Detection
#'
#' When `layer_type = "auto"`, the function inspects the metadata to determine
#' the geometry type and applies appropriate styling:
#' - Polygon/MultiPolygon → fill layer
#' - LineString/MultiLineString → line layer
#' - Point/MultiPoint → circle layer
#'
#' @examples
#' \dontrun{
#' # View a local PMTiles file
#' pm_view("data.pmtiles")
#'
#' # Customize the display
#' pm_view("data.pmtiles",
#'         fill_color = "#ff0000",
#'         fill_opacity = 0.7)
#'
#' # View remote PMTiles
#' pm_view("https://example.com/tiles.pmtiles")
#'
#' # Use specific source layer
#' pm_view("data.pmtiles", source_layer = "buildings")
#'
#' # Further customize with mapgl
#' pm_view("data.pmtiles") |>
#'   mapgl::fit_bounds(c(-122.5, 37.7, -122.3, 37.9))
#' }
#'
#' @export
pm_view <- function(
  input,
  source_layer = NULL,
  style = NULL,
  port = 8080,
  layer_type = c("auto", "fill", "line", "circle"),
  fill_color = "#088",
  fill_opacity = 0.5,
  line_color = "#088",
  line_width = 1,
  circle_color = "#088",
  circle_radius = 5,
  inspect_features = FALSE,
  ...
) {
  layer_type <- match.arg(layer_type)

  # Check if mapgl is available
  if (!requireNamespace("mapgl", quietly = TRUE)) {
    stop(
      "Package 'mapgl' is required for pm_view().\n",
      "Install it with: install.packages('mapgl')",
      call. = FALSE
    )
  }

  # Determine if input is local or remote
  is_remote <- grepl("^https?://", input)

  if (is_remote) {
    # Remote file - use URL directly
    pmtiles_url <- input
    message("Loading remote PMTiles: ", input)

    # Get metadata from remote source
    metadata <- pm_show(input, metadata = TRUE)
  } else {
    # Local file - start server and get metadata
    input <- path.expand(input)

    if (!file.exists(input)) {
      stop("File not found: ", input, call. = FALSE)
    }

    # Get metadata first
    metadata <- pm_show(input, metadata = TRUE)

    # Start server
    message("Starting local PMTiles server...")
    server <- pm_serve(input, port = port, background = TRUE)

    # Build PMTiles URL - include the .pmtiles extension
    filename <- basename(input)
    pmtiles_url <- paste0(server$url, "/", filename)

    message("PMTiles URL: ", pmtiles_url)
  }

  # Extract source layer info
  layer_index <- 1 # Track which layer we're using
  if (is.null(source_layer)) {
    if (
      !is.null(metadata$vector_layers) && length(metadata$vector_layers) > 0
    ) {
      source_layer <- metadata$vector_layers[[1]]$id
      layer_index <- 1
      message("Using source layer: ", source_layer)
    } else {
      stop(
        "Could not determine source layer. Please specify source_layer argument.",
        call. = FALSE
      )
    }
  } else {
    # Find the index of the specified source layer
    if (!is.null(metadata$vector_layers)) {
      for (i in seq_along(metadata$vector_layers)) {
        if (metadata$vector_layers[[i]]$id == source_layer) {
          layer_index <- i
          break
        }
      }
    }
  }

  # Determine geometry type for auto layer type
  if (layer_type == "auto") {
    if (!is.null(metadata$tilestats$layers)) {
      geom_type <- metadata$tilestats$layers[[1]]$geometry
      layer_type <- switch(
        geom_type,
        "Polygon" = "fill",
        "MultiPolygon" = "fill",
        "LineString" = "line",
        "MultiLineString" = "line",
        "Point" = "circle",
        "MultiPoint" = "circle",
        "fill" # default fallback
      )
      message(
        "Detected geometry: ",
        geom_type,
        " -> using ",
        layer_type,
        " layer"
      )
    } else {
      layer_type <- "fill"
      message("Could not detect geometry type, using fill layer")
    }
  }

  # Get bounds and zoom info for initial view
  bounds <- NULL
  if (!is.null(metadata$antimeridian_adjusted_bounds)) {
    bounds_str <- strsplit(metadata$antimeridian_adjusted_bounds, ",")[[1]]
    bounds <- as.numeric(bounds_str)
  }

  # Get min/max zoom from the specific layer we're displaying
  minzoom <- NULL
  maxzoom <- NULL
  if (
    !is.null(metadata$vector_layers) &&
      length(metadata$vector_layers) >= layer_index
  ) {
    layer_metadata <- metadata$vector_layers[[layer_index]]
    minzoom <- layer_metadata$minzoom
    maxzoom <- layer_metadata$maxzoom
  }

  # Create base map with minZoom constraint if tiles have a minzoom
  if (is.null(style)) {
    style <- mapgl::openfreemap_style("positron")
  }

  map <- mapgl::maplibre(
    style = style,
    bounds = bounds,
    minZoom = minzoom
  ) |>
    mapgl::set_projection("globe")

  # Add PMTiles source
  map <- mapgl::add_pmtiles_source(
    map,
    id = "pmtiles",
    url = pmtiles_url
  )

  # Build hover_options and popup if inspect_features is TRUE
  hover_opts <- NULL
  popup_val <- NULL
  if (inspect_features) {
    if (layer_type == "fill") {
      hover_opts <- list(fill_color = "yellow", fill_opacity = 1)
    } else if (layer_type == "line") {
      hover_opts <- list(line_color = "yellow", line_width = line_width + 2)
    } else if (layer_type == "circle") {
      hover_opts <- list(
        circle_color = "yellow",
        circle_radius = circle_radius + 2
      )
    }

    # Extract attribute names from tilestats or vector_layers to build popup
    attr_names <- NULL

    # Try tilestats first (tippecanoe format)
    if (
      !is.null(metadata$tilestats$layers) &&
        length(metadata$tilestats$layers) >= layer_index
    ) {
      layer_stats <- metadata$tilestats$layers[[layer_index]]
      if (
        !is.null(layer_stats$attributes) && length(layer_stats$attributes) > 0
      ) {
        # Extract the attribute names from the list structure
        attr_names <- sapply(layer_stats$attributes, function(x) x$attribute)
      }
    }

    # Fallback to vector_layers.fields (e.g., Overture Maps, Planetiler format)
    if (is.null(attr_names) && !is.null(metadata$vector_layers) &&
        length(metadata$vector_layers) >= layer_index) {
      vector_layer <- metadata$vector_layers[[layer_index]]
      if (!is.null(vector_layer$fields)) {
        attr_names <- names(vector_layer$fields)
      }
    }

    # Build tooltip HTML using concat() and get_column()
    if (!is.null(attr_names) && length(attr_names) > 0) {
      # Build concat() parts for tooltip
      tooltip_parts <- list()

      for (i in seq_along(attr_names)) {
        attr <- attr_names[i]

        # Add line break between attributes (except first)
        if (i > 1) {
          tooltip_parts <- c(tooltip_parts, list("<br>"))
        }

        # Add the attribute label and value
        tooltip_parts <- c(
          tooltip_parts,
          list("<strong>", attr, ":</strong> "),
          list(mapgl::get_column(attr))
        )
      }

      # Use concat() to combine all parts
      popup_val <- do.call(mapgl::concat, tooltip_parts)
    }
  }

  # Add appropriate layer type
  if (layer_type == "fill") {
    map <- mapgl::add_fill_layer(
      map,
      id = "pmtiles-layer",
      source = "pmtiles",
      source_layer = source_layer,
      fill_color = fill_color,
      fill_opacity = fill_opacity,
      hover_options = hover_opts,
      tooltip = popup_val,
      ...
    )
  } else if (layer_type == "line") {
    map <- mapgl::add_line_layer(
      map,
      id = "pmtiles-layer",
      source = "pmtiles",
      source_layer = source_layer,
      line_color = line_color,
      line_width = line_width,
      hover_options = hover_opts,
      tooltip = popup_val,
      ...
    )
  } else if (layer_type == "circle") {
    map <- mapgl::add_circle_layer(
      map,
      id = "pmtiles-layer",
      source = "pmtiles",
      source_layer = source_layer,
      circle_color = circle_color,
      circle_radius = circle_radius,
      hover_options = hover_opts,
      tooltip = popup_val,
      ...
    )
  }

  return(map)
}
