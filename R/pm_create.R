#' Create PMTiles or MBTiles from GeoJSON with tippecanoe
#'
#' Generate vector tiles from GeoJSON, FlatGeobuf, or CSV input using
#' tippecanoe. This function requires tippecanoe to be installed on your system.
#' See \url{https://github.com/felt/tippecanoe} for installation instructions.
#'
#' @param input An sf object, path to a GeoJSON/FlatGeobuf/CSV file, or a list
#'   for multi-layer output. For multi-layer PMTiles, provide either:
#'   \itemize{
#'     \item A list of sf objects or file paths (all layers share the same options)
#'     \item A list of \code{\link{pm_layer}} objects (each layer can have different options)
#'   }
#' @param output Path to output file (.pmtiles or .mbtiles)
#' @param layer_name Name for the layer in the tileset. If NULL, derived from
#'   input filename or a random string for sf objects (tippecanoe -l). For
#'   multi-layer input, can be a character vector of names (one per layer), or
#'   names will be derived from list names.
#'
#' @section Zoom Levels:
#' @param min_zoom Minimum zoom level (tippecanoe -Z, default 0)
#' @param max_zoom Maximum zoom level (tippecanoe -z, default 14)
#' @param guess_maxzoom If TRUE, guess appropriate maxzoom based on feature
#'   density (tippecanoe -zg)
#' @param smallest_maximum_zoom_guess Use specified zoom if lower maxzoom is
#'   guessed (tippecanoe --smallest-maximum-zoom-guess)
#' @param base_zoom Zoom at and above which all points are included
#'   (tippecanoe -B). If NULL, defaults to maxzoom.
#' @param extend_zooms_if_still_dropping Increase maxzoom if features still
#'   being dropped (tippecanoe -ae)
#'
#' @section Tile Resolution:
#' @param full_detail Detail at max zoom (default 12, for 4096 tile resolution,
#'   tippecanoe -d)
#' @param low_detail Detail at lower zooms (default 12, tippecanoe -D)
#' @param minimum_detail Minimum detail if tiles too big (default 7,
#'   tippecanoe -m)
#' @param extra_detail Generate tiles with extra detail for precision
#'   (tippecanoe --extra-detail)
#'
#' @section Filtering Attributes:
#' @param exclude Character vector of attribute names to exclude (tippecanoe -x)
#' @param include Character vector of attribute names to include, excluding all
#'   others (tippecanoe -y)
#' @param exclude_all If TRUE, exclude all attributes and encode only geometries
#'   (tippecanoe -X)
#'
#' @section Feature Dropping:
#' @param drop_rate Rate at which features dropped at zoom levels below basezoom
#'   (default 2.5, tippecanoe -r). Use "g" for auto-guess.
#' @param drop_densest_as_needed Reduce feature spacing if tile too large
#'   (tippecanoe -as)
#' @param drop_fraction_as_needed Drop fraction of features to keep under size
#'   limit (tippecanoe -ad)
#' @param drop_smallest_as_needed Drop smallest features to keep under size
#'   limit (tippecanoe -an)
#' @param drop_lines Apply dot-dropping to lines (tippecanoe -al)
#' @param drop_polygons Apply dot-dropping to polygons (tippecanoe -ap)
#'
#' @section Feature Coalescing:
#' @param coalesce Coalesce consecutive features with same attributes
#'   (tippecanoe -ac)
#' @param coalesce_smallest_as_needed Combine smallest features into nearby ones
#'   (tippecanoe -aN)
#' @param coalesce_densest_as_needed Combine densest features into nearby ones
#'   (tippecanoe -aD)
#' @param coalesce_fraction_as_needed Combine fraction of features into nearby
#'   ones (tippecanoe -aS)
#'
#' @section Clustering:
#' @param cluster_distance Cluster points within distance of each other
#'   (tippecanoe -K, max 255)
#' @param cluster_maxzoom Max zoom for clustering (tippecanoe -k). Use "g" to
#'   set to maxzoom - 1.
#'
#' @section Simplification:
#' @param simplification Multiply tolerance for line/polygon simplification
#'   (tippecanoe -S, default ~1)
#' @param no_line_simplification Don't simplify lines and polygons
#'   (tippecanoe -ps)
#' @param simplify_only_low_zooms Don't simplify at maxzoom (tippecanoe -pS)
#' @param no_tiny_polygon_reduction Don't combine tiny polygons into squares
#'   (tippecanoe -pt)
#' @param detect_shared_borders Detect and simplify shared polygon borders
#'   identically (tippecanoe -ab)
#' @param no_simplification_of_shared_nodes Don't simplify nodes where lines
#'   converge/diverge (tippecanoe -pn)
#'
#' @section Feature Ordering:
#' @param preserve_input_order Preserve original input order instead of
#'   geographic order (tippecanoe -pi)
#' @param reorder Reorder features to put same attributes in sequence
#'   (tippecanoe -ao)
#' @param hilbert Use Hilbert Curve order instead of Z-order (tippecanoe -ah)
#'
#' @section Tile Size Limits:
#' @param maximum_tile_bytes Maximum compressed tile size in bytes (default 500K,
#'   tippecanoe -M)
#' @param maximum_tile_features Maximum features per tile (default 200,000,
#'   tippecanoe -O)
#' @param no_feature_limit Don't limit tiles to 200,000 features
#'   (tippecanoe -pf)
#' @param no_tile_size_limit Don't limit tiles to 500K bytes (tippecanoe -pk)
#'
#' @section Other Options:
#' @param generate_ids Add feature IDs to features without them (tippecanoe -ai)
#' @param calculate_feature_density Add tippecanoe_feature_density attribute
#'   (tippecanoe -ag)
#' @param read_parallel Use multiple threads for line-delimited GeoJSON
#'   (tippecanoe -P)
#' @param attribution Attribution text for tileset (tippecanoe -A)
#' @param description Description for tileset (tippecanoe -N)
#' @param buffer Buffer size in screen pixels (default 5, tippecanoe -b)
#'
#' @param other_options Character vector of additional tippecanoe options not
#'   covered by other parameters. Example: c("-pf", "-pk", "--coalesce")
#' @param force If TRUE, overwrite existing output file (default TRUE,
#'   tippecanoe -f)
#' @param keep_geojson If TRUE, keep temporary GeoJSON file for sf objects
#' @param quiet If TRUE, suppress progress messages (tippecanoe -q)
#'
#' @return Path to output file (invisibly)
#'
#' @details
#' This function wraps the tippecanoe command-line tool. Tippecanoe must be
#' installed separately:
#' \itemize{
#'   \item macOS: \code{brew install tippecanoe}
#'   \item Ubuntu: \code{sudo apt-get install tippecanoe}
#'   \item From source: \url{https://github.com/felt/tippecanoe}
#' }
#'
#' The function handles sf objects by converting them to temporary GeoJSON files.
#' For faster GeoJSON writing with large datasets, install the \code{yyjsonr}
#' package, which can be significantly faster than the default \code{sf::st_write()}.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Simple usage with sf object
#' pm_create(
#'   my_sf_data,
#'   "output.pmtiles",
#'   max_zoom = 14
#' )
#'
#' # Complex parcel tileset
#' pm_create(
#'   "parcels.geojson",
#'   "parcels.pmtiles",
#'   layer_name = "parcels",
#'   min_zoom = 10,
#'   max_zoom = 18,
#'   full_detail = 15,
#'   preserve_input_order = TRUE,
#'   no_tiny_polygon_reduction = TRUE,
#'   coalesce_densest_as_needed = TRUE,
#'   coalesce_fraction_as_needed = TRUE,
#'   extend_zooms_if_still_dropping = TRUE,
#'   simplification = 1,
#'   detect_shared_borders = TRUE,
#'   other_options = c("-pf", "-pk", "-ai")
#' )
#'
#' # Point clustering
#' pm_create(
#'   points_sf,
#'   "points.pmtiles",
#'   max_zoom = 14,
#'   cluster_distance = 10,
#'   cluster_maxzoom = "g",
#'   generate_ids = TRUE
#' )
#'
#' # With attribute filtering
#' pm_create(
#'   roads_sf,
#'   "roads.pmtiles",
#'   include = c("name", "highway", "surface"),
#'   drop_densest_as_needed = TRUE,
#'   simplification = 10
#' )
#'
#' # Multi-layer with shared options (simple)
#' pm_create(
#'   input = list(
#'     counties = counties_sf,
#'     tracts = tracts_sf
#'   ),
#'   output = "census.pmtiles",
#'   min_zoom = 2,
#'   max_zoom = 12
#' )
#'
#' # Multi-layer with per-layer options (using pm_layer)
#' pm_create(
#'   input = list(
#'     pm_layer(
#'       input = counties_sf,
#'       layer_name = "counties",
#'       min_zoom = 2,
#'       max_zoom = 8
#'     ),
#'     pm_layer(
#'       input = tracts_sf,
#'       layer_name = "tracts",
#'       min_zoom = 8,
#'       max_zoom = 14,
#'       drop_densest_as_needed = TRUE
#'     )
#'   ),
#'   output = "census.pmtiles",
#'   generate_ids = TRUE
#' )
#' }
#'
#' @seealso [pm_layer()] for creating layers with per-layer options
#' @export
pm_create <- function(
  input,
  output,
  layer_name = NULL,

  # Zoom levels
  min_zoom = NULL,
  max_zoom = NULL,
  guess_maxzoom = FALSE,
  smallest_maximum_zoom_guess = NULL,
  base_zoom = NULL,
  extend_zooms_if_still_dropping = FALSE,

  # Tile resolution
  full_detail = NULL,
  low_detail = NULL,
  minimum_detail = NULL,
  extra_detail = NULL,

  # Filtering attributes
  exclude = NULL,
  include = NULL,
  exclude_all = FALSE,

  # Feature dropping
  drop_rate = NULL,
  drop_densest_as_needed = FALSE,
  drop_fraction_as_needed = FALSE,
  drop_smallest_as_needed = FALSE,
  drop_lines = FALSE,
  drop_polygons = FALSE,

  # Coalescing
  coalesce = FALSE,
  coalesce_smallest_as_needed = FALSE,
  coalesce_densest_as_needed = FALSE,
  coalesce_fraction_as_needed = FALSE,

  # Clustering
  cluster_distance = NULL,
  cluster_maxzoom = NULL,

  # Simplification
  simplification = NULL,
  no_line_simplification = FALSE,
  simplify_only_low_zooms = FALSE,
  no_tiny_polygon_reduction = FALSE,
  detect_shared_borders = FALSE,
  no_simplification_of_shared_nodes = FALSE,

  # Ordering
  preserve_input_order = FALSE,
  reorder = FALSE,
  hilbert = FALSE,

  # Tile limits
  maximum_tile_bytes = NULL,
  maximum_tile_features = NULL,
  no_feature_limit = FALSE,
  no_tile_size_limit = FALSE,

  # Other
  generate_ids = TRUE,
  calculate_feature_density = FALSE,
  read_parallel = FALSE,
  attribution = NULL,
  description = NULL,
  buffer = NULL,

  # Advanced
  other_options = NULL,
  force = TRUE,
  keep_geojson = FALSE,
  quiet = FALSE
) {
  # Check for tippecanoe
  tippecanoe_path <- Sys.which("tippecanoe")
  if (tippecanoe_path == "") {
    stop(
      "tippecanoe is not installed or cannot be found.\n",
      "Installation instructions:\n",
      "  - macOS: brew install tippecanoe\n",
      "  - Ubuntu: sudo apt-get install tippecanoe\n",
      "  - From source: https://github.com/felt/tippecanoe\n",
      "\nAfter installation, make sure tippecanoe is in your PATH.",
      call. = FALSE
    )
  }

  # Check for multi-layer input (list of sf/files or list of pm_layer objects)
  if (is.list(input) && !inherits(input, "sf")) {
    # Build global options list from function arguments
    global_opts <- list(
      layer_name = layer_name,
      min_zoom = min_zoom,
      max_zoom = max_zoom,
      guess_maxzoom = guess_maxzoom,
      smallest_maximum_zoom_guess = smallest_maximum_zoom_guess,
      base_zoom = base_zoom,
      extend_zooms_if_still_dropping = extend_zooms_if_still_dropping,
      full_detail = full_detail,
      low_detail = low_detail,
      minimum_detail = minimum_detail,
      extra_detail = extra_detail,
      exclude = exclude,
      include = include,
      exclude_all = exclude_all,
      drop_rate = drop_rate,
      drop_densest_as_needed = drop_densest_as_needed,
      drop_fraction_as_needed = drop_fraction_as_needed,
      drop_smallest_as_needed = drop_smallest_as_needed,
      drop_lines = drop_lines,
      drop_polygons = drop_polygons,
      coalesce = coalesce,
      coalesce_smallest_as_needed = coalesce_smallest_as_needed,
      coalesce_densest_as_needed = coalesce_densest_as_needed,
      coalesce_fraction_as_needed = coalesce_fraction_as_needed,
      cluster_distance = cluster_distance,
      cluster_maxzoom = cluster_maxzoom,
      simplification = simplification,
      no_line_simplification = no_line_simplification,
      simplify_only_low_zooms = simplify_only_low_zooms,
      no_tiny_polygon_reduction = no_tiny_polygon_reduction,
      detect_shared_borders = detect_shared_borders,
      no_simplification_of_shared_nodes = no_simplification_of_shared_nodes,
      preserve_input_order = preserve_input_order,
      reorder = reorder,
      hilbert = hilbert,
      maximum_tile_bytes = maximum_tile_bytes,
      maximum_tile_features = maximum_tile_features,
      no_feature_limit = no_feature_limit,
      no_tile_size_limit = no_tile_size_limit,
      generate_ids = generate_ids,
      calculate_feature_density = calculate_feature_density,
      read_parallel = read_parallel,
      attribution = attribution,
      description = description,
      buffer = buffer,
      other_options = other_options
    )

    return(.process_multi_layer(
      input = input,
      output = output,
      layer_name = layer_name,
      global_opts = global_opts,
      tippecanoe_path = tippecanoe_path,
      force = force,
      quiet = quiet
    ))
  }

  # Handle sf objects vs file paths (single layer)
  temp_file <- NULL
  if (inherits(input, "sf")) {
    # Convert sf to GeoJSON
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required for sf object input", call. = FALSE)
    }

    # Transform to WGS84
    if (!quiet) {
      message("Converting sf object to GeoJSON...")
    }

    # Check if the input is already in WGS84 - no need to transform otherwise
    if (!sf::st_crs(input) == 4326) {
      input <- sf::st_transform(input, 4326)
    }

    # Create temp file
    if (keep_geojson) {
      if (is.null(layer_name)) {
        layer_name <- deparse(substitute(input))
      }
      temp_file <- paste0(layer_name, ".geojson")
    } else {
      temp_file <- tempfile(fileext = ".geojson")
    }

    # Use yyjsonr if available for faster GeoJSON writing
    use_yyjsonr <- requireNamespace("yyjsonr", quietly = TRUE)

    if (use_yyjsonr) {
      if (!quiet) {
        message("  Writing GeoJSON with yyjsonr (fast)...")
      }
      yyjsonr::write_geojson_file(input, temp_file)
    } else {
      if (!quiet) {
        message(
          "  Writing GeoJSON with sf (install 'yyjsonr' for faster writing)..."
        )
      }
      sf::st_write(input, temp_file, quiet = TRUE, delete_dsn = TRUE)
    }

    input_path <- temp_file

    # Generate layer name if not provided
    if (is.null(layer_name)) {
      layer_name <- stringi::stri_rand_strings(1, 6)
    }
  } else if (is.character(input)) {
    input_path <- path.expand(input)
    if (!file.exists(input_path)) {
      stop("Input file does not exist: ", input_path, call. = FALSE)
    }
  } else {
    stop("input must be an sf object or file path", call. = FALSE)
  }

  # Build tippecanoe arguments
  args <- c("-o", output)

  # Layer name
  if (!is.null(layer_name)) {
    args <- c(args, "-l", layer_name)
  }

  # Zoom levels
  if (guess_maxzoom) {
    args <- c(args, "-zg")
  } else if (!is.null(max_zoom)) {
    args <- c(args, "-z", as.character(max_zoom))
  }

  if (!is.null(min_zoom)) {
    args <- c(args, "-Z", as.character(min_zoom))
  }

  if (!is.null(smallest_maximum_zoom_guess)) {
    args <- c(
      args,
      paste0("--smallest-maximum-zoom-guess=", smallest_maximum_zoom_guess)
    )
  }

  if (!is.null(base_zoom)) {
    args <- c(args, "-B", as.character(base_zoom))
  }

  if (extend_zooms_if_still_dropping) {
    args <- c(args, "-ae")
  }

  # Tile resolution
  if (!is.null(full_detail)) {
    args <- c(args, "-d", as.character(full_detail))
  }

  if (!is.null(low_detail)) {
    args <- c(args, "-D", as.character(low_detail))
  }

  if (!is.null(minimum_detail)) {
    args <- c(args, "-m", as.character(minimum_detail))
  }

  if (!is.null(extra_detail)) {
    args <- c(args, paste0("--extra-detail=", extra_detail))
  }

  # Filtering attributes
  if (exclude_all) {
    args <- c(args, "-X")
  } else {
    if (!is.null(exclude)) {
      for (attr in exclude) {
        args <- c(args, "-x", attr)
      }
    }

    if (!is.null(include)) {
      for (attr in include) {
        args <- c(args, "-y", attr)
      }
    }
  }

  # Feature dropping
  if (!is.null(drop_rate)) {
    args <- c(args, paste0("-r", drop_rate))
  }

  if (drop_densest_as_needed) {
    args <- c(args, "-as")
  }

  if (drop_fraction_as_needed) {
    args <- c(args, "-ad")
  }

  if (drop_smallest_as_needed) {
    args <- c(args, "-an")
  }

  if (drop_lines) {
    args <- c(args, "-al")
  }

  if (drop_polygons) {
    args <- c(args, "-ap")
  }

  # Coalescing
  if (coalesce) {
    args <- c(args, "-ac")
  }

  if (coalesce_smallest_as_needed) {
    args <- c(args, "-aN")
  }

  if (coalesce_densest_as_needed) {
    args <- c(args, "-aD")
  }

  if (coalesce_fraction_as_needed) {
    args <- c(args, "-aS")
  }

  # Clustering
  if (!is.null(cluster_distance)) {
    args <- c(args, "-K", as.character(cluster_distance))
  }

  if (!is.null(cluster_maxzoom)) {
    args <- c(args, paste0("-k", cluster_maxzoom))
  }

  # Simplification
  if (!is.null(simplification)) {
    args <- c(args, "-S", as.character(simplification))
  }

  if (no_line_simplification) {
    args <- c(args, "-ps")
  }

  if (simplify_only_low_zooms) {
    args <- c(args, "-pS")
  }

  if (no_tiny_polygon_reduction) {
    args <- c(args, "-pt")
  }

  if (detect_shared_borders) {
    args <- c(args, "-ab")
  }

  if (no_simplification_of_shared_nodes) {
    args <- c(args, "-pn")
  }

  # Ordering
  if (preserve_input_order) {
    args <- c(args, "-pi")
  }

  if (reorder) {
    args <- c(args, "-ao")
  }

  if (hilbert) {
    args <- c(args, "-ah")
  }

  # Tile limits
  if (!is.null(maximum_tile_bytes)) {
    args <- c(args, "-M", as.character(maximum_tile_bytes))
  }

  if (!is.null(maximum_tile_features)) {
    args <- c(args, "-O", as.character(maximum_tile_features))
  }

  if (no_feature_limit) {
    args <- c(args, "-pf")
  }

  if (no_tile_size_limit) {
    args <- c(args, "-pk")
  }

  # Other options
  if (generate_ids) {
    args <- c(args, "-ai")
  }

  if (calculate_feature_density) {
    args <- c(args, "-ag")
  }

  if (read_parallel) {
    args <- c(args, "-P")
  }

  if (!is.null(attribution)) {
    args <- c(args, "-A", attribution)
  }

  if (!is.null(description)) {
    args <- c(args, "-N", description)
  }

  if (!is.null(buffer)) {
    args <- c(args, "-b", as.character(buffer))
  }

  # Force overwrite
  if (force) {
    args <- c(args, "-f")
  }

  # Quiet
  if (quiet) {
    args <- c(args, "-q")
  }

  # Additional options
  if (!is.null(other_options)) {
    args <- c(args, other_options)
  }

  # Input file (must be last)
  args <- c(args, input_path)

  # Execute tippecanoe
  if (!quiet) {
    message("Running tippecanoe...")
  }

  result <- tryCatch(
    {
      processx::run(
        command = as.character(tippecanoe_path),
        args = args,
        echo_cmd = !quiet,
        echo = !quiet,
        error_on_status = TRUE
      )
    },
    error = function(e) {
      # Clean up temp file if it exists
      if (!keep_geojson && !is.null(temp_file) && file.exists(temp_file)) {
        unlink(temp_file)
      }
      stop("tippecanoe execution failed: ", e$message, call. = FALSE)
    }
  )

  # Clean up temp file
  if (!keep_geojson && !is.null(temp_file) && file.exists(temp_file)) {
    unlink(temp_file)
  }

  if (!quiet) {
    message("\u2713 Created tileset: ", output)
  }

  invisible(output)
}


# =============================================================================
# Internal helper functions for multi-layer support
# =============================================================================

#' Build tippecanoe arguments from options
#' @noRd
.build_tippecanoe_args <- function(opts) {
  args <- character()

  # Layer name
  if (!is.null(opts$layer_name)) {
    args <- c(args, "-l", opts$layer_name)
  }

 # Zoom levels
  if (isTRUE(opts$guess_maxzoom)) {
    args <- c(args, "-zg")
  } else if (!is.null(opts$max_zoom)) {
    args <- c(args, "-z", as.character(opts$max_zoom))
  }

  if (!is.null(opts$min_zoom)) {
    args <- c(args, "-Z", as.character(opts$min_zoom))
  }

  if (!is.null(opts$smallest_maximum_zoom_guess)) {
    args <- c(
      args,
      paste0("--smallest-maximum-zoom-guess=", opts$smallest_maximum_zoom_guess)
    )
  }

  if (!is.null(opts$base_zoom)) {
    args <- c(args, "-B", as.character(opts$base_zoom))
  }

  if (isTRUE(opts$extend_zooms_if_still_dropping)) {
    args <- c(args, "-ae")
  }

  # Tile resolution
  if (!is.null(opts$full_detail)) {
    args <- c(args, "-d", as.character(opts$full_detail))
  }

  if (!is.null(opts$low_detail)) {
    args <- c(args, "-D", as.character(opts$low_detail))
  }

  if (!is.null(opts$minimum_detail)) {
    args <- c(args, "-m", as.character(opts$minimum_detail))
  }

  if (!is.null(opts$extra_detail)) {
    args <- c(args, paste0("--extra-detail=", opts$extra_detail))
  }

  # Filtering attributes
  if (isTRUE(opts$exclude_all)) {
    args <- c(args, "-X")
  } else {
    if (!is.null(opts$exclude)) {
      for (attr in opts$exclude) {
        args <- c(args, "-x", attr)
      }
    }

    if (!is.null(opts$include)) {
      for (attr in opts$include) {
        args <- c(args, "-y", attr)
      }
    }
  }

  # Feature dropping
  if (!is.null(opts$drop_rate)) {
    args <- c(args, paste0("-r", opts$drop_rate))
  }

  if (isTRUE(opts$drop_densest_as_needed)) {
    args <- c(args, "-as")
  }

  if (isTRUE(opts$drop_fraction_as_needed)) {
    args <- c(args, "-ad")
  }

  if (isTRUE(opts$drop_smallest_as_needed)) {
    args <- c(args, "-an")
  }

  if (isTRUE(opts$drop_lines)) {
    args <- c(args, "-al")
  }

  if (isTRUE(opts$drop_polygons)) {
    args <- c(args, "-ap")
  }

  # Coalescing
  if (isTRUE(opts$coalesce)) {
    args <- c(args, "-ac")
  }

  if (isTRUE(opts$coalesce_smallest_as_needed)) {
    args <- c(args, "-aN")
  }

  if (isTRUE(opts$coalesce_densest_as_needed)) {
    args <- c(args, "-aD")
  }

  if (isTRUE(opts$coalesce_fraction_as_needed)) {
    args <- c(args, "-aS")
  }

  # Clustering
  if (!is.null(opts$cluster_distance)) {
    args <- c(args, "-K", as.character(opts$cluster_distance))
  }

  if (!is.null(opts$cluster_maxzoom)) {
    args <- c(args, paste0("-k", opts$cluster_maxzoom))
  }

  # Simplification
  if (!is.null(opts$simplification)) {
    args <- c(args, "-S", as.character(opts$simplification))
  }

  if (isTRUE(opts$no_line_simplification)) {
    args <- c(args, "-ps")
  }

  if (isTRUE(opts$simplify_only_low_zooms)) {
    args <- c(args, "-pS")
  }

  if (isTRUE(opts$no_tiny_polygon_reduction)) {
    args <- c(args, "-pt")
  }

  if (isTRUE(opts$detect_shared_borders)) {
    args <- c(args, "-ab")
  }

  if (isTRUE(opts$no_simplification_of_shared_nodes)) {
    args <- c(args, "-pn")
  }

  # Ordering
  if (isTRUE(opts$preserve_input_order)) {
    args <- c(args, "-pi")
  }

  if (isTRUE(opts$reorder)) {
    args <- c(args, "-ao")
  }

  if (isTRUE(opts$hilbert)) {
    args <- c(args, "-ah")
  }

  # Tile limits
  if (!is.null(opts$maximum_tile_bytes)) {
    args <- c(args, "-M", as.character(opts$maximum_tile_bytes))
  }

  if (!is.null(opts$maximum_tile_features)) {
    args <- c(args, "-O", as.character(opts$maximum_tile_features))
  }

  if (isTRUE(opts$no_feature_limit)) {
    args <- c(args, "-pf")
  }

  if (isTRUE(opts$no_tile_size_limit)) {
    args <- c(args, "-pk")
  }

  # Other options
  if (isTRUE(opts$generate_ids)) {
    args <- c(args, "-ai")
  }

  if (isTRUE(opts$calculate_feature_density)) {
    args <- c(args, "-ag")
  }

  if (isTRUE(opts$read_parallel)) {
    args <- c(args, "-P")
  }

  if (!is.null(opts$attribution)) {
    args <- c(args, "-A", opts$attribution)
  }

  if (!is.null(opts$description)) {
    args <- c(args, "-N", opts$description)
  }

  if (!is.null(opts$buffer)) {
    args <- c(args, "-b", as.character(opts$buffer))
  }

  # Additional options
  if (!is.null(opts$other_options)) {
    args <- c(args, opts$other_options)
  }

  args
}


#' Convert sf object to GeoJSON file
#' @noRd
.sf_to_geojson <- function(sf_obj, output_path, quiet = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for sf object input", call. = FALSE)
  }

  # Transform to WGS84 if needed
  if (!sf::st_crs(sf_obj) == 4326) {
    sf_obj <- sf::st_transform(sf_obj, 4326)
  }

  # Use yyjsonr if available for faster GeoJSON writing
  use_yyjsonr <- requireNamespace("yyjsonr", quietly = TRUE)

  if (use_yyjsonr) {
    if (!quiet) {
      message("  Writing GeoJSON with yyjsonr (fast)...")
    }
    yyjsonr::write_geojson_file(sf_obj, output_path)
  } else {
    if (!quiet) {
      message(
        "  Writing GeoJSON with sf (install 'yyjsonr' for faster writing)..."
      )
    }
    sf::st_write(sf_obj, output_path, quiet = TRUE, delete_dsn = TRUE)
  }

  output_path
}


#' Run tile-join to merge multiple tilesets
#' @noRd
.run_tile_join <- function(input_files, output, force = TRUE,
                           no_tile_size_limit = FALSE,
                           exclude_all = FALSE,
                           exclude = NULL,
                           include = NULL,
                           quiet = FALSE) {
  tile_join_path <- Sys.which("tile-join")
  if (tile_join_path == "") {
    stop(
      "tile-join is not installed or cannot be found.\n",
      "tile-join is installed as part of tippecanoe.\n",
      "See: https://github.com/felt/tippecanoe",
      call. = FALSE
    )
  }

  args <- c("-o", output)
  if (force) {
    args <- c(args, "-f")
  }
  if (no_tile_size_limit) {
    args <- c(args, "-pk")
  }
  if (exclude_all) {
    args <- c(args, "-X")
  } else {
    if (!is.null(exclude)) {
      for (attr in exclude) {
        args <- c(args, "-x", attr)
      }
    }
    if (!is.null(include)) {
      for (attr in include) {
        args <- c(args, "-y", attr)
      }
    }
  }
  args <- c(args, input_files)

  if (!quiet) {
    message("Merging layers with tile-join...")
  }

  result <- processx::run(
    command = as.character(tile_join_path),
    args = args,
    echo_cmd = !quiet,
    echo = !quiet,
    error_on_status = TRUE
  )

  result
}


#' Process multi-layer input
#' @noRd
.process_multi_layer <- function(
    input,
    output,
    layer_name,
    global_opts,
    tippecanoe_path,
    force,
    quiet) {
  # Determine if input is list of pm_layer objects or list of sf/files
  is_pm_layer_list <- all(vapply(input, inherits, logical(1), "pm_layer"))
  is_simple_list <- all(vapply(input, function(x) {
    inherits(x, "sf") || is.character(x)
  }, logical(1)))

  if (!is_pm_layer_list && !is_simple_list) {
    stop(
      "Multi-layer input must be either:\n",
      "  - A list of sf objects or file paths (shared options)\
",
      "  - A list of pm_layer() objects (per-layer options)",
      call. = FALSE
    )
  }

  n_layers <- length(input)

  # Resolve layer names
  if (is_pm_layer_list) {
    layer_names <- vapply(input, function(x) x$layer_name, character(1))
  } else {
    # Simple list: use provided layer_name vector, list names, or generate
    if (!is.null(layer_name)) {
      if (length(layer_name) != n_layers) {
        stop(
          "layer_name must have the same length as input list (",
          n_layers, " layers)",
          call. = FALSE
        )
      }
      layer_names <- layer_name
    } else if (!is.null(names(input))) {
      layer_names <- names(input)
      # Fill in missing names
      missing_names <- layer_names == "" | is.na(layer_names)
      if (any(missing_names)) {
        layer_names[missing_names] <- stringi::stri_rand_strings(
          sum(missing_names), 6
        )
      }
    } else {
      layer_names <- stringi::stri_rand_strings(n_layers, 6)
    }
  }

  # Create temp directory for intermediate files
  temp_dir <- tempfile(pattern = "pmtiles_multi_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  temp_mbtiles <- character(n_layers)

  # Process each layer
  for (i in seq_len(n_layers)) {
    if (!quiet) {
      message(sprintf("Processing layer %d/%d: %s", i, n_layers, layer_names[i]))
    }

    if (is_pm_layer_list) {
      # pm_layer object: merge layer opts with global opts (layer takes priority)
      layer <- input[[i]]
      layer_input <- layer$input
      layer_opts <- layer[!names(layer) %in% c("input")]

      # Merge: layer options override global options
      opts <- global_opts
      for (nm in names(layer_opts)) {
        if (!is.null(layer_opts[[nm]])) {
          opts[[nm]] <- layer_opts[[nm]]
        }
      }
    } else {
      # Simple list: use global options with layer name
      layer_input <- input[[i]]
      opts <- global_opts
      opts$layer_name <- layer_names[i]
    }

    # Prepare input file
    if (inherits(layer_input, "sf")) {
      if (!quiet) {
        message("  Converting sf object to GeoJSON...")
      }
      input_path <- file.path(temp_dir, paste0(layer_names[i], ".geojson"))
      .sf_to_geojson(layer_input, input_path, quiet = quiet)
    } else if (is.character(layer_input)) {
      input_path <- path.expand(layer_input)
      if (!file.exists(input_path)) {
        stop("Input file does not exist: ", input_path, call. = FALSE)
      }
    } else {
      stop("Layer input must be an sf object or file path", call. = FALSE)
    }

    # Build tippecanoe args for this layer
    temp_mbtiles[i] <- file.path(temp_dir, paste0(layer_names[i], ".mbtiles"))
    args <- c("-o", temp_mbtiles[i], "-f")
    args <- c(args, .build_tippecanoe_args(opts))
    args <- c(args, input_path)

    if (!quiet) {
      message("  Running tippecanoe...")
    }

    tryCatch(
      {
        processx::run(
          command = as.character(tippecanoe_path),
          args = args,
          echo_cmd = !quiet,
          echo = !quiet,
          error_on_status = TRUE
        )
      },
      error = function(e) {
        stop(
          "tippecanoe execution failed for layer '", layer_names[i], "': ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  # Merge all layers with tile-join directly to final output
  # tile-join supports both .mbtiles and .pmtiles output
  .run_tile_join(
    temp_mbtiles,
    output,
    force = force,
    no_tile_size_limit = isTRUE(global_opts$no_tile_size_limit),
    exclude_all = isTRUE(global_opts$exclude_all),
    exclude = global_opts$exclude,
    include = global_opts$include,
    quiet = quiet
  )

  if (!quiet) {
    message("\u2713 Created tileset: ", output)
  }

  invisible(output)
}
