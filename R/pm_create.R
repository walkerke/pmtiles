#' Create PMTiles or MBTiles from GeoJSON with tippecanoe
#'
#' Generate vector tiles from GeoJSON, FlatGeobuf, or CSV input using
#' tippecanoe. This function requires tippecanoe to be installed on your system.
#' See \url{https://github.com/felt/tippecanoe} for installation instructions.
#'
#' @param input An sf object, or path to a GeoJSON, FlatGeobuf, or CSV file
#' @param output Path to output file (.pmtiles or .mbtiles)
#' @param layer_name Name for the layer in the tileset. If NULL, derived from
#'   input filename or a random string for sf objects (tippecanoe -l)
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
#' }
#'
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

  # Handle sf objects vs file paths
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
    input <- sf::st_transform(input, 4326)

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
        message("  Writing GeoJSON with sf (install 'yyjsonr' for faster writing)...")
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
