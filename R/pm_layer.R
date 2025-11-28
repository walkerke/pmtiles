#' Define a layer for multi-layer PMTiles creation
#'
#' @description
#' Creates a layer specification for use with `pm_create()` when building
#' multi-layer PMTiles with per-layer tippecanoe options. Each layer can have
#' its own zoom levels, drop rates, simplification settings, and other options.
#'
#' @param input An sf object, or path to a GeoJSON, FlatGeobuf, or CSV file
#' @param layer_name Name for this layer in the tileset (required)
#'
#' @section Zoom Levels:
#' @param min_zoom Minimum zoom level (tippecanoe -Z)
#' @param max_zoom Maximum zoom level (tippecanoe -z)
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
#'   (tippecanoe -r). Use "g" for auto-guess.
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
#'   (tippecanoe -S)
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
#' @param maximum_tile_bytes Maximum compressed tile size in bytes
#'   (tippecanoe -M)
#' @param maximum_tile_features Maximum features per tile (tippecanoe -O)
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
#' @param buffer Buffer size in screen pixels (tippecanoe -b)
#' @param other_options Character vector of additional tippecanoe options not
#'   covered by other parameters. Example: c("--coalesce")
#'
#' @return A list with class `"pm_layer"` containing the input and all
#'   specified options.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create a multi-layer PMTiles with per-layer options
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
#' @seealso [pm_create()]
#' @export
pm_layer <- function(
    input,
    layer_name,

    # Zoom levels
    min_zoom = NULL,
    max_zoom = NULL,
    guess_maxzoom = NULL,
    smallest_maximum_zoom_guess = NULL,
    base_zoom = NULL,
    extend_zooms_if_still_dropping = NULL,

    # Tile resolution
    full_detail = NULL,
    low_detail = NULL,
    minimum_detail = NULL,
    extra_detail = NULL,

    # Filtering attributes
    exclude = NULL,
    include = NULL,
    exclude_all = NULL,

    # Feature dropping
    drop_rate = NULL,
    drop_densest_as_needed = NULL,
    drop_fraction_as_needed = NULL,
    drop_smallest_as_needed = NULL,
    drop_lines = NULL,
    drop_polygons = NULL,

    # Coalescing
    coalesce = NULL,
    coalesce_smallest_as_needed = NULL,
    coalesce_densest_as_needed = NULL,
    coalesce_fraction_as_needed = NULL,

    # Clustering
    cluster_distance = NULL,
    cluster_maxzoom = NULL,

    # Simplification
    simplification = NULL,
    no_line_simplification = NULL,
    simplify_only_low_zooms = NULL,
    no_tiny_polygon_reduction = NULL,
    detect_shared_borders = NULL,
    no_simplification_of_shared_nodes = NULL,

    # Ordering
    preserve_input_order = NULL,
    reorder = NULL,
    hilbert = NULL,

    # Tile limits
    maximum_tile_bytes = NULL,
    maximum_tile_features = NULL,
    no_feature_limit = NULL,
    no_tile_size_limit = NULL,

    # Other
    generate_ids = NULL,
    calculate_feature_density = NULL,
    read_parallel = NULL,
    buffer = NULL,
    other_options = NULL) {
  # Validate required arguments

if (missing(input)) {
    stop("'input' is required", call. = FALSE)
  }

  if (missing(layer_name) || is.null(layer_name)) {
    stop("'layer_name' is required for pm_layer()", call. = FALSE)
  }

  # Build list of all options (only non-NULL values)
  opts <- list(
    input = input,
    layer_name = layer_name,

    # Zoom levels
    min_zoom = min_zoom,
    max_zoom = max_zoom,
    guess_maxzoom = guess_maxzoom,
    smallest_maximum_zoom_guess = smallest_maximum_zoom_guess,
    base_zoom = base_zoom,
    extend_zooms_if_still_dropping = extend_zooms_if_still_dropping,

    # Tile resolution
    full_detail = full_detail,
    low_detail = low_detail,
    minimum_detail = minimum_detail,
    extra_detail = extra_detail,

    # Filtering attributes
    exclude = exclude,
    include = include,
    exclude_all = exclude_all,

    # Feature dropping
    drop_rate = drop_rate,
    drop_densest_as_needed = drop_densest_as_needed,
    drop_fraction_as_needed = drop_fraction_as_needed,
    drop_smallest_as_needed = drop_smallest_as_needed,
    drop_lines = drop_lines,
    drop_polygons = drop_polygons,

    # Coalescing
    coalesce = coalesce,
    coalesce_smallest_as_needed = coalesce_smallest_as_needed,
    coalesce_densest_as_needed = coalesce_densest_as_needed,
    coalesce_fraction_as_needed = coalesce_fraction_as_needed,

    # Clustering
    cluster_distance = cluster_distance,
    cluster_maxzoom = cluster_maxzoom,

    # Simplification
    simplification = simplification,
    no_line_simplification = no_line_simplification,
    simplify_only_low_zooms = simplify_only_low_zooms,
    no_tiny_polygon_reduction = no_tiny_polygon_reduction,
    detect_shared_borders = detect_shared_borders,
    no_simplification_of_shared_nodes = no_simplification_of_shared_nodes,

    # Ordering
    preserve_input_order = preserve_input_order,
    reorder = reorder,
    hilbert = hilbert,

    # Tile limits
    maximum_tile_bytes = maximum_tile_bytes,
    maximum_tile_features = maximum_tile_features,
    no_feature_limit = no_feature_limit,
    no_tile_size_limit = no_tile_size_limit,

    # Other
    generate_ids = generate_ids,
    calculate_feature_density = calculate_feature_density,
    read_parallel = read_parallel,
    buffer = buffer,
    other_options = other_options
  )

  structure(opts, class = "pm_layer")
}

#' @export
print.pm_layer <- function(x, ...) {
  cat("<pm_layer>\n")
  cat("  Layer name:", x$layer_name, "\n")

  # Show input type

  if (inherits(x$input, "sf")) {
    cat("  Input: sf object with", nrow(x$input), "features\n")
  } else {
    cat("  Input:", x$input, "\n")
  }

  # Show key options if set
  if (!is.null(x$min_zoom) || !is.null(x$max_zoom)) {
    cat("  Zoom:", x$min_zoom %||% "default", "-", x$max_zoom %||% "default", "\n")
  }

  # Count non-NULL options (excluding input and layer_name)
  opts <- x[!names(x) %in% c("input", "layer_name")]
  n_set <- sum(!vapply(opts, is.null, logical(1)))
  if (n_set > 0) {
    cat("  Options set:", n_set, "\n")
  }

  invisible(x)
}

# Internal null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
