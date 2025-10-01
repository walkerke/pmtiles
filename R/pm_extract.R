#' Extract a subset from a PMTiles archive
#'
#' @description
#' Create a smaller PMTiles archive from a larger one by extracting a subset
#' of zoom levels or a geographic region. The source archive can be local or remote.
#'
#' @param input Path to input PMTiles archive (local or remote URL).
#' @param output Path for the output PMTiles archive.
#' @param bbox Numeric vector of bounding box coordinates in the form
#'   `c(min_lon, min_lat, max_lon, max_lat)`. Mutually exclusive with `region`.
#' @param region Path to a GeoJSON file containing a Polygon, MultiPolygon,
#'   Feature, or FeatureCollection defining the area of interest.
#'   Mutually exclusive with `bbox`.
#' @param minzoom Minimum zoom level to extract (inclusive). Default is 0.
#' @param maxzoom Maximum zoom level to extract (inclusive). If not specified,
#'   extracts all zoom levels from the source.
#' @param bucket Optional remote bucket specification if `input` is remote.
#' @param download_threads Number of parallel download threads for remote archives.
#'   Default is 4.
#' @param overfetch Ratio of extra data to download to minimize number of requests.
#'   For example, 0.05 means 5 percent overfetch. Default is 0.05.
#' @param dry_run Logical. If `TRUE`, calculates tiles to extract without
#'   actually downloading them. Default is `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress information. Default is `TRUE`.
#'
#' @details
#' Extracting a full sub-pyramid from zoom 0 to `maxzoom` is an efficient
#' operation. However, using a `minzoom` > 0 may require many more requests
#' and should only be used when necessary.
#'
#' @return Invisibly returns the path to the output archive.
#'
#' @examples
#' \dontrun{
#' # Extract zoom levels 0-10
#' pm_extract("large.pmtiles", "subset.pmtiles", maxzoom = 10)
#'
#' # Extract by bounding box
#' pm_extract(
#'   "large.pmtiles",
#'   "bbox_subset.pmtiles",
#'   bbox = c(-122.5, 37.7, -122.3, 37.9)
#' )
#'
#' # Extract by GeoJSON region
#' pm_extract(
#'   "large.pmtiles",
#'   "region_subset.pmtiles",
#'   region = "boundary.geojson",
#'   maxzoom = 12
#' )
#'
#' # Extract from remote archive
#' pm_extract(
#'   "large.pmtiles",
#'   "local_copy.pmtiles",
#'   bucket = "s3://my-bucket",
#'   maxzoom = 10,
#'   download_threads = 8
#' )
#' }
#'
#'
#' @export
pm_extract <- function(input,
                       output,
                       bbox = NULL,
                       region = NULL,
                       minzoom = NULL,
                       maxzoom = NULL,
                       bucket = NULL,
                       download_threads = 4,
                       overfetch = 0.05,
                       dry_run = FALSE,
                       verbose = TRUE) {

  # Validate inputs
  if (!is.null(bbox) && !is.null(region)) {
    stop("Cannot specify both bbox and region", call. = FALSE)
  }

  if (!is.null(bbox)) {
    if (length(bbox) != 4) {
      stop("bbox must be a numeric vector of length 4: c(min_lon, min_lat, max_lon, max_lat)",
           call. = FALSE)
    }
  }

  if (!is.null(region) && !file.exists(region)) {
    stop("Region file not found: ", region, call. = FALSE)
  }

  # Build command arguments
  args <- c("extract", input, output)

  if (!is.null(bucket)) {
    args <- c(args, paste0("--bucket=", bucket))
  }

  if (!is.null(bbox)) {
    args <- c(args, paste0("--bbox=", paste(bbox, collapse = ",")))
  }

  if (!is.null(region)) {
    args <- c(args, paste0("--region=", region))
  }

  if (!is.null(minzoom)) {
    args <- c(args, paste0("--minzoom=", as.integer(minzoom)))
  }

  if (!is.null(maxzoom)) {
    args <- c(args, paste0("--maxzoom=", as.integer(maxzoom)))
  }

  args <- c(args, paste0("--download-threads=", as.integer(download_threads)))
  args <- c(args, paste0("--overfetch=", overfetch))

  if (dry_run) {
    args <- c(args, "--dry-run")
  }

  # Execute command
  if (verbose) {
    message("Extracting from ", input, " to ", output, "...")
  }

  result <- pmtiles_exec(args)

  if (verbose) {
    cat(result$stdout)
  }

  if (result$status == 0 && !dry_run) {
    message("\u2713 Successfully created: ", output)
  }

  return(invisible(output))
}
