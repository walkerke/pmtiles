#' Show PMTiles archive information
#'
#' @description
#' Inspect a local or remote PMTiles archive and display header information
#' and metadata.
#'
#' @param input Path to a local PMTiles file or URL to a remote archive.
#'   Remote archives can be HTTP URLs or cloud storage paths.
#' @param bucket Optional remote bucket specification for cloud storage
#'   (e.g., "s3://bucket-name"). See Details for cloud storage usage.
#' @param metadata Logical. If `TRUE`, return only the JSON metadata.
#'   Default is `FALSE`.
#' @param header_json Logical. If `TRUE`, return only the header as JSON.
#'   Default is `FALSE`.
#' @param tilejson Logical. If `TRUE`, return TileJSON specification.
#'   Default is `FALSE`.
#' @param public_url Character. Public base URL for TileJSON
#'   (e.g., "https://example.com/tiles"). Only used when `tilejson = TRUE`.
#'
#' @details
#' # Cloud Storage
#'
#' PMTiles supports reading from cloud storage buckets:
#' - **S3**: `bucket = "s3://BUCKET_NAME"`
#' - **S3-compatible** (R2, etc.): `bucket = "s3://BUCKET?endpoint=https://example.com&region=auto"`
#' - **Azure**: `bucket = "azblob://CONTAINER?storage_account=ACCOUNT"`
#' - **Google Cloud**: `bucket = "gs://BUCKET_NAME"`
#'
#' Authentication uses standard cloud provider environment variables
#' (e.g., `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY` for S3).
#'
#' @return
#' - If `metadata = TRUE` or `header_json = TRUE` or `tilejson = TRUE`:
#'   Returns a parsed list from the JSON output
#' - Otherwise: Returns invisible `NULL` and prints archive information
#'
#' @examples
#' \dontrun{
#' # Show local archive info
#' pm_show("path/to/archive.pmtiles")
#'
#' # Get metadata as list
#' metadata <- pm_show("archive.pmtiles", metadata = TRUE)
#'
#' # Show remote archive
#' pm_show(
#'   "archive.pmtiles",
#'   bucket = "s3://my-bucket?endpoint=https://account.r2.cloudflarestorage.com&region=auto"
#' )
#'
#' # Get TileJSON
#' tilejson <- pm_show(
#'   "archive.pmtiles",
#'   tilejson = TRUE,
#'   public_url = "https://example.com/tiles"
#' )
#' }
#'
#' @export
pm_show <- function(input,
                    bucket = NULL,
                    metadata = FALSE,
                    header_json = FALSE,
                    tilejson = FALSE,
                    public_url = NULL) {

  # Expand path if it's a local file
  if (!grepl("^https?://", input) && is.null(bucket)) {
    input <- path.expand(input)
  }

  # Build command arguments
  args <- c("show", input)

  if (!is.null(bucket)) {
    args <- c(args, paste0("--bucket=", bucket))
  }

  if (metadata) {
    args <- c(args, "--metadata")
  }

  if (header_json) {
    args <- c(args, "--header-json")
  }

  if (tilejson) {
    args <- c(args, "--tilejson")
    if (!is.null(public_url)) {
      args <- c(args, paste0("--public-url=", public_url))
    }
  }

  # Execute command
  result <- pmtiles_exec(args)

  # Parse and return based on output type
  if (metadata || header_json || tilejson) {
    parsed <- parse_pmtiles_json(result$stdout)
    return(parsed)
  } else {
    # Print human-readable output
    cat(result$stdout)
    return(invisible(NULL))
  }
}
