#' Upload PMTiles archive to cloud storage
#'
#' @description
#' Upload a local PMTiles archive to cloud storage (S3, Azure, Google Cloud).
#' Requires appropriate authentication via environment variables.
#'
#' @param input Path to local PMTiles file to upload.
#' @param remote Name for the PMTiles file in cloud storage.
#' @param bucket Bucket specification (e.g., "s3://bucket-name"). See Details.
#' @param max_concurrency Maximum number of parallel upload threads. Default is 2.
#' @param verbose Logical. If `TRUE`, prints progress information. Default is `TRUE`.
#'
#' @details
#' # Authentication
#'
#' PMTiles uses standard cloud provider authentication methods:
#'
#' **AWS S3:**
#' - Set `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` environment variables
#' - Requires write permissions to the bucket
#'
#' **S3-compatible (R2, MinIO, etc.):**
#' - Same credentials as S3
#' - Specify endpoint in bucket parameter:
#'   `bucket = "s3://bucket?endpoint=https://account.r2.cloudflarestorage.com&region=auto"`
#'
#' **Azure Blob:**
#' - Uses Azure SDK default authentication
#' - `bucket = "azblob://container?storage_account=ACCOUNT"`
#'
#' **Google Cloud Storage:**
#' - Uses Application Default Credentials
#' - `bucket = "gs://bucket-name"`
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @examples
#' \dontrun{
#' # Set credentials
#' Sys.setenv(
#'   AWS_ACCESS_KEY_ID = "your-key-id",
#'   AWS_SECRET_ACCESS_KEY = "your-secret"
#' )
#'
#' # Upload to S3
#' pm_upload(
#'   "local.pmtiles",
#'   "remote.pmtiles",
#'   bucket = "s3://my-bucket"
#' )
#'
#' # Upload to Cloudflare R2
#' pm_upload(
#'   "local.pmtiles",
#'   "remote.pmtiles",
#'   bucket = "s3://my-bucket?endpoint=https://account.r2.cloudflarestorage.com&region=auto"
#' )
#' }
#'
#' @export
pm_upload <- function(input,
                      remote,
                      bucket,
                      max_concurrency = 2,
                      verbose = TRUE) {

  # Validate inputs
  if (!file.exists(input)) {
    stop("Input file not found: ", input, call. = FALSE)
  }

  if (missing(bucket)) {
    stop("bucket parameter is required", call. = FALSE)
  }

  # Build command arguments
  args <- c(
    "upload",
    input,
    remote,
    paste0("--bucket=", bucket),
    paste0("--max-concurrency=", as.integer(max_concurrency))
  )

  # Execute command
  if (verbose) {
    message("Uploading ", input, " to ", bucket, "/", remote, "...")
  }

  result <- pmtiles_exec(args)

  if (verbose) {
    cat(result$stdout)
  }

  if (result$status == 0) {
    message("\u2713 Successfully uploaded to: ", bucket, "/", remote)
  }

  return(invisible(TRUE))
}
