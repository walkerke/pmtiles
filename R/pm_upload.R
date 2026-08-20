#' Upload PMTiles archive to cloud storage
#'
#' @description
#' Upload a local PMTiles archive to cloud storage (S3, Cloudflare R2, Google
#' Cloud Storage, Azure, or any S3-compatible service). The easiest way to
#' specify the destination is with a bucket helper -- see [r2_bucket()] and
#' friends -- which builds the bucket URL and can carry credentials for you.
#'
#' @param input Path to local PMTiles file to upload.
#' @param remote Name for the PMTiles file in cloud storage.
#' @param bucket Destination bucket: either a helper object from
#'   [r2_bucket()], [s3_bucket()], [gcs_bucket()], [azure_bucket()], or
#'   [s3_compatible_bucket()], or a bucket URL string
#'   (e.g., `"s3://bucket-name"`). See Details.
#' @param max_concurrency Maximum number of parallel upload threads. Default is 2.
#' @param verbose Logical. If `TRUE`, prints progress information. Default is `TRUE`.
#'
#' @details
#' # Specifying the bucket
#'
#' The bucket helpers handle each provider's URL format and authentication
#' details (see [bucket_helpers] for provider-by-provider setup guides):
#'
#' ```r
#' # Cloudflare R2
#' bucket = r2_bucket("my-tiles", account_id = "your-account-id")
#'
#' # Amazon S3
#' bucket = s3_bucket("my-tiles", region = "us-east-1")
#'
#' # Google Cloud Storage
#' bucket = gcs_bucket("my-tiles")
#'
#' # Azure Blob Storage
#' bucket = azure_bucket("my-container", storage_account = "myaccount")
#' ```
#'
#' A raw URL string is also accepted, e.g.
#' `"s3://bucket?endpoint=https://account.r2.cloudflarestorage.com&region=auto"`.
#'
#' # Authentication
#'
#' Credentials can be supplied directly to the bucket helpers (passed only to
#' the pmtiles subprocess), or via the provider's standard environment
#' variables:
#'
#' - **S3 / R2 / S3-compatible**: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`
#' - **Google Cloud Storage**: Application Default Credentials or
#'   `GOOGLE_APPLICATION_CREDENTIALS`
#' - **Azure**: `AZURE_STORAGE_ACCOUNT` plus `AZURE_STORAGE_KEY` or
#'   `AZURE_STORAGE_SAS_TOKEN`
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @seealso [bucket_helpers]
#'
#' @examples
#' \dontrun{
#' # Upload to Cloudflare R2, with credentials stored in ~/.Renviron
#' pm_upload(
#'   "local.pmtiles",
#'   "remote.pmtiles",
#'   bucket = r2_bucket(
#'     "my-tiles",
#'     account_id = "your-cloudflare-account-id",
#'     access_key = Sys.getenv("R2_ACCESS_KEY_ID"),
#'     secret_key = Sys.getenv("R2_SECRET_ACCESS_KEY")
#'   )
#' )
#'
#' # Upload to S3 using the standard AWS credential chain
#' pm_upload(
#'   "local.pmtiles",
#'   "remote.pmtiles",
#'   bucket = s3_bucket("my-bucket", region = "us-east-1")
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

  bucket <- resolve_bucket(bucket)

  # Build command arguments
  args <- c(
    "upload",
    input,
    remote,
    paste0("--bucket=", bucket$url),
    paste0("--max-concurrency=", as.integer(max_concurrency))
  )

  # Execute command
  if (verbose) {
    message("Uploading ", input, " to ", bucket$url, "/", remote, "...")
  }

  result <- pmtiles_exec(args, env = bucket$env)

  if (verbose) {
    cat(result$stdout)
  }

  if (result$status == 0) {
    message("\u2713 Successfully uploaded to: ", bucket$url, "/", remote)
  }

  return(invisible(TRUE))
}
