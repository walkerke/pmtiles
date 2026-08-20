#' Cloud storage bucket helpers
#'
#' @description
#' These helpers build the bucket specification used by [pm_upload()],
#' [pm_show()], [pm_extract()], [pm_tile()], and [pm_serve_zxy()], so you
#' never have to hand-assemble a `"s3://bucket?endpoint=...&region=..."` URL.
#' Each helper knows the URL format and required parameters for its provider:
#'
#' - `r2_bucket()`: Cloudflare R2 (the recommended PMTiles host; no egress fees)
#' - `s3_bucket()`: Amazon S3
#' - `gcs_bucket()`: Google Cloud Storage
#' - `azure_bucket()`: Azure Blob Storage
#' - `s3_compatible_bucket()`: any other S3-compatible service (MinIO,
#'   DigitalOcean Spaces, Backblaze B2, Source Cooperative, ...)
#'
#' Credentials can optionally be supplied to each helper. When provided, they
#' are passed **only to the pmtiles subprocess** for that one command -- they
#' are never written into your R session's environment variables. When
#' omitted, the standard environment variables for the provider are used
#' (e.g. `AWS_ACCESS_KEY_ID`/`AWS_SECRET_ACCESS_KEY` for S3 and R2).
#'
#' @param bucket Name of the bucket (as shown in your cloud provider's
#'   console), e.g. `"my-tiles"`.
#' @param account_id For `r2_bucket()`: your Cloudflare account ID, a
#'   32-character hex string. Find it in the right-hand **Account Details**
#'   panel of the R2 dashboard (the same panel with the *Manage R2 API
#'   Tokens* link). Pasting the full "S3 API" URL from a bucket's Settings
#'   page also works -- the account ID is extracted automatically.
#' @param jurisdiction For `r2_bucket()`: optional R2 jurisdiction for
#'   buckets created in a specific jurisdiction, e.g. `"eu"` or `"fedramp"`.
#'   Leave as `NULL` (the default) for standard buckets.
#' @param region For `s3_bucket()`: the AWS region of the bucket, e.g.
#'   `"us-east-1"`. If `NULL`, the `AWS_REGION` environment variable or your
#'   AWS config default is used. For `s3_compatible_bucket()`: the region
#'   string your provider expects. If `NULL` (the default), the region is
#'   inferred from recognized endpoints (DigitalOcean Spaces, Backblaze B2,
#'   Source Cooperative); for other endpoints it must be supplied -- check
#'   your provider's documentation.
#' @param endpoint For `s3_compatible_bucket()`: the service's S3 API
#'   endpoint URL, e.g. `"https://nyc3.digitaloceanspaces.com"`. This is the
#'   *base* endpoint -- do not include the bucket name in it.
#' @param path_style Logical. Use path-style addressing
#'   (`endpoint/bucket/key` instead of `bucket.endpoint/key`). Required by
#'   MinIO and SeaweedFS, among others. Default is `FALSE`.
#' @param access_key,secret_key Optional access key ID and secret access key
#'   for S3-style providers (for `azure_bucket()`, `access_key` is the
#'   storage account access key). Supply both or neither. See Details for
#'   where each provider issues these.
#' @param session_token For `s3_bucket()` and `s3_compatible_bucket()`:
#'   optional session token for temporary credentials (e.g. AWS STS, or
#'   Source Cooperative's issued credentials). Requires `access_key` and
#'   `secret_key`.
#' @param credentials_file For `gcs_bucket()`: optional path to a service
#'   account JSON key file. If `NULL`, Google Application Default
#'   Credentials are used (e.g. from `gcloud auth application-default login`
#'   or the `GOOGLE_APPLICATION_CREDENTIALS` environment variable).
#' @param container For `azure_bucket()`: name of the blob container.
#' @param storage_account For `azure_bucket()`: name of the Azure storage
#'   account the container belongs to.
#' @param sas_token For `azure_bucket()`: optional shared access signature
#'   (SAS) token, as an alternative to `access_key`.
#'
#' @details
#' # Cloudflare R2
#'
#' You need three things, all from the Cloudflare dashboard:
#'
#' 1. **Account ID**: R2 overview page, right-hand *Account Details* panel.
#' 2. **Access key + secret key**: *Manage R2 API Tokens* > *Create API
#'    Token* with "Object Read & Write" permission. The token-creation
#'    screen shows the Access Key ID and Secret Access Key once -- store
#'    them (e.g. in `~/.Renviron`).
#' 3. **Bucket name**: the name you gave the bucket when creating it.
#'
#' R2's S3 API requires `region=auto`; the helper sets this for you.
#'
#' # Amazon S3
#'
#' Credentials follow the standard AWS chain: `AWS_ACCESS_KEY_ID` /
#' `AWS_SECRET_ACCESS_KEY` environment variables, shared config files
#' (`~/.aws/credentials`), or an attached IAM role. Supply `access_key` /
#' `secret_key` only if you want to override that chain for this command.
#'
#' # Google Cloud Storage
#'
#' Authentication uses Application Default Credentials. Either run
#' `gcloud auth application-default login` once, or create a service account
#' key (IAM & Admin > Service Accounts > Keys) and pass its JSON file path
#' as `credentials_file`.
#'
#' # Azure Blob Storage
#'
#' Find the storage account's access keys under *Storage account* >
#' *Security + networking* > *Access keys* in the Azure portal. Pass one as
#' `access_key`, or use a SAS token via `sas_token`. If neither is given,
#' the `AZURE_STORAGE_KEY` / `AZURE_STORAGE_SAS_TOKEN` environment variables
#' are used.
#'
#' # Other S3-compatible services
#'
#' Use `s3_compatible_bucket()` with the provider's endpoint:
#'
#' | Provider | Endpoint | Notes |
#' |---|---|---|
#' | MinIO | `https://minio.example.com` | needs `path_style = TRUE` and `region` |
#' | DigitalOcean Spaces | `https://{region}.digitaloceanspaces.com` | region inferred |
#' | Backblaze B2 | `https://s3.{region}.backblazeb2.com` | region inferred |
#' | Source Cooperative | `https://data.source.coop` | bucket is your org name; pass `session_token` |
#'
#' @return An object of class `pmtiles_bucket`: the bucket URL plus any
#'   credentials to pass to the pmtiles subprocess. Pass it as the `bucket`
#'   argument of [pm_upload()] and friends, anywhere a bucket string is
#'   accepted.
#'
#' @examples
#' \dontrun{
#' # Cloudflare R2: upload with credentials from ~/.Renviron
#' pm_upload(
#'   "tiles.pmtiles",
#'   "tiles.pmtiles",
#'   bucket = r2_bucket(
#'     "my-tiles",
#'     account_id = "0123456789abcdef0123456789abcdef",
#'     access_key = Sys.getenv("R2_ACCESS_KEY_ID"),
#'     secret_key = Sys.getenv("R2_SECRET_ACCESS_KEY")
#'   )
#' )
#'
#' # Amazon S3, using the standard AWS credential chain
#' pm_upload(
#'   "tiles.pmtiles",
#'   "tiles.pmtiles",
#'   bucket = s3_bucket("my-tiles", region = "us-east-1")
#' )
#'
#' # Google Cloud Storage with a service account key
#' pm_upload(
#'   "tiles.pmtiles",
#'   "tiles.pmtiles",
#'   bucket = gcs_bucket("my-tiles", credentials_file = "~/keys/gcs.json")
#' )
#'
#' # Azure Blob Storage
#' pm_upload(
#'   "tiles.pmtiles",
#'   "tiles.pmtiles",
#'   bucket = azure_bucket(
#'     "tiles-container",
#'     storage_account = "mystorageaccount",
#'     access_key = Sys.getenv("AZURE_STORAGE_KEY")
#'   )
#' )
#'
#' # DigitalOcean Spaces (any S3-compatible service)
#' pm_upload(
#'   "tiles.pmtiles",
#'   "tiles.pmtiles",
#'   bucket = s3_compatible_bucket(
#'     "my-tiles",
#'     endpoint = "https://nyc3.digitaloceanspaces.com",
#'     access_key = Sys.getenv("DO_SPACES_KEY"),
#'     secret_key = Sys.getenv("DO_SPACES_SECRET")
#'   )
#' )
#'
#' # The helpers work everywhere a bucket is accepted
#' bucket <- r2_bucket("my-tiles", account_id = "0123456789abcdef0123456789abcdef")
#' pm_show("tiles.pmtiles", bucket = bucket)
#' pm_extract("tiles.pmtiles", "subset.pmtiles", bucket = bucket, maxzoom = 10)
#' }
#'
#' @name bucket_helpers
NULL

#' @rdname bucket_helpers
#' @export
r2_bucket <- function(bucket,
                      account_id,
                      jurisdiction = NULL,
                      access_key = NULL,
                      secret_key = NULL) {

  check_bucket_name(bucket)

  parsed <- parse_r2_account_id(account_id)
  if (is.null(jurisdiction)) {
    jurisdiction <- parsed$jurisdiction
  }
  if (!is.null(jurisdiction)) {
    jurisdiction <- tolower(trimws(jurisdiction))
    if (!jurisdiction %in% c("eu", "fedramp")) {
      stop(
        "jurisdiction must be \"eu\" or \"fedramp\" (or NULL for standard ",
        "buckets), not \"", jurisdiction, "\"",
        call. = FALSE
      )
    }
  }

  host <- if (is.null(jurisdiction)) {
    sprintf("%s.r2.cloudflarestorage.com", parsed$account_id)
  } else {
    sprintf("%s.%s.r2.cloudflarestorage.com", parsed$account_id, jurisdiction)
  }

  # R2's S3 API requires region=auto
  url <- sprintf("s3://%s?endpoint=https://%s&region=auto", bucket, host)

  new_pmtiles_bucket(
    url = url,
    env = aws_credential_env(access_key, secret_key),
    provider = "Cloudflare R2"
  )
}

#' @rdname bucket_helpers
#' @export
s3_bucket <- function(bucket,
                      region = NULL,
                      access_key = NULL,
                      secret_key = NULL,
                      session_token = NULL) {

  check_bucket_name(bucket)

  url <- paste0("s3://", bucket)
  if (!is.null(region)) {
    check_optional_string(region, "region")
    url <- paste0(url, "?region=", trimws(region))
  }

  new_pmtiles_bucket(
    url = url,
    env = aws_credential_env(access_key, secret_key, session_token),
    provider = "Amazon S3"
  )
}

#' @rdname bucket_helpers
#' @export
gcs_bucket <- function(bucket, credentials_file = NULL) {

  check_bucket_name(bucket)

  env <- character()
  if (!is.null(credentials_file)) {
    check_optional_string(credentials_file, "credentials_file")
    credentials_file <- path.expand(credentials_file)
    if (!file.exists(credentials_file)) {
      stop("Credentials file not found: ", credentials_file, call. = FALSE)
    }
    env <- c(GOOGLE_APPLICATION_CREDENTIALS = credentials_file)
  }

  new_pmtiles_bucket(
    url = paste0("gs://", bucket),
    env = env,
    provider = "Google Cloud Storage"
  )
}

#' @rdname bucket_helpers
#' @export
azure_bucket <- function(container,
                         storage_account,
                         access_key = NULL,
                         sas_token = NULL) {

  check_bucket_name(container, what = "container")

  if (!is.character(storage_account) || length(storage_account) != 1 ||
      !nzchar(storage_account)) {
    stop("storage_account must be a single non-empty string", call. = FALSE)
  }

  if (!is.null(access_key) && !is.null(sas_token)) {
    stop("Provide either access_key or sas_token, not both", call. = FALSE)
  }
  check_optional_string(access_key, "access_key")
  check_optional_string(sas_token, "sas_token")

  env <- c(AZURE_STORAGE_ACCOUNT = storage_account)
  if (!is.null(access_key)) {
    env <- c(env, AZURE_STORAGE_KEY = access_key)
  }
  if (!is.null(sas_token)) {
    env <- c(env, AZURE_STORAGE_SAS_TOKEN = sas_token)
  }

  new_pmtiles_bucket(
    url = sprintf("azblob://%s?storage_account=%s", container, storage_account),
    env = env,
    provider = "Azure Blob Storage"
  )
}

#' @rdname bucket_helpers
#' @export
s3_compatible_bucket <- function(bucket,
                                 endpoint,
                                 region = NULL,
                                 path_style = FALSE,
                                 access_key = NULL,
                                 secret_key = NULL,
                                 session_token = NULL) {

  check_bucket_name(bucket)

  if (is.null(endpoint)) {
    stop("endpoint must be a single non-empty string", call. = FALSE)
  }
  check_optional_string(endpoint, "endpoint")
  endpoint <- trimws(endpoint)
  scheme <- if (grepl("^http://", endpoint)) "http://" else "https://"
  rest <- sub("^https?://", "", endpoint)
  rest <- sub("/+$", "", rest)
  host <- sub("/.*$", "", rest)
  if (!nzchar(rest) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9.-]*(:[0-9]+)?$", host)) {
    stop(
      "endpoint must include a host, e.g. \"https://nyc3.digitaloceanspaces.com\"",
      call. = FALSE
    )
  }
  endpoint <- paste0(scheme, rest)

  # A pasted endpoint that ends with the bucket name would put the bucket
  # in the URL twice, which fails at upload time -- catch it here instead.
  bucket_suffix <- paste0("/", bucket)
  if (endsWith(endpoint, bucket_suffix)) {
    stop(
      "endpoint must not include the bucket name.\n",
      "Use endpoint = \"",
      substr(endpoint, 1, nchar(endpoint) - nchar(bucket_suffix)), "\"",
      call. = FALSE
    )
  }

  if (is.null(region)) {
    region <- infer_s3_region(endpoint)
    if (is.null(region)) {
      stop(
        "Could not determine the region for endpoint ", endpoint, ".\n",
        "Pass region explicitly -- check your provider's documentation. ",
        "AWS-style providers (e.g. DigitalOcean Spaces, Backblaze B2) ",
        "require their region slug; some providers accept region = \"auto\".",
        call. = FALSE
      )
    }
  } else {
    check_optional_string(region, "region")
    region <- trimws(region)
  }

  url <- sprintf("s3://%s?endpoint=%s&region=%s", bucket, endpoint, region)
  if (isTRUE(path_style)) {
    url <- paste0(url, "&use_path_style=true")
  }

  new_pmtiles_bucket(
    url = url,
    env = aws_credential_env(access_key, secret_key, session_token),
    provider = "S3-compatible"
  )
}

#' @export
print.pmtiles_bucket <- function(x, ...) {
  cat("<pmtiles bucket>\n")
  cat("  Provider:", x$provider, "\n")
  cat("  URL:", x$url, "\n")
  if (length(x$env)) {
    cat("  Credentials:", paste(names(x$env), collapse = ", "),
        "(passed only to the pmtiles subprocess)\n")
  } else {
    cat("  Credentials: from environment\n")
  }
  invisible(x)
}

#' @export
format.pmtiles_bucket <- function(x, ...) {
  x$url
}

#' @export
as.character.pmtiles_bucket <- function(x, ...) {
  x$url
}

# --- internal ---------------------------------------------------------------

new_pmtiles_bucket <- function(url, env = character(), provider) {
  structure(
    list(url = url, env = env, provider = provider),
    class = "pmtiles_bucket"
  )
}

#' Normalize a bucket argument to a URL string plus subprocess env vars
#'
#' Accepts NULL, a plain bucket URL string, or a pmtiles_bucket object from
#' the bucket helpers.
#' @keywords internal
#' @noRd
resolve_bucket <- function(bucket) {
  if (is.null(bucket)) {
    return(list(url = NULL, env = character()))
  }
  if (inherits(bucket, "pmtiles_bucket")) {
    return(list(url = bucket$url, env = bucket$env))
  }
  if (is.character(bucket) && length(bucket) == 1 && nzchar(bucket)) {
    return(list(url = bucket, env = character()))
  }
  stop(
    "bucket must be a single character string or a bucket helper object ",
    "(see ?r2_bucket)",
    call. = FALSE
  )
}

check_bucket_name <- function(bucket, what = "bucket") {
  if (!is.character(bucket) || length(bucket) != 1 || !nzchar(bucket)) {
    stop(what, " must be a single non-empty string", call. = FALSE)
  }
  if (grepl("^[a-z0-9]+://", bucket)) {
    stop(
      what, " should be just the ", what, " name (e.g. \"my-tiles\"), ",
      "not a URL",
      call. = FALSE
    )
  }
  if (grepl("[[:space:]]", bucket)) {
    stop(what, " must not contain whitespace", call. = FALSE)
  }
  invisible(bucket)
}

#' Validate an optional credential-like argument
#'
#' NULL is allowed (meaning "use the environment"); anything else must be a
#' single non-empty string.
#' @keywords internal
#' @noRd
check_optional_string <- function(x, name) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (!is.character(x) || length(x) != 1 || !nzchar(trimws(x))) {
    stop(
      name, " must be a non-empty string. If you are reading it with ",
      "Sys.getenv(), check that the variable is set ",
      "(e.g. in ~/.Renviron, then restart R).",
      call. = FALSE
    )
  }
  invisible(x)
}

aws_credential_env <- function(access_key, secret_key, session_token = NULL) {
  if (is.null(access_key) != is.null(secret_key)) {
    stop(
      "Provide both access_key and secret_key, or neither ",
      "(to use environment variables)",
      call. = FALSE
    )
  }
  check_optional_string(session_token, "session_token")
  if (is.null(access_key)) {
    if (!is.null(session_token)) {
      stop(
        "session_token requires access_key and secret_key to be ",
        "supplied as well",
        call. = FALSE
      )
    }
    return(character())
  }
  check_optional_string(access_key, "access_key")
  check_optional_string(secret_key, "secret_key")
  env <- c(
    AWS_ACCESS_KEY_ID = access_key,
    AWS_SECRET_ACCESS_KEY = secret_key
  )
  if (!is.null(session_token)) {
    env <- c(env, AWS_SESSION_TOKEN = session_token)
  }
  env
}

#' Infer the S3 region from a recognized S3-compatible endpoint
#'
#' Returns NULL when the endpoint isn't recognized, in which case the caller
#' must supply a region.
#' @keywords internal
#' @noRd
infer_s3_region <- function(endpoint) {
  host <- sub("^https?://", "", endpoint)
  host <- sub("/.*$", "", host)
  host <- sub(":[0-9]+$", "", host)

  # DigitalOcean Spaces: {region}.digitaloceanspaces.com
  m <- regmatches(host, regexec("^([a-z0-9-]+)\\.digitaloceanspaces\\.com$", host))[[1]]
  if (length(m) == 2) {
    return(m[[2]])
  }

  # Backblaze B2: s3.{region}.backblazeb2.com
  m <- regmatches(host, regexec("^s3\\.([a-z0-9-]+)\\.backblazeb2\\.com$", host))[[1]]
  if (length(m) == 2) {
    return(m[[2]])
  }

  # Source Cooperative accepts "auto"
  if (host == "data.source.coop") {
    return("auto")
  }

  NULL
}

#' Extract a bare R2 account ID from whatever the user pasted
#'
#' Tolerates the full "S3 API" URL shown on a bucket's Settings page
#' (https://ACCOUNT.r2.cloudflarestorage.com/bucket-name), with or without
#' scheme, jurisdiction subdomain, or trailing bucket path.
#' @keywords internal
#' @noRd
parse_r2_account_id <- function(account_id) {
  if (!is.character(account_id) || length(account_id) != 1 ||
      !nzchar(trimws(account_id))) {
    stop("account_id must be a single non-empty string", call. = FALSE)
  }

  original <- trimws(account_id)
  x <- sub("^https?://", "", original)
  jurisdiction <- NULL

  if (grepl("\\.r2\\.cloudflarestorage\\.com", x, ignore.case = TRUE)) {
    x <- sub("/.*$", "", x)  # drop any /bucket-name path
    prefix <- sub("\\.r2\\.cloudflarestorage\\.com$", "", x, ignore.case = TRUE)
    parts <- strsplit(prefix, ".", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      jurisdiction <- parts[[2]]
      x <- parts[[1]]
    } else if (length(parts) == 1) {
      x <- parts[[1]]
    } else {
      stop(
        "Could not extract an account ID from \"", original, "\".\n",
        "Pass your Cloudflare account ID (32-character hex string from the ",
        "Account Details panel of the R2 dashboard).",
        call. = FALSE
      )
    }
    message("Extracted account ID \"", x, "\" from the URL provided.")
  }

  if (!grepl("^[[:xdigit:]]{32}$", x)) {
    stop(
      "account_id \"", original, "\" doesn't look like a Cloudflare account ",
      "ID (a 32-character hex string). Copy it from the Account Details ",
      "panel of the R2 dashboard.",
      call. = FALSE
    )
  }

  list(account_id = x, jurisdiction = jurisdiction)
}
