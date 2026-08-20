# Bucket helper constructors

ACCOUNT <- "0123456789abcdef0123456789abcdef"

test_that("r2_bucket builds the correct URL", {
  b <- r2_bucket("parcel-data", account_id = ACCOUNT)
  expect_s3_class(b, "pmtiles_bucket")
  expect_identical(
    b$url,
    sprintf(
      "s3://parcel-data?endpoint=https://%s.r2.cloudflarestorage.com&region=auto",
      ACCOUNT
    )
  )
  expect_length(b$env, 0)
})

test_that("r2_bucket extracts the account ID from a pasted S3 API URL", {
  plain <- r2_bucket("parcel-data", account_id = ACCOUNT)

  # Full URL from the bucket Settings page, including the /bucket-name suffix
  pasted <- NULL
  expect_message(
    pasted <- r2_bucket(
      "parcel-data",
      account_id = sprintf(
        "https://%s.r2.cloudflarestorage.com/parcel-data", ACCOUNT
      )
    ),
    "Extracted account ID"
  )
  expect_identical(pasted$url, plain$url)

  # Scheme-less variant
  bare <- NULL
  expect_message(
    bare <- r2_bucket(
      "parcel-data",
      account_id = sprintf("%s.r2.cloudflarestorage.com", ACCOUNT)
    ),
    "Extracted account ID"
  )
  expect_identical(bare$url, plain$url)
})

test_that("r2_bucket detects a jurisdiction from a pasted URL", {
  b <- NULL
  expect_message(
    b <- r2_bucket(
      "tiles",
      account_id = sprintf("https://%s.eu.r2.cloudflarestorage.com", ACCOUNT)
    ),
    "Extracted account ID"
  )
  expect_match(
    b$url,
    sprintf("%s\\.eu\\.r2\\.cloudflarestorage\\.com", ACCOUNT)
  )
})

test_that("r2_bucket honors an explicit jurisdiction", {
  b <- r2_bucket("tiles", account_id = ACCOUNT, jurisdiction = "eu")
  expect_match(b$url, "\\.eu\\.r2\\.cloudflarestorage\\.com")
})

test_that("r2_bucket rejects invalid account IDs", {
  expect_error(
    r2_bucket("tiles", account_id = "not-an-account-id"),
    "doesn't look like a Cloudflare account"
  )
  # right characters, wrong length
  expect_error(
    r2_bucket("tiles", account_id = "abc123"),
    "doesn't look like a Cloudflare account"
  )
  # unrelated URL
  expect_error(
    r2_bucket("tiles", account_id = "https://example.com/foo"),
    "doesn't look like a Cloudflare account"
  )
  expect_error(r2_bucket("tiles", account_id = ""), "non-empty")
  expect_error(r2_bucket("tiles", account_id = NULL), "non-empty")
})

test_that("credentials are attached to the object, never the session", {
  b <- r2_bucket(
    "tiles", account_id = ACCOUNT,
    access_key = "AK", secret_key = "SK"
  )
  expect_identical(
    b$env,
    c(AWS_ACCESS_KEY_ID = "AK", AWS_SECRET_ACCESS_KEY = "SK")
  )
  expect_identical(Sys.getenv("AWS_ACCESS_KEY_ID", unset = ""), "")
})

test_that("partial or empty AWS credentials are rejected", {
  expect_error(
    r2_bucket("tiles", account_id = ACCOUNT, access_key = "AK"),
    "both access_key and secret_key"
  )
  expect_error(
    r2_bucket("tiles", account_id = ACCOUNT, secret_key = "SK"),
    "both access_key and secret_key"
  )
  # empty string, e.g. from an unset Sys.getenv() variable
  expect_error(
    r2_bucket(
      "tiles", account_id = ACCOUNT,
      access_key = Sys.getenv("PMTILES_UNSET_TEST_VAR"), secret_key = "SK"
    ),
    "non-empty"
  )
})

test_that("s3_bucket builds URLs with and without a region", {
  expect_identical(s3_bucket("b")$url, "s3://b")
  expect_identical(
    s3_bucket("b", region = "us-east-1")$url,
    "s3://b?region=us-east-1"
  )
})

test_that("s3_bucket supports session tokens only alongside key pairs", {
  b <- s3_bucket("b", access_key = "A", secret_key = "S", session_token = "T")
  expect_identical(unname(b$env[["AWS_SESSION_TOKEN"]]), "T")
  expect_error(
    s3_bucket("b", session_token = "T"),
    "session_token requires access_key and secret_key"
  )
  expect_error(
    s3_bucket("b", access_key = "A", secret_key = "S", session_token = ""),
    "non-empty"
  )
})

test_that("gcs_bucket builds URLs and validates the credentials file", {
  expect_identical(gcs_bucket("b")$url, "gs://b")
  expect_error(
    gcs_bucket("b", credentials_file = tempfile("nope", fileext = ".json")),
    "not found"
  )
  expect_error(gcs_bucket("b", credentials_file = ""), "non-empty")

  creds <- tempfile(fileext = ".json")
  writeLines("{}", creds)
  on.exit(unlink(creds), add = TRUE)
  b <- gcs_bucket("b", credentials_file = creds)
  expect_identical(
    unname(b$env[["GOOGLE_APPLICATION_CREDENTIALS"]]),
    creds
  )
})

test_that("azure_bucket builds URLs and env vars", {
  b <- azure_bucket("cont", storage_account = "acct", access_key = "K")
  expect_identical(b$url, "azblob://cont?storage_account=acct")
  expect_identical(
    b$env,
    c(AZURE_STORAGE_ACCOUNT = "acct", AZURE_STORAGE_KEY = "K")
  )

  b2 <- azure_bucket("cont", storage_account = "acct", sas_token = "S")
  expect_identical(unname(b2$env[["AZURE_STORAGE_SAS_TOKEN"]]), "S")
})

test_that("azure_bucket rejects conflicting or empty credentials", {
  expect_error(
    azure_bucket("c", "a", access_key = "K", sas_token = "S"),
    "not both"
  )
  expect_error(azure_bucket("c", "a", access_key = ""), "non-empty")
  expect_error(azure_bucket("c", "a", sas_token = " "), "non-empty")
  expect_error(azure_bucket("c", storage_account = ""), "non-empty")
})

test_that("s3_compatible_bucket infers regions for recognized endpoints", {
  # DigitalOcean Spaces
  b <- s3_compatible_bucket(
    "b", endpoint = "https://nyc3.digitaloceanspaces.com"
  )
  expect_identical(
    b$url,
    "s3://b?endpoint=https://nyc3.digitaloceanspaces.com&region=nyc3"
  )

  # Backblaze B2
  b <- s3_compatible_bucket(
    "b", endpoint = "https://s3.us-west-004.backblazeb2.com"
  )
  expect_match(b$url, "region=us-west-004", fixed = TRUE)

  # Source Cooperative
  b <- s3_compatible_bucket("org", endpoint = "https://data.source.coop")
  expect_match(b$url, "region=auto", fixed = TRUE)
})

test_that("blank endpoints and regions are rejected", {
  expect_error(s3_compatible_bucket("b", endpoint = "   ", region = "us-east-1"), "non-empty")
  expect_error(s3_compatible_bucket("b", endpoint = NULL, region = "us-east-1"), "non-empty")
  expect_error(s3_compatible_bucket("b", endpoint = "https:///", region = "x"), "host")
  expect_error(
    s3_compatible_bucket("b", endpoint = "https://minio.example.com", region = ""),
    "non-empty"
  )
  expect_error(
    s3_compatible_bucket("b", endpoint = "https://minio.example.com", region = "  "),
    "non-empty"
  )
  expect_error(s3_bucket("b", region = ""), "non-empty")
})

test_that("r2_bucket rejects unsupported jurisdictions", {
  expect_error(
    r2_bucket("tiles", account_id = ACCOUNT, jurisdiction = "bogus"),
    "jurisdiction must be"
  )
  expect_match(
    r2_bucket("tiles", account_id = ACCOUNT, jurisdiction = "fedramp")$url,
    "\\.fedramp\\.r2\\.cloudflarestorage\\.com"
  )
})

test_that("s3_compatible_bucket requires a region for unknown endpoints", {
  expect_error(
    s3_compatible_bucket("b", endpoint = "https://minio.example.com"),
    "Could not determine the region"
  )
  b <- s3_compatible_bucket(
    "b", endpoint = "minio.example.com:9000/",
    region = "us-east-1", path_style = TRUE
  )
  expect_identical(
    b$url,
    "s3://b?endpoint=https://minio.example.com:9000&region=us-east-1&use_path_style=true"
  )
})

test_that("s3_compatible_bucket catches a bucket name inside the endpoint", {
  expect_error(
    s3_compatible_bucket(
      "my-tiles", endpoint = "https://nyc3.digitaloceanspaces.com/my-tiles"
    ),
    "must not include the bucket name"
  )
  # exact match only: a dotted bucket name must not be treated as a regex
  b <- s3_compatible_bucket(
    "foo.bar", endpoint = "https://example.com/fooXbar", region = "auto"
  )
  expect_match(b$url, "s3://foo.bar?", fixed = TRUE)
})

test_that("bucket and container names are validated", {
  expect_error(r2_bucket("s3://tiles", account_id = ACCOUNT), "not a URL")
  expect_error(r2_bucket("", account_id = ACCOUNT), "non-empty")
  expect_error(r2_bucket("foo bar", account_id = ACCOUNT), "whitespace")
  expect_error(azure_bucket("", "acct"), "non-empty")
})

test_that("session tokens work in s3_compatible_bucket", {
  b <- s3_compatible_bucket(
    "org", endpoint = "https://data.source.coop",
    access_key = "A", secret_key = "S", session_token = "T"
  )
  expect_identical(unname(b$env[["AWS_SESSION_TOKEN"]]), "T")
})

test_that("resolve_bucket handles NULL, strings, and helper objects", {
  expect_null(resolve_bucket(NULL)$url)
  expect_identical(resolve_bucket("s3://plain")$url, "s3://plain")
  expect_length(resolve_bucket("s3://plain")$env, 0)

  b <- r2_bucket("tiles", account_id = ACCOUNT, access_key = "A", secret_key = "S")
  resolved <- resolve_bucket(b)
  expect_identical(resolved$url, b$url)
  expect_identical(resolved$env, b$env)

  expect_error(resolve_bucket(42), "bucket helper")
  expect_error(resolve_bucket(c("a", "b")), "bucket helper")
})

test_that("print, format, and as.character methods work", {
  b <- r2_bucket(
    "tiles", account_id = ACCOUNT,
    access_key = "AKIAEXAMPLEKEY", secret_key = "supersecretvalue"
  )
  expect_identical(as.character(b), b$url)
  expect_identical(format(b), b$url)
  expect_output(print(b), "Cloudflare R2")
  expect_output(print(b), "AWS_ACCESS_KEY_ID")
  # secret values themselves are never printed
  printed <- paste(capture.output(print(b)), collapse = "\n")
  expect_false(grepl("supersecretvalue", printed, fixed = TRUE))
  expect_false(grepl("AKIAEXAMPLEKEY", printed, fixed = TRUE))
})
