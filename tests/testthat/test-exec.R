# Regression tests for pmtiles_exec() error surfacing and the credential
# env plumbing from bucket helpers into the go-pmtiles subprocess.
#
# These run the bundled pmtiles binary; skip if it isn't available for this
# platform.

skip_if_no_binary <- function() {
  ok <- tryCatch({
    pmtiles_binary()
    TRUE
  }, error = function(e) FALSE)
  if (!ok) skip("pmtiles binary not available")
}

test_that("the bundled binary runs", {
  skip_if_no_binary()
  expect_output(pm_version(), "pmtiles")
})

test_that("CLI errors surface the underlying message (stderr path)", {
  skip_if_no_binary()
  err <- tryCatch(
    pm_show(file.path(tempdir(), "definitely-missing.pmtiles")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "failed with status")
  # the go-pmtiles message, not a generic placeholder
  expect_match(err, "Failed to show archive")
  expect_no_match(err, "no output captured")
})

test_that("pm_tile surfaces errors written to a redirected stdout file", {
  skip_if_no_binary()
  out <- tempfile(fileext = ".mvt")
  err <- tryCatch(
    pm_tile("definitely-missing.pmtiles", 0, 0, 0, output = out),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "failed with status")
  expect_no_match(err, "no output captured")
  # a failed run must not leave an error-text file masquerading as a tile
  expect_false(file.exists(out))
})

test_that("a failed pm_tile preserves a pre-existing output file", {
  skip_if_no_binary()
  out <- tempfile(fileext = ".mvt")
  writeBin(as.raw(c(0x1a, 0x2b, 0x3c)), out)
  on.exit(unlink(out), add = TRUE)

  expect_error(pm_tile("definitely-missing.pmtiles", 0, 0, 0, output = out))

  expect_true(file.exists(out))
  expect_identical(
    readBin(out, "raw", n = 10),
    as.raw(c(0x1a, 0x2b, 0x3c))
  )
})

test_that("bucket helper credentials reach the subprocess env", {
  skip_if_no_binary()
  # An unreadable GCS credentials file makes go-pmtiles fail while parsing
  # it -- proof that GOOGLE_APPLICATION_CREDENTIALS was delivered to the
  # child process. No valid cloud credentials or network access required.
  creds <- tempfile(fileext = ".json")
  writeLines("this is not json", creds)
  on.exit(unlink(creds), add = TRUE)

  before <- Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA)

  b <- gcs_bucket("some-bucket", credentials_file = creds)
  err <- tryCatch(
    pm_show("x.pmtiles", bucket = b),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "GOOGLE_APPLICATION_CREDENTIALS environment variable")

  # and the parent R session was never touched
  expect_identical(Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS", unset = NA), before)
})
