# Internal utility functions for pmtiles package

#' Get path to pmtiles binary
#' @keywords internal
#' @noRd
pmtiles_binary <- function() {
  # Detect platform
  os <- tolower(Sys.info()[["sysname"]])
  arch <- Sys.info()[["machine"]]

  # Map OS names
  if (os == "darwin") {
    platform <- "darwin"
  } else if (os == "linux") {
    platform <- "linux"
  } else if (grepl("windows", os, ignore.case = TRUE)) {
    platform <- "windows"
  } else {
    stop("Unsupported operating system: ", os, call. = FALSE)
  }

  # Map architecture names
  if (arch %in% c("x86_64", "x86-64", "amd64", "AMD64")) {
    binarch <- "amd64"
  } else if (arch %in% c("aarch64", "arm64", "ARM64")) {
    binarch <- "arm64"
  } else {
    stop("Unsupported architecture: ", arch, call. = FALSE)
  }

  # Determine binary name
  binary_name <- if (platform == "windows") "pmtiles.exe" else "pmtiles"

  # Look for binary in package installation
  binary_path <- system.file(
    "bin",
    paste0(platform, "_", binarch),
    binary_name,
    package = "pmtiles"
  )

  # For development with devtools::load_all(), check local inst/ directory
  if (!file.exists(binary_path) || binary_path == "") {
    # Try to find inst/bin in development
    pkg_path <- find.package("pmtiles", quiet = TRUE)
    if (length(pkg_path) > 0) {
      dev_binary_path <- file.path(
        pkg_path,
        "inst", "bin",
        paste0(platform, "_", binarch),
        binary_name
      )
      if (file.exists(dev_binary_path)) {
        binary_path <- dev_binary_path
      }
    }
  }

  # Check if binary exists
  if (!file.exists(binary_path) || binary_path == "") {
    stop(
      "PMTiles binary not found for ", platform, "_", binarch, ".\n",
      "Please reinstall the package or build from source with Go installed.",
      call. = FALSE
    )
  }

  # Ensure binary is executable (Unix-like systems)
  if (platform != "windows") {
    Sys.chmod(binary_path, mode = "0755")
  }

  return(binary_path)
}

#' Execute pmtiles command
#' @keywords internal
#' @noRd
pmtiles_exec <- function(args, stdout = "|", stderr = "|", error_on_status = TRUE,
                         stdout_callback = NULL) {
  binary <- pmtiles_binary()

  # Build argument list, only including non-NULL optional parameters
  run_args <- list(
    command = binary,
    args = args,
    stdout = stdout,
    stderr = stderr,
    error_on_status = FALSE,
    echo_cmd = FALSE,
    echo = FALSE
  )

  if (!is.null(stdout_callback)) {
    run_args$stdout_callback <- stdout_callback
  }

  result <- do.call(processx::run, run_args)

  if (error_on_status && result$status != 0) {
    stop(
      "PMTiles command failed with status ", result$status, "\n",
      "Error: ", result$stderr,
      call. = FALSE
    )
  }

  return(result)
}

#' Parse JSON output from pmtiles
#' @keywords internal
#' @noRd
parse_pmtiles_json <- function(json_string) {
  if (is.null(json_string) || nchar(trimws(json_string)) == 0) {
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(json_string, simplifyVector = FALSE),
    error = function(e) {
      warning("Failed to parse JSON output: ", e$message, call. = FALSE)
      return(json_string)
    }
  )
}

#' Format file size for display
#' @keywords internal
#' @noRd
format_bytes <- function(bytes) {
  if (is.na(bytes) || is.null(bytes)) return("Unknown")

  units <- c("B", "KB", "MB", "GB", "TB")
  size <- as.numeric(bytes)
  unit_idx <- 1

  while (size >= 1024 && unit_idx < length(units)) {
    size <- size / 1024
    unit_idx <- unit_idx + 1
  }

  sprintf("%.2f %s", size, units[unit_idx])
}
