#' Convert MBTiles to PMTiles
#'
#' @description
#' Convert an MBTiles database to PMTiles format. The conversion process
#' automatically deduplicates tiles unless disabled.
#'
#' @param input Path to input MBTiles file.
#' @param output Path for output PMTiles file.
#' @param force Logical. If `TRUE`, removes existing output file if present.
#'   Default is `FALSE`.
#' @param no_deduplication Logical. If `TRUE`, skips tile deduplication to
#'   speed up conversion. Use this if you know the input has only unique tiles.
#'   Default is `FALSE`.
#' @param tmpdir Optional path to a folder for temporary files during conversion.
#'   If not specified, uses the system temporary directory.
#' @param verbose Logical. If `TRUE`, prints progress information. Default is `TRUE`.
#'
#' @return Invisibly returns the path to the output archive.
#'
#' @examples
#' \dontrun{
#' # Convert MBTiles to PMTiles
#' pm_convert("input.mbtiles", "output.pmtiles")
#'
#' # Convert without deduplication (faster)
#' pm_convert("input.mbtiles", "output.pmtiles", no_deduplication = TRUE)
#'
#' # Force overwrite existing file
#' pm_convert("input.mbtiles", "output.pmtiles", force = TRUE)
#' }
#'
#' @export
pm_convert <- function(input,
                       output,
                       force = FALSE,
                       no_deduplication = FALSE,
                       tmpdir = NULL,
                       verbose = TRUE) {

  # Validate inputs
  if (!file.exists(input)) {
    stop("Input file not found: ", input, call. = FALSE)
  }

  if (file.exists(output)) {
    if (force) {
      if (verbose) {
        message("Removing existing output file: ", output)
      }
      file.remove(output)
    } else {
      stop("Output file already exists: ", output, "\nUse force = TRUE to overwrite.",
           call. = FALSE)
    }
  }

  # Build command arguments
  args <- c("convert", input, output)

  if (no_deduplication) {
    args <- c(args, "--no-deduplication")
  }

  if (!is.null(tmpdir)) {
    if (!dir.exists(tmpdir)) {
      stop("Temporary directory not found: ", tmpdir, call. = FALSE)
    }
    args <- c(args, paste0("--tmpdir=", tmpdir))
  }

  # Execute command
  if (verbose) {
    message("Converting ", input, " to PMTiles format...")
    if (!no_deduplication) {
      message("Deduplication enabled (this may take some time)")
    }
  }

  result <- pmtiles_exec(args)

  if (verbose) {
    cat(result$stdout)
  }

  if (result$status == 0) {
    message("\u2713 Successfully created: ", output)
  }

  return(invisible(output))
}
