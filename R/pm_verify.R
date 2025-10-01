#' Verify PMTiles archive structure
#'
#' @description
#' Check that a PMTiles archive is ordered correctly and has correct header
#' information. This verifies the archive structure without checking individual
#' tile contents.
#'
#' @param input Path to a local PMTiles file.
#'
#' @return Invisibly returns `TRUE` if verification succeeds, throws an error otherwise.
#'
#' @examples
#' \dontrun{
#' # Verify an archive
#' pm_verify("archive.pmtiles")
#' }
#'
#' @export
pm_verify <- function(input) {

  if (!file.exists(input)) {
    stop("File not found: ", input, call. = FALSE)
  }

  # Build command arguments
  args <- c("verify", input)

  # Execute command
  result <- pmtiles_exec(args)

  # Print output
  cat(result$stdout)

  if (result$status == 0) {
    message("\u2713 Archive verification successful")
    return(invisible(TRUE))
  } else {
    stop("Archive verification failed", call. = FALSE)
  }
}
