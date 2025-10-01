#' Get PMTiles CLI version
#'
#' @description
#' Display the version information of the PMTiles command-line tool.
#'
#' @return Character string containing version information.
#'
#' @examples
#' \dontrun{
#' pm_version()
#' }
#'
#' @export
pm_version <- function() {
  args <- c("version")

  result <- pmtiles_exec(args)

  version_info <- trimws(result$stdout)
  cat(version_info, "\n")

  return(invisible(version_info))
}
