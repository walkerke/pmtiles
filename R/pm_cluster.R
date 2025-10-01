#' Cluster a PMTiles archive
#'
#' @description
#' Cluster an unclustered PMTiles archive, optimizing its size and layout.
#' Archives created by tippecanoe, planetiler, and the pmtiles CLI are already
#' clustered and do not need this operation.
#'
#' @param input Path to input PMTiles file. The file will be modified in place.
#' @param no_deduplication Logical. If `TRUE`, skips tile deduplication to
#'   speed up clustering. Use this if you know the input has only unique tiles.
#'   Default is `FALSE`.
#' @param verbose Logical. If `TRUE`, prints progress information. Default is `TRUE`.
#'
#' @return Invisibly returns the path to the clustered archive.
#'
#' @examples
#' \dontrun{
#' # Cluster an archive
#' pm_cluster("archive.pmtiles")
#'
#' # Cluster without deduplication (faster)
#' pm_cluster("archive.pmtiles", no_deduplication = TRUE)
#' }
#'
#' @export
pm_cluster <- function(input,
                       no_deduplication = FALSE,
                       verbose = TRUE) {

  # Validate inputs
  if (!file.exists(input)) {
    stop("Input file not found: ", input, call. = FALSE)
  }

  # Build command arguments
  args <- c("cluster", input)

  if (no_deduplication) {
    args <- c(args, "--no-deduplication")
  }

  # Execute command
  if (verbose) {
    message("Clustering ", input, "...")
    message("Note: This will modify the file in place")
  }

  result <- pmtiles_exec(args)

  if (verbose) {
    cat(result$stdout)
  }

  if (result$status == 0) {
    message("\u2713 Successfully clustered: ", input)
  }

  return(invisible(input))
}
