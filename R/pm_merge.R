#' Merge multiple PMTiles archives into one
#'
#' @description
#' Combine two or more disjoint PMTiles archives into a single archive.
#' Archives are disjoint when no tile coordinate appears in more than one
#' input -- for example, tilesets covering non-overlapping geographic regions
#' at the same zoom levels, or the same region at non-overlapping zoom
#' ranges.
#'
#' @param inputs Character vector of paths to two or more local PMTiles
#'   archives to merge.
#' @param output Path for the merged output archive.
#' @param verbose Logical. If `TRUE`, prints progress information. Default is
#'   `TRUE`.
#'
#' @details
#' Merging is performed by the go-pmtiles CLI (`pmtiles merge`). The input
#' archives must be disjoint; merging archives that contain the same tile
#' coordinates is an error. To recombine overlapping tilesets, re-create the
#' archive from source data instead (see [pm_create()], which supports
#' multi-layer output via [pm_layer()]).
#'
#' @return Invisibly returns the path to the output archive.
#'
#' @seealso [pm_create()], [pm_extract()], [pm_convert()]
#'
#' @examples
#' \dontrun{
#' # Merge two regional tilesets into one archive
#' pm_merge(
#'   c("west_region.pmtiles", "east_region.pmtiles"),
#'   output = "combined.pmtiles"
#' )
#' }
#'
#' @export
pm_merge <- function(inputs, output, verbose = TRUE) {

  if (!is.character(inputs) || length(inputs) < 2) {
    stop(
      "inputs must be a character vector of at least two PMTiles archives",
      call. = FALSE
    )
  }

  inputs <- path.expand(inputs)
  missing_inputs <- inputs[!file.exists(inputs)]
  if (length(missing_inputs)) {
    stop(
      "Input file(s) not found: ", paste(missing_inputs, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(output) || length(output) != 1 || !nzchar(output)) {
    stop("output must be a single file path", call. = FALSE)
  }
  output <- path.expand(output)

  args <- c("merge", inputs, output)

  if (verbose) {
    message("Merging ", length(inputs), " archives into ", output, "...")
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
