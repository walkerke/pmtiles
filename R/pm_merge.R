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
#' coordinates is an error. All inputs must also share the same tile type
#' and tile compression.
#'
#' The merged archive's JSON metadata and center are copied from the
#' **first input only**. In particular, `vector_layers` metadata comes from
#' the first archive -- so inputs should use the same layer name(s), or
#' layers present only in later inputs will be missing from the merged
#' metadata even though their tiles are included. To recombine tilesets
#' with differing layers, re-create the archive from source data instead
#' (see [pm_create()], which supports multi-layer output via [pm_layer()]).
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

  if (!is.character(output) || length(output) != 1 || !nzchar(trimws(output))) {
    stop("output must be a single file path", call. = FALSE)
  }
  output <- path.expand(output)

  input_paths <- normalizePath(inputs, mustWork = FALSE)
  output_path <- normalizePath(output, mustWork = FALSE)
  if (output_path %in% input_paths) {
    stop("output must not be one of the input archives", call. = FALSE)
  }

  # Merge into a temp file first: the CLI truncates its output path
  # immediately, so a failed merge must not destroy an existing output
  tmp_out <- tempfile(fileext = ".pmtiles")
  on.exit(unlink(tmp_out), add = TRUE)

  args <- c("merge", inputs, tmp_out)

  if (verbose) {
    message("Merging ", length(inputs), " archives into ", output, "...")
  }

  result <- pmtiles_exec(args)

  if (verbose) {
    cat(result$stdout)
  }

  # Replace the destination via rename, which swaps the directory entry:
  # copying with overwrite = TRUE would write through a destination that
  # hard-links an input (or anything else) and corrupt it. The staging file
  # lives next to the output so the rename stays on one filesystem.
  staging <- paste0(output, ".merging-", Sys.getpid())
  on.exit(unlink(staging), add = TRUE)
  if (!file.copy(tmp_out, staging, overwrite = TRUE)) {
    stop("Failed to write merged archive to: ", output, call. = FALSE)
  }
  moved <- file.rename(staging, output)
  if (!moved) {
    # e.g. Windows with an existing destination: clear it, then retry
    unlink(output)
    moved <- file.rename(staging, output) || file.copy(staging, output)
  }
  if (!moved) {
    stop("Failed to write merged archive to: ", output, call. = FALSE)
  }

  if (verbose) {
    message("\u2713 Successfully created: ", output)
  }

  return(invisible(output))
}
