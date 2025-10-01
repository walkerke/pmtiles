#' Extract a single tile from a PMTiles archive
#'
#' @description
#' Fetch one tile from a local or remote PMTiles archive and save it to a file.
#'
#' @param input Path to a local PMTiles file or URL to a remote archive.
#' @param z Integer zoom level.
#' @param x Integer tile column.
#' @param y Integer tile row.
#' @param output Path where the tile should be saved. If `NULL` (default),
#'   returns the raw tile data as a raw vector.
#' @param bucket Optional remote bucket specification for cloud storage.
#'
#' @return
#' If `output` is specified, writes tile to file and returns the output path invisibly.
#' If `output` is `NULL`, returns the tile data as a raw vector.
#'
#' @examples
#' \dontrun{
#' # Get tile data
#' tile_data <- pm_tile("archive.pmtiles", z = 0, x = 0, y = 0)
#'
#' # Save tile to file
#' pm_tile("archive.pmtiles", z = 5, x = 10, y = 12, output = "tile.mvt")
#' }
#'
#' @export
pm_tile <- function(input, z, x, y, output = NULL, bucket = NULL) {

  # Validate inputs
  if (!is.numeric(z) || !is.numeric(x) || !is.numeric(y)) {
    stop("z, x, and y must be numeric values", call. = FALSE)
  }

  # Build command arguments
  args <- c("tile", input, as.character(z), as.character(x), as.character(y))

  if (!is.null(bucket)) {
    args <- c(args, paste0("--bucket=", bucket))
  }

  # Execute command - capture binary output
  if (is.null(output)) {
    # Use temp file to capture binary data
    tmpfile <- tempfile(fileext = ".tile")
    on.exit(unlink(tmpfile), add = TRUE)

    result <- pmtiles_exec(args, stdout = tmpfile)

    # Read binary data
    raw_data <- readBin(tmpfile, "raw", n = file.info(tmpfile)$size)
    return(raw_data)
  } else {
    # Write directly to file
    result <- pmtiles_exec(args, stdout = output)
    message("Tile written to: ", output)
    return(invisible(output))
  }
}
