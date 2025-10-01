#' Edit PMTiles archive header or metadata
#'
#' @description
#' Modify parts of the PMTiles archive header or replace the JSON metadata.
#' Editing only the header modifies the file in-place, while editing metadata
#' creates a new copy.
#'
#' @param input Path to PMTiles file to edit.
#' @param header_json Path to JSON file containing modified header data.
#'   Use `pm_show(input, header_json = TRUE)` to get current header.
#' @param metadata Path to JSON file containing replacement metadata.
#'   Use `pm_show(input, metadata = TRUE)` to get current metadata.
#' @param verbose Logical. If `TRUE`, prints progress information. Default is `TRUE`.
#'
#' @details
#' # Editable Header Fields
#'
#' The following header fields can be modified:
#' - `tile_type`: Type of tiles (e.g., "mvt", "png", "jpg")
#' - `tile_compression`: Compression format
#' - `minzoom`: Minimum zoom level
#' - `maxzoom`: Maximum zoom level
#' - `bounds`: Geographic bounds
#' - `center`: Center point and zoom
#'
#' Other header fields are not editable.
#'
#' # Important Notes
#'
#' - Editing only the header modifies the file in-place
#' - Writing new metadata requires creating a new archive copy
#' - The new copy will replace the original file
#'
#' @return Invisibly returns the path to the edited archive.
#'
#' @examples
#' \dontrun{
#' # Get current header and metadata
#' header <- pm_show("archive.pmtiles", header_json = TRUE)
#' metadata <- pm_show("archive.pmtiles", metadata = TRUE)
#'
#' # Modify and save to JSON files
#' jsonlite::write_json(header, "header.json", auto_unbox = TRUE)
#' jsonlite::write_json(metadata, "metadata.json", auto_unbox = TRUE)
#'
#' # Edit the archive
#' pm_edit("archive.pmtiles",
#'         header_json = "header.json",
#'         metadata = "metadata.json")
#' }
#'
#' @export
pm_edit <- function(input,
                    header_json = NULL,
                    metadata = NULL,
                    verbose = TRUE) {

  # Validate inputs
  if (!file.exists(input)) {
    stop("Input file not found: ", input, call. = FALSE)
  }

  if (is.null(header_json) && is.null(metadata)) {
    stop("Must specify at least one of header_json or metadata", call. = FALSE)
  }

  if (!is.null(header_json) && !file.exists(header_json)) {
    stop("Header JSON file not found: ", header_json, call. = FALSE)
  }

  if (!is.null(metadata) && !file.exists(metadata)) {
    stop("Metadata JSON file not found: ", metadata, call. = FALSE)
  }

  # Build command arguments
  args <- c("edit", input)

  if (!is.null(header_json)) {
    args <- c(args, paste0("--header-json=", header_json))
  }

  if (!is.null(metadata)) {
    args <- c(args, paste0("--metadata=", metadata))
  }

  # Execute command
  if (verbose) {
    message("Editing ", input, "...")
    if (!is.null(metadata)) {
      message("Note: Writing metadata will create a new copy of the archive")
    }
  }

  result <- pmtiles_exec(args)

  if (verbose) {
    cat(result$stdout)
  }

  if (result$status == 0) {
    message("\u2713 Successfully edited: ", input)
  }

  return(invisible(input))
}
