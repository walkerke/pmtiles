#' Serve PMTiles as Z/X/Y tile endpoints
#'
#' @description
#' Start a local tile server that serves PMTiles archives as standard Z/X/Y tile
#' endpoints (e.g., `/\{tileset\}/\{z\}/\{x\}/\{y\}.mvt`). This uses the native
#' `pmtiles serve` command and works with any map client, not just PMTiles.js.
#'
#' Unlike `pm_serve()` which serves raw .pmtiles files for direct consumption by
#' PMTiles.js, this function extracts individual tiles on-demand, making the tiles
#' accessible to any mapping library that supports standard tile URLs.
#'
#' @param path Directory containing PMTiles files, or a specific path prefix.
#'   Default is current directory (`"."`).
#' @param bucket Optional cloud storage bucket specification (e.g., `"s3://bucket-name"`
#'   or `"s3://bucket?endpoint=https://account.r2.cloudflarestorage.com&region=auto"`).
#'   When specified, serves tiles directly from cloud storage without downloading.
#' @param port Port number for the tile server. Default is 8080.
#' @param cors CORS origins to allow. Can be `"*"` for all origins, a character
#'   vector of specific origins, or `NULL` for no CORS headers. Default is `"*"`.
#' @param cache_size Cache size in megabytes. Default is 64 MB.
#' @param public_url Public-facing URL for TileJSON generation (e.g.,
#'   `"https://example.com"`). Required for accurate TileJSON metadata.
#' @param background Logical. If `TRUE`, runs server in background using `processx`.
#'   If `FALSE` (default), runs in foreground (blocking).
#'
#' @details
#' # Tile Endpoints
#'
#' The server provides these endpoints:
#'
#' - **Tiles**: `http://localhost:PORT/TILESET/\{z\}/\{x\}/\{y\}.ext`
#'   - Extension (`.mvt`, `.png`, `.jpg`, etc.) is auto-detected from PMTiles metadata
#' - **TileJSON**: `http://localhost:PORT/TILESET.json` (requires `public_url`)
#'   - Returns TileJSON metadata for the tileset
#'
#' This approach works with any map client and is particularly useful for:
#' - Large PMTiles files (multi-GB)
#' - Serving directly from cloud storage
#' - Clients that don't support the PMTiles protocol
#'
#' # Cloud Storage
#'
#' You can serve tiles directly from cloud storage without downloading:
#'
#' **Cloudflare R2:**
#' ```r
#' pm_serve_zxy(
#'   bucket = "s3://my-bucket?endpoint=https://account.r2.cloudflarestorage.com&region=auto"
#' )
#' ```
#'
#' **AWS S3:**
#' ```r
#' pm_serve_zxy(bucket = "s3://my-bucket")
#' ```
#'
#' Requires appropriate environment variables for authentication
#' (`AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`).
#'
#' # Background Mode
#'
#' When `background = TRUE`, the server runs in a background process. Use
#' `pm_stop_server()` to stop background servers.
#'
#' # Comparison with pm_serve()
#'
#' - `pm_serve()`: Serves raw .pmtiles files for PMTiles.js (HTTP Range requests)
#' - `pm_serve_zxy()`: Serves individual Z/X/Y tiles for any map client
#'
#' Use `pm_serve_zxy()` when you need standard tile URLs or want to serve from
#' cloud storage. Use `pm_serve()` for quick local preview with PMTiles.js.
#'
#' @return If `background = FALSE`, blocks until server is stopped (Ctrl+C).
#'   If `background = TRUE`, invisibly returns the `processx::process` object.
#'
#' @examples
#' \dontrun{
#' # Serve all PMTiles in current directory
#' pm_serve_zxy()
#'
#' # Serve specific directory on custom port
#' pm_serve_zxy(path = "~/pmtiles", port = 9000)
#'
#' # Serve from Cloudflare R2
#' pm_serve_zxy(
#'   bucket = "s3://my-bucket?endpoint=https://account.r2.cloudflarestorage.com&region=auto",
#'   public_url = "https://tiles.example.com"
#' )
#'
#' # Run in background
#' server <- pm_serve_zxy(background = TRUE)
#' # ... do other work ...
#' pm_stop_server(server)
#' }
#'
#' @export
pm_serve_zxy <- function(path = ".",
                         bucket = NULL,
                         port = 8080,
                         cors = "*",
                         cache_size = 64,
                         public_url = NULL,
                         background = FALSE) {

  # Check for pmtiles binary
  pmtiles_path <- pmtiles_binary()

  # Build command arguments
  args <- c("serve")

  # Add path or bucket
  if (!is.null(bucket)) {
    # Bucket specified explicitly
    args <- c(args, path, paste0("--bucket=", bucket))
  } else {
    # Check if path is a URL
    if (grepl("^https?://", path)) {
      # Parse URL to extract base and path
      # Extract everything up to the last /
      url_parts <- regmatches(path, regexpr("^(https?://[^/]+)(/.*)$", path, perl = TRUE))
      if (length(url_parts) == 0) {
        # Just a domain, no path
        args <- c(args, "/", paste0("--bucket=", path))
      } else {
        # Extract base URL and path
        base_url <- sub("^(https?://[^/]+).*$", "\\1", path)
        url_path <- sub("^https?://[^/]+(.*)$", "\\1", path)
        # Remove .pmtiles extension from path for serving
        url_path <- dirname(url_path)
        if (url_path == ".") url_path <- "/"

        args <- c(args, url_path, paste0("--bucket=", base_url))
      }
    } else {
      # It's a local path - expand ~ and normalize
      path <- normalizePath(path.expand(path), mustWork = FALSE)
      args <- c(args, path)
    }
  }

  # Add port
  args <- c(args, paste0("--port=", as.integer(port)))

  # Add CORS
  if (!is.null(cors)) {
    if (length(cors) == 1 && cors == "*") {
      args <- c(args, "--cors=*")
    } else {
      # Join multiple origins with commas
      args <- c(args, paste0("--cors=", paste(cors, collapse = ",")))
    }
  }

  # Add cache size
  args <- c(args, paste0("--cache-size=", as.integer(cache_size)))

  # Add public URL if specified
  if (!is.null(public_url)) {
    args <- c(args, paste0("--public-url=", public_url))
  }

  # Display startup message
  if (!is.null(bucket)) {
    message("Starting PMTiles Z/X/Y tile server from cloud storage...")
    message("  Bucket: ", bucket)
  } else {
    message("Starting PMTiles Z/X/Y tile server...")
    message("  Path: ", path)
  }
  message("  Port: ", port)
  message("  Tiles: http://localhost:", port, "/{tileset}/{z}/{x}/{y}")
  message("  TileJSON: http://localhost:", port, "/{tileset}.json")

  if (background) {
    # Run in background with processx
    message("  Running in background mode (use pm_stop_serve_zxy() to stop)")

    proc <- processx::process$new(
      command = pmtiles_path,
      args = args,
      stdout = "|",
      stderr = "|"
    )

    # Store process in package environment for tracking
    if (!exists("zxy_servers", envir = .GlobalEnv)) {
      assign("zxy_servers", new.env(parent = emptyenv()), envir = .GlobalEnv)
    }
    zxy_servers <- get("zxy_servers", envir = .GlobalEnv)
    zxy_servers[[as.character(port)]] <- proc

    return(invisible(proc))

  } else {
    # Run in foreground (blocking)
    message("  Press Ctrl+C to stop server")

    tryCatch({
      processx::run(
        command = pmtiles_path,
        args = args,
        echo = TRUE,
        error_on_status = FALSE
      )
    }, interrupt = function(e) {
      message("\nServer stopped")
    })

    return(invisible(NULL))
  }
}
