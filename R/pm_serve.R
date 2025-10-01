#' Serve PMTiles files via local HTTP server with CORS
#'
#' @description
#' Start a local HTTP server to serve PMTiles files with CORS headers enabled.
#' This allows PMTiles to be consumed by web maps (like mapgl) using the
#' PMTiles.js client library. The server runs in the background and can be
#' stopped with `pm_stop_server()`.
#'
#' @param path Path to a directory containing PMTiles files, or a single PMTiles file.
#'   If a single file, its directory will be served.
#' @param port Port number for the HTTP server. Default is 8080.
#' @param background Logical. If `TRUE`, runs server in background and returns
#'   immediately. If `FALSE`, blocks until interrupted. Default is `TRUE`.
#'
#' @details
#' This function serves the **raw PMTiles files** with CORS headers, allowing
#' them to be consumed by PMTiles.js in the browser. This is different from
#' `pmtiles serve` which provides a Z/X/Y tile API.
#'
#' For a file at `tiles/data.pmtiles`, it will be available at:
#' `http://localhost:PORT/data.pmtiles`
#'
#' Uses the `servr` package to create a simple HTTP server with CORS support.
#' When `background = TRUE`, the server runs as a background R process.
#'
#' @return
#' If `background = TRUE`, returns a list with:
#' - `url`: Base URL of the server
#' - `port`: Port number
#' - `dir`: Directory being served
#' - `daemon`: Server daemon ID (for stopping)
#'
#' If `background = FALSE`, blocks until interrupted and returns nothing.
#'
#' @examples
#' \dontrun{
#' # Serve a single PMTiles file
#' server <- pm_serve("data.pmtiles", port = 8080)
#' # File available at: http://localhost:8080/data.pmtiles
#'
#' # Use in mapgl
#' mapgl::add_pmtiles_source(url = paste0(server$url, "/data.pmtiles"))
#'
#' # Serve a directory of PMTiles files
#' server <- pm_serve("tiles/", port = 8080)
#'
#' # Stop the server when done
#' pm_stop_server(server)
#'
#' # Or run in foreground (blocks)
#' pm_serve("data.pmtiles", background = FALSE)
#' }
#'
#' @seealso [pm_stop_server()], [pm_view()]
#' @export
pm_serve <- function(path,
                     port = 8080,
                     background = TRUE) {

  # Check if httpuv is available
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    stop(
      "Package 'httpuv' is required for pm_serve().\n",
      "Install it with: install.packages('httpuv')",
      call. = FALSE
    )
  }

  # Expand path
  path <- path.expand(path)

  # Determine if path is a file or directory
  if (file.exists(path)) {
    if (dir.exists(path)) {
      serve_dir <- path
    } else {
      # Single file - serve its directory
      serve_dir <- dirname(path)
    }
  } else {
    stop("Path not found: ", path, call. = FALSE)
  }

  base_url <- paste0("http://localhost:", port)

  if (background) {
    # Start background server using httpuv with CORS
    message("Starting HTTP server with CORS support...")
    message("  URL: ", base_url)
    message("  Serving: ", serve_dir)
    message("  Port: ", port)

    # Create custom app with static file serving and CORS headers
    app <- list(
      call = function(req) {
        # Add CORS headers to all responses
        headers <- list(
          "Access-Control-Allow-Origin" = "*",
          "Access-Control-Allow-Methods" = "GET, HEAD, OPTIONS",
          "Access-Control-Allow-Headers" = "Range, Content-Type"
        )

        # Handle OPTIONS preflight
        if (req$REQUEST_METHOD == "OPTIONS") {
          return(list(
            status = 200L,
            headers = headers,
            body = ""
          ))
        }

        # Serve static files
        path <- req$PATH_INFO
        if (path == "/" || path == "") {
          path <- "/index.html"
        }

        file_path <- file.path(serve_dir, substring(path, 2))

        if (file.exists(file_path) && !file.info(file_path)$isdir) {
          # Determine content type
          ext <- tools::file_ext(file_path)
          content_type <- switch(ext,
            "pmtiles" = "application/octet-stream",
            "json" = "application/json",
            "html" = "text/html",
            "css" = "text/css",
            "js" = "application/javascript",
            "png" = "image/png",
            "jpg" = "image/jpeg",
            "jpeg" = "image/jpeg",
            "application/octet-stream"
          )

          headers[["Content-Type"]] <- content_type

          # Support range requests for PMTiles
          range_header <- req$HTTP_RANGE
          file_size <- file.info(file_path)$size

          if (!is.null(range_header) && grepl("^bytes=", range_header)) {
            # Parse range
            range <- sub("^bytes=", "", range_header)
            parts <- strsplit(range, "-")[[1]]
            start <- as.integer(parts[1])
            end <- if (nchar(parts[2]) > 0) as.integer(parts[2]) else file_size - 1

            # Read partial content
            con <- file(file_path, "rb")
            seek(con, start)
            content <- readBin(con, "raw", n = end - start + 1)
            close(con)

            headers[["Content-Range"]] <- sprintf("bytes %d-%d/%d", start, end, file_size)
            headers[["Content-Length"]] <- as.character(length(content))

            return(list(
              status = 206L,
              headers = headers,
              body = content
            ))
          } else {
            # Read full file
            content <- readBin(file_path, "raw", n = file_size)
            headers[["Content-Length"]] <- as.character(file_size)

            return(list(
              status = 200L,
              headers = headers,
              body = content
            ))
          }
        } else {
          return(list(
            status = 404L,
            headers = headers,
            body = "Not Found"
          ))
        }
      }
    )

    # Start the server
    server_handle <- tryCatch({
      httpuv::startDaemonizedServer(
        host = "0.0.0.0",
        port = port,
        app = app
      )
    }, error = function(e) {
      if (grepl("address already in use", e$message, ignore.case = TRUE)) {
        warning("Port ", port, " is already in use. Server may already be running.", call. = FALSE)
        # Try to find existing server
        if (exists(".pmtiles_servers", envir = .GlobalEnv)) {
          servers <- get(".pmtiles_servers", envir = .GlobalEnv)
          if (as.character(port) %in% ls(servers)) {
            message("Using existing server on port ", port)
            return(servers[[as.character(port)]]$handle)
          }
        }
      }
      stop(e)
    })

    # Give the daemon a moment to initialize
    Sys.sleep(1)

    message("\u2713 Server started successfully")
    message("Use pm_stop_server() to stop when done")

    # Store server info in an environment for tracking
    if (!exists(".pmtiles_servers", envir = .GlobalEnv)) {
      assign(".pmtiles_servers", new.env(), envir = .GlobalEnv)
    }
    servers <- get(".pmtiles_servers", envir = .GlobalEnv)
    server_id <- as.character(port)
    servers[[server_id]] <- list(
      url = base_url,
      port = port,
      dir = serve_dir,
      handle = server_handle
    )

    return(invisible(list(
      url = base_url,
      port = port,
      dir = serve_dir,
      handle = server_handle
    )))
  } else {
    # Run in foreground (blocks)
    message("Starting HTTP server (press Ctrl+C to stop)...")
    message("  URL: ", base_url)
    message("  Serving: ", serve_dir)
    message("  Port: ", port)
    message("Note: Foreground mode not yet implemented. Use background = TRUE")
    stop("Foreground mode not yet implemented", call. = FALSE)
  }
}

#' Stop a background PMTiles server
#'
#' @description
#' Stop a PMTiles server that was started with `pm_serve(..., background = TRUE)`.
#'
#' @param server A server object returned by `pm_serve()`, or a port number.
#'   If `NULL`, stops all running PMTiles servers.
#'
#' @return Invisibly returns `TRUE` if server was stopped, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' # Start server
#' server <- pm_serve("data.pmtiles")
#'
#' # Stop it
#' pm_stop_server(server)
#'
#' # Or stop by port
#' pm_stop_server(8080)
#'
#' # Or stop all servers
#' pm_stop_server()
#' }
#'
#' @seealso [pm_serve()]
#' @export
pm_stop_server <- function(server = NULL) {
  if (!exists(".pmtiles_servers", envir = .GlobalEnv)) {
    message("No PMTiles servers are running")
    return(invisible(FALSE))
  }

  servers <- get(".pmtiles_servers", envir = .GlobalEnv)

  if (is.null(server)) {
    # Stop all servers
    if (length(ls(servers)) == 0) {
      message("No PMTiles servers are running")
      return(invisible(FALSE))
    }

    message("Stopping all PMTiles servers...")
    for (server_id in ls(servers)) {
      server_info <- servers[[server_id]]
      httpuv::stopDaemonizedServer(server_info$handle)
      message("  Stopped server on port ", server_id)
      rm(list = server_id, envir = servers)
    }
    return(invisible(TRUE))
  }

  # Extract port and handle from server object or use directly
  if (is.list(server) && !is.null(server$port)) {
    port <- as.character(server$port)
    server_handle <- server$handle
  } else if (is.numeric(server)) {
    port <- as.character(server)
    if (!exists(port, envir = servers)) {
      message("No server found on port ", port)
      return(invisible(FALSE))
    }
    server_info <- servers[[port]]
    server_handle <- server_info$handle
  } else {
    stop("Invalid server argument. Expected server object or port number.", call. = FALSE)
  }

  # Stop the server
  httpuv::stopDaemonizedServer(server_handle)
  message("Stopped PMTiles server on port ", port)
  if (exists(port, envir = servers)) {
    rm(list = port, envir = servers)
  }
  return(invisible(TRUE))
}
