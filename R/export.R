#' Save besthr plot to file
#'
#' Saves a besthr visualization to a file with sensible publication defaults.
#' Supports PNG, PDF, SVG, and TIFF formats.
#'
#' @param hrest An hrest object from \code{\link{estimate}}
#' @param filename Output filename. Format is detected from extension.
#' @param type Plot type: "default" (two-panel) or "raincloud"
#' @param width Plot width in inches (default 8)
#' @param height Plot height in inches (default 6)
#' @param dpi Resolution in dots per inch (default 300)
#' @param ... Additional arguments passed to the plot function (e.g., theme, colors)
#'
#' @return The filename (invisibly)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' save_besthr(hr, "figure1.png")
#' save_besthr(hr, "figure1.pdf", width = 10, height = 8)
#' save_besthr(hr, "figure1.png", type = "raincloud")
#' }
#'
save_besthr <- function(hrest, filename, type = "default",
                        width = 8, height = 6, dpi = 300, ...) {
  if (!inherits(hrest, "hrest")) {
    stop("hrest must be an hrest object from estimate()")
  }

  # Detect format from extension
  ext <- tolower(tools::file_ext(filename))
  valid_formats <- c("png", "pdf", "svg", "tiff", "tif", "jpeg", "jpg")
  if (!ext %in% valid_formats) {
    stop("Unsupported format: ", ext,
         ". Supported formats: ", paste(valid_formats, collapse = ", "))
  }

  # Generate the plot
  p <- switch(type,
    default = plot(hrest, ...),
    raincloud = plot_raincloud(hrest, ...),
    stop("Unknown plot type: ", type, ". Choose 'default' or 'raincloud'.")
  )

  # Save using ggplot2::ggsave
  ggplot2::ggsave(
    filename = filename,
    plot = p,
    width = width,
    height = height,
    dpi = dpi
  )

  invisible(filename)
}
