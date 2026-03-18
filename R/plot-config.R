#' Create a besthr plot configuration
#'
#' Creates a configuration object that controls the appearance and behavior
#' of besthr plots. All parameters have defaults that reproduce the original
#' besthr appearance for backward compatibility.
#'
#' @param panel_widths Numeric vector of relative panel widths for patchwork layout.
#' @param panel_spacing A grid unit specifying spacing between panels.
#' @param y_limits Numeric vector of length 2 for y-axis limits, or NULL for auto.
#' @param y_expand Numeric giving proportional expansion of y-axis limits.
#' @param point_size_range Numeric vector of length 2 for min/max point sizes.
#' @param point_alpha Numeric between 0 and 1 for point transparency.
#' @param mean_line_type Line type for mean indicator lines.
#' @param mean_line_width Line width for mean indicator lines.
#' @param density_alpha Numeric between 0 and 1 for density plot transparency.
#' @param density_style Character: "points" (default, jittered bootstrap points),
#'   "gradient" (density with CI shading), or "solid" (single color density).
#' @param theme_style Character: "classic" or "modern".
#' @param color_palette Character: "default", "okabe_ito", or "viridis".
#'
#' @return An object of class "besthr_plot_config" containing all plot settings.
#'
#' @export
#'
#' @examples
#' cfg <- besthr_plot_config()
#' cfg <- besthr_plot_config(panel_widths = c(2, 1), theme_style = "modern")
#'
besthr_plot_config <- function(
    panel_widths = c(1, 1),
    panel_spacing = grid::unit(0.5, "cm"),
    y_limits = NULL,
    y_expand = 0.05,
    point_size_range = c(2, 8),
    point_alpha = 0.8,
    mean_line_type = 3,
    mean_line_width = 1,
    density_alpha = 0.7,
    density_style = "points",
    theme_style = "modern",
    color_palette = "okabe_ito"
) {
  if (!is.numeric(panel_widths) || length(panel_widths) < 1) {
    stop("panel_widths must be a numeric vector")
  }

  if (!is.null(y_limits) && (!is.numeric(y_limits) || length(y_limits) != 2)) {
    stop("y_limits must be NULL or a numeric vector of length 2")
  }

  if (!is.numeric(y_expand) || length(y_expand) != 1 || y_expand < 0) {
    stop("y_expand must be a non-negative number")
  }

  if (!is.numeric(point_size_range) || length(point_size_range) != 2) {
    stop("point_size_range must be a numeric vector of length 2")
  }

  if (!is.numeric(point_alpha) || point_alpha < 0 || point_alpha > 1) {
    stop("point_alpha must be a number between 0 and 1")
  }

  if (!is.numeric(density_alpha) || density_alpha < 0 || density_alpha > 1) {
    stop("density_alpha must be a number between 0 and 1")
  }

  if (!density_style %in% c("gradient", "solid", "points")) {
    stop("density_style must be 'gradient', 'solid', or 'points'")
  }

  if (!theme_style %in% c("classic", "modern")) {
    stop("theme_style must be 'classic' or 'modern'")
  }

  if (!color_palette %in% c("default", "okabe_ito", "viridis")) {
    stop("color_palette must be 'default', 'okabe_ito', or 'viridis'")
  }

  structure(
    list(
      panel_widths = panel_widths,
      panel_spacing = panel_spacing,
      y_limits = y_limits,
      y_expand = y_expand,
      point_size_range = point_size_range,
      point_alpha = point_alpha,
      mean_line_type = mean_line_type,
      mean_line_width = mean_line_width,
      density_alpha = density_alpha,
      density_style = density_style,
      theme_style = theme_style,
      color_palette = color_palette
    ),
    class = "besthr_plot_config"
  )
}

#' Print method for besthr_plot_config
#'
#' @param x A besthr_plot_config object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#' @export
#'
print.besthr_plot_config <- function(x, ...) {
  cat("besthr plot configuration:\n")
  cat("  Panel widths:", paste(x$panel_widths, collapse = ", "), "\n")
  cat("  Y-axis limits:", if (is.null(x$y_limits)) "auto" else paste(x$y_limits, collapse = " - "), "\n")
  cat("  Y-axis expand:", x$y_expand, "\n")
  cat("  Point size range:", paste(x$point_size_range, collapse = " - "), "\n")
  cat("  Point alpha:", x$point_alpha, "\n")
  cat("  Mean line type:", x$mean_line_type, "\n")
  cat("  Mean line width:", x$mean_line_width, "\n")
  cat("  Density alpha:", x$density_alpha, "\n")
  cat("  Density style:", x$density_style, "\n")
  cat("  Theme:", x$theme_style, "\n")
  cat("  Colors:", x$color_palette, "\n")
  invisible(x)
}

#' Update a besthr plot configuration
#'
#' Creates a new configuration by updating specific fields of an existing one.
#'
#' @param config An existing besthr_plot_config object
#' @param ... Named arguments to update
#'
#' @return A new besthr_plot_config object
#' @export
#'
#' @examples
#' cfg <- besthr_plot_config()
#' cfg2 <- update_config(cfg, theme_style = "modern", panel_widths = c(2, 1))
#'
update_config <- function(config, ...) {
  if (!inherits(config, "besthr_plot_config")) {
    stop("config must be a besthr_plot_config object")
  }

  updates <- list(...)
  valid_names <- names(config)

  for (name in names(updates)) {
    if (!name %in% valid_names) {
      stop("Unknown config parameter: ", name)
    }
  }

  args <- as.list(config)
  args[names(updates)] <- updates
  do.call(besthr_plot_config, args)
}
