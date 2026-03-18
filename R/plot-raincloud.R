#' Raincloud plot for hrest objects
#'
#' Creates a unified raincloud visualization combining:
#' \itemize{
#'   \item Jittered raw data points
#'   \item Half-violin density plots
#'   \item Mean with confidence interval as pointrange
#' }
#'
#' This provides an alternative to the standard two-panel besthr plot,
#' combining all information in a single comprehensive visualization.
#'
#' @param hrest An hrest object from \code{\link{estimate}}
#' @param theme the visual theme to use. Either "modern" (default) or "classic"
#' @param colors the color palette to use. Either "okabe_ito" (default), "default", or "viridis"
#' @param config an optional besthr_plot_config object for advanced customization.
#'   If provided, theme and colors parameters are ignored.
#' @param show_bootstrap Ignored (kept for backward compatibility).
#' @param jitter_width Numeric width of jitter for data points. Default 0.15.
#' @param point_size Numeric size for jittered points. Default 1.5.
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#' \donttest{
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' plot_raincloud(hr)
#' }
#'
plot_raincloud <- function(hrest, theme = "modern", colors = "okabe_ito",
                           config = NULL, show_bootstrap = TRUE,
                           jitter_width = 0.15, point_size = 1.5) {
  if (!inherits(hrest, "hrest")) {
    stop("hrest must be an hrest object from estimate()")
  }

  # Build config from parameters or use provided config
  if (is.null(config)) {
    config <- besthr_plot_config(
      theme_style = theme,
      color_palette = colors
    )
  }

  # Create unified data view
  data_view <- besthr_data_view(hrest, config)
  group_col <- data_view$group_col

  # Base plot with ranked data
  p <- ggplot2::ggplot(data_view$ranked, ggplot2::aes(x = !!group_col, y = rank))

  # Add jittered data points
  p <- p + ggplot2::geom_jitter(
    ggplot2::aes(colour = !!group_col),
    width = jitter_width,
    height = 0,
    size = point_size,
    alpha = config$point_alpha
  )

  # Add mean with CI as pointrange
  ci_data <- data_view$ci

  p <- p + ggplot2::geom_pointrange(
    data = ci_data,
    mapping = ggplot2::aes(
      x = !!group_col,
      y = mean,
      ymin = low,
      ymax = high,
      colour = !!group_col
    ),
    size = 0.8,
    linewidth = 1.2,
    position = ggplot2::position_nudge(x = 0.3)
  )

  # Add group mean lines
  p <- p + ggplot2::geom_hline(
    data = data_view$group_means,
    mapping = ggplot2::aes(yintercept = mean, colour = !!group_col),
    linetype = config$mean_line_type,
    linewidth = config$mean_line_width * 0.7,
    alpha = 0.5
  )

  # Apply y-axis limits
  p <- p + ggplot2::ylim(data_view$rank_limits)

  # Labels
  p <- p + ggplot2::labs(
    x = NULL,
    y = "Rank"
  )

  # Apply theme
  p <- p + theme_besthr(config$theme_style)

  # Apply color palette if not default
  if (config$color_palette != "default") {
    p <- p +
      scale_color_besthr(config$color_palette) +
      scale_fill_besthr(config$color_palette)
  }

  # Remove redundant legend
  p <- p + ggplot2::guides(
    colour = "none",
    fill = "none"
  )

  p
}

#' Raincloud plot showing bootstrap distributions
#'
#' Creates a raincloud plot specifically for bootstrap distributions, showing
#' the distribution of bootstrap mean ranks with jittered points and summary
#' statistics.
#'
#' @param hrest An hrest object from \code{\link{estimate}}
#' @param theme the visual theme to use. Either "modern" (default) or "classic"
#' @param colors the color palette to use. Either "okabe_ito" (default), "default", or "viridis"
#' @param config an optional besthr_plot_config object
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#' \donttest{
#' d <- make_data()
#' hr <- estimate(d, score, group, nits = 100)
#' plot_bootstrap_raincloud(hr)
#' }
#'
plot_bootstrap_raincloud <- function(hrest, theme = "modern", colors = "okabe_ito",
                                     config = NULL) {
  if (!inherits(hrest, "hrest")) {
    stop("hrest must be an hrest object from estimate()")
  }

  # Build config from parameters or use provided config
  if (is.null(config)) {
    config <- besthr_plot_config(
      theme_style = theme,
      color_palette = colors
    )
  }

  # Create unified data view
  data_view <- besthr_data_view(hrest, config)
  group_col <- data_view$group_col

  # Filter bootstrap data to exclude control group
  bootstrap_data <- data_view$bootstrap

  # Build plot with jittered bootstrap means
  p <- ggplot2::ggplot(bootstrap_data, ggplot2::aes(x = !!group_col, y = mean))

  # Add jittered bootstrap points
  p <- p + ggplot2::geom_jitter(
    ggplot2::aes(colour = !!group_col),
    width = 0.15,
    height = 0,
    size = 0.5,
    alpha = 0.3
  )

  # Add CI as pointrange
  ci_data <- data_view$ci
  p <- p + ggplot2::geom_pointrange(
    data = ci_data,
    mapping = ggplot2::aes(
      x = !!group_col,
      y = mean,
      ymin = low,
      ymax = high,
      colour = !!group_col
    ),
    size = 1,
    linewidth = 1.5,
    position = ggplot2::position_nudge(x = 0.25)
  )

  # Y-axis limits (mean rank values)
  p <- p + ggplot2::ylim(data_view$rank_limits)

  # Labels
  low_pct <- data_view$quantiles["low"] * 100
  high_pct <- data_view$quantiles["high"] * 100
  p <- p + ggplot2::labs(
    x = NULL,
    y = "Bootstrap Mean Rank",
    caption = paste0(low_pct, "% - ", high_pct, "% CI")
  )

  # Apply theme
  p <- p + theme_besthr(config$theme_style)

  # Apply color palette if not default
  if (config$color_palette != "default") {
    p <- p +
      scale_color_besthr(config$color_palette) +
      scale_fill_besthr(config$color_palette)
  }

  # Remove redundant legend
  p <- p + ggplot2::guides(fill = "none", colour = "none")

  p
}
