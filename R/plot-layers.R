#' Create a unified data view for besthr plotting
#'
#' Extracts and organizes data from an hrest object for plotting. Computes
#' unified axis limits that ensure alignment between observation and bootstrap
#' panels.
#'
#' @param hrest An hrest object from \code{\link{estimate}}
#' @param config A besthr_plot_config object (optional). If NULL, uses defaults.
#'
#' @return An object of class "besthr_data_view" containing ranked data,
#'   original data, bootstrap samples, group means, confidence intervals,
#'   group sample sizes, unified rank limits, group column symbol, column info,
#'   control group name, and quantile values.
#'
#' @export
#'
#' @examples
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' dv <- besthr_data_view(hr)
#'
besthr_data_view <- function(hrest, config = NULL) {
  if (!inherits(hrest, "hrest")) {
    stop("hrest must be an hrest object from estimate()")
  }

  if (is.null(config)) {
    config <- besthr_plot_config()
  }

  # Extract group column name
  group_col_name <- names(hrest$group_n)[names(hrest$group_n) != "n"][[1]]
  group_col <- rlang::sym(group_col_name)

  # Compute unified rank limits from ranked data and bootstrap means only
  # (original_data scores may have different range than ranks)
  all_ranks <- c(
    hrest$ranked_data$rank,
    hrest$bootstraps$mean
  )
  rank_min <- min(all_ranks, na.rm = TRUE)
  rank_max <- max(all_ranks, na.rm = TRUE)

  # Apply expansion (minimal padding to avoid excessive whitespace)
  if (!is.null(config$y_limits)) {
    rank_limits <- config$y_limits
  } else {
    padding <- (rank_max - rank_min) * config$y_expand
    rank_limits <- c(max(0, rank_min - padding), rank_max + padding)
  }

  structure(
    list(
      ranked = hrest$ranked_data,
      original = hrest$original_data,
      bootstrap = hrest$bootstraps,
      group_means = hrest$group_means,
      ci = hrest$ci,
      group_n = hrest$group_n,
      rank_limits = rank_limits,
      group_col = group_col,
      group_col_name = group_col_name,
      column_info = hrest$column_info,
      control = hrest$control,
      quantiles = c(low = hrest$low, high = hrest$high)
    ),
    class = "besthr_data_view"
  )
}

#' Print method for besthr_data_view
#'
#' @param x A besthr_data_view object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#' @export
#'
print.besthr_data_view <- function(x, ...) {
  cat("besthr data view:\n")
  cat("  Groups:", nrow(x$group_n), "\n")
  cat("  Observations:", nrow(x$ranked), "\n")
  cat("  Bootstrap iterations:", nrow(x$bootstrap) / (nrow(x$group_n) - 1), "\n")
  cat("  Rank limits:", paste(round(x$rank_limits, 2), collapse = " - "), "\n")
  cat("  Control:", x$control, "\n")
  invisible(x)
}

#' Create ranked dots layer
#'
#' Creates a ggplot2 layer showing ranked observations as points, where point
#' size indicates the count of observations at each rank/group combination.
#'
#' @param data_view A besthr_data_view object
#' @param config A besthr_plot_config object
#'
#' @return A list of ggplot2 layers
#'
#' @export
#'
#' @examples
#' \donttest{
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' dv <- besthr_data_view(hr)
#' cfg <- besthr_plot_config()
#' # layer_ranked_dots returns layers to add to a ggplot
#' }
#'
layer_ranked_dots <- function(data_view, config) {
  group_col <- data_view$group_col

  # Compute count summary
  dot_data <- data_view$ranked %>%
    dplyr::group_by(!!group_col, rank) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  list(
    ggplot2::geom_point(
      data = dot_data,
      mapping = ggplot2::aes(
        x = !!group_col,
        y = rank,
        size = count,
        colour = !!group_col,
        fill = !!group_col
      ),
      alpha = config$point_alpha
    ),
    ggplot2::scale_size_continuous(range = config$point_size_range)
  )
}

#' Create group mean lines layer
#'
#' Creates a ggplot2 layer showing horizontal lines at group mean ranks.
#'
#' @param data_view A besthr_data_view object
#' @param config A besthr_plot_config object
#'
#' @return A ggplot2 layer
#'
#' @export
#'
#' @examples
#' \donttest{
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' dv <- besthr_data_view(hr)
#' cfg <- besthr_plot_config()
#' # layer_group_means returns a ggplot layer
#' }
#'
layer_group_means <- function(data_view, config) {
  group_col <- data_view$group_col

  ggplot2::geom_hline(
    data = data_view$group_means,
    mapping = ggplot2::aes(yintercept = mean, colour = !!group_col),
    linetype = config$mean_line_type,
    linewidth = config$mean_line_width
  )
}

#' Create bootstrap density layer
#'
#' Creates a ggplot2 layer showing ridge density plots of bootstrap distributions
#' with confidence interval shading.
#'
#' @param data_view A besthr_data_view object
#' @param config A besthr_plot_config object
#'
#' @return A list of ggplot2 layers
#'
#' @export
#' @importFrom ggplot2 after_stat
#'
#' @examples
#' \donttest{
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' dv <- besthr_data_view(hr)
#' cfg <- besthr_plot_config()
#' # layer_bootstrap_density returns layers to add to a ggplot
#' }
#'
layer_bootstrap_density <- function(data_view, config) {
  group_col <- data_view$group_col
  ci_colors <- derive_ci_colors(config$color_palette, config$theme_style)

  low <- data_view$quantiles["low"]
  high <- data_view$quantiles["high"]

  list(
    ggplot2::aes(x = mean, y = !!group_col, fill = factor(after_stat(quantile))),
    ggridges::stat_density_ridges(
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE,
      quantiles = c(low, high),
      alpha = config$density_alpha
    ),
    ggplot2::scale_fill_manual(
      values = ci_colors,
      name = "percentile",
      labels = c(
        paste0("<", low),
        paste0(low, "-", high),
        paste0(">", high)
      ),
      guide = ggplot2::guide_legend(reverse = TRUE)
    )
  )
}

#' Create technical replicate dots layer
#'
#' Creates a ggplot2 layer showing raw score observations with technical
#' replicates displayed separately. Points are sized by observation count.
#'
#' @param data_view A besthr_data_view object
#' @param config A besthr_plot_config object
#'
#' @return A list of ggplot2 layers
#'
#' @export
#'
layer_tech_rep_dots <- function(data_view, config) {
  if (length(data_view$column_info) < 3) {
    stop("Technical replicate layer requires data with technical replicates")
  }

  score_col <- data_view$column_info[[1]]
  group_col <- data_view$column_info[[2]]
  tech_rep_col <- data_view$column_info[[3]]

  # Compute count summary
  dot_data <- data_view$original %>%
    factorise_cols(list(group_col, tech_rep_col)) %>%
    dplyr::group_by(!!group_col, !!tech_rep_col, !!score_col) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  list(
    ggplot2::geom_point(
      data = dot_data,
      mapping = ggplot2::aes(
        x = !!tech_rep_col,
        y = !!score_col,
        size = count,
        colour = !!group_col,
        fill = !!group_col
      ),
      alpha = config$point_alpha
    ),
    ggplot2::scale_size_continuous(range = config$point_size_range),
    ggplot2::facet_wrap(
      ggplot2::vars(!!group_col),
      strip.position = "bottom",
      nrow = 1
    )
  )
}
