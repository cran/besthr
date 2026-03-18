#' plots the \code{hrest} object
#'
#' returns a ggplot object representing the hrest object from
#' \code{\link{estimate}}. The content of left panel varies according to the
#' value of the \code{which} parameter. If \code{which = "rank_simulation"} is
#' used a plot of rank score values will be plotted in the left panel. In this
#' case technical replicates will be averaged if provided. If
#' \code{which = "just_data" } a plot of scores only is created and technical
#' replicates are displayed as is. In each case, the right hand panel shows the
#' rank bootstrap distribution and confidence interval boundaries for all non-
#' control groups.
#'
#' @param x the \code{hrest} object from \code{\link{estimate}}
#' @param which the type of left hand panel to create. Either "rank_simulation"
#'  or "just_data"
#' @param theme the visual theme to use. Either "modern" (default, cleaner
#'  contemporary style) or "classic" (original besthr appearance)
#' @param colors the color palette to use. Either "okabe_ito" (default,
#'  colorblind-safe), "default" (original colors), or "viridis"
#' @param config an optional besthr_plot_config object for advanced customization.
#'  If provided, theme and colors parameters are ignored.
#' @param show_significance Logical, whether to show significance stars on groups
#'  where CI doesn't overlap control (default FALSE)
#' @param show_effect_size Logical, whether to show effect size annotation (default FALSE)
#' @param ... Other parameters (ignored)
#'
#' @examples
#'
#'  d1 <- make_data()
#'  hr_est <- estimate(d1, score, group)
#'  plot(hr_est)
#'
#'  # Use modern theme with colorblind-safe palette
#'  plot(hr_est, theme = "modern", colors = "okabe_ito")
#'
#'  # Advanced configuration
#'  cfg <- besthr_plot_config(
#'    panel_widths = c(2, 1),
#'    point_size_range = c(3, 10)
#'  )
#'  plot(hr_est, config = cfg)
#'
#' @export
#' @return ggplot object
#' @importFrom stats quantile
#' @importFrom ggplot2 after_stat
plot.hrest <- function(x, ..., which = "rank_simulation",
                       theme = "modern", colors = "okabe_ito",
                       config = NULL,
                       show_significance = FALSE,
                       show_effect_size = FALSE) {
  hrest <- x

  # Build config from parameters or use provided config
  if (is.null(config)) {
    config <- besthr_plot_config(
      theme_style = theme,
      color_palette = colors
    )
  }

  # Create unified data view (computes aligned limits)
  data_view <- besthr_data_view(hrest, config)

  # Message about CI bounds
  low_pct <- data_view$quantiles["low"] * 100
  high_pct <- data_view$quantiles["high"] * 100
  message(sprintf("Confidence interval: %.1f%% - %.1f%%", low_pct, high_pct))

  # Build panels
  p1 <- build_observation_panel(data_view, config, which)
  p2 <- build_bootstrap_panel(data_view, config)

  # Add significance annotations if requested
  if (show_significance) {
    sig <- compute_significance(hrest)
    sig_data <- sig[!is.na(sig$significant) & sig$stars != "", ]

    if (nrow(sig_data) > 0) {
      # Get y positions from group means for significant groups
      group_col_name <- data_view$group_col_name
      sig_data$y_pos <- sapply(sig_data$group, function(g) {
        data_view$group_means$mean[data_view$group_means[[group_col_name]] == g]
      })

      p1 <- p1 + ggplot2::geom_text(
        data = sig_data,
        mapping = ggplot2::aes(
          x = .data$group,
          y = .data$y_pos,
          label = .data$stars
        ),
        vjust = -0.5,
        hjust = 0.5,
        size = 5,
        inherit.aes = FALSE
      )
    }
  }

  # Add effect size annotation if requested
  if (show_effect_size) {
    effect <- compute_effect_size(hrest)
    effect_data <- effect[!is.na(effect$effect), ]

    if (nrow(effect_data) > 0) {
      effect_text <- paste(
        sapply(seq_len(nrow(effect_data)), function(i) {
          sprintf("%s: %.2f [%.2f, %.2f]",
                  effect_data$group[i],
                  effect_data$effect[i],
                  effect_data$effect_ci_low[i],
                  effect_data$effect_ci_high[i])
        }),
        collapse = "\n"
      )

      # Add as subtitle via patchwork annotation
      p2 <- p2 + ggplot2::labs(caption = paste("Effect sizes:", effect_text))
    }
  }

  # Compose with smart alignment
  compose_besthr_panels(list(p1, p2), config)
}

#' dot plot of ranked data without technical replicates
#'
#' \code{dot_plot} returns a ggplot object of ranked data with group on the
#' x-axis and rank on the y-axis. Point size indicates the number of
#' observations seen at that point. A per group horizontal line shows the group
#' ranked mean
#'
#' @param hrest the hrest object from \code{estimate}
#' @param group_col quoted group column name
#' @param theme_style character specifying the theme style
#' @param color_palette character specifying the color palette
#' @keywords internal
#' importFrom rlang .data
dot_plot <- function(hrest, group_col, theme_style = "modern",
                     color_palette = "okabe_ito") {
  p <- hrest$ranked_data %>%
    dplyr::group_by(!!group_col, rank) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    ggplot2::ggplot() +
    ggplot2::aes(!!group_col, rank) +
    ggplot2::geom_point(ggplot2::aes(size = .data$count, colour = !!group_col,
                                     fill = !!group_col)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = mean, colour = !!group_col),
                        data = hrest$group_means, linetype = 3, linewidth = 1)

  # Apply theme
  p <- p + theme_besthr(theme_style)

 # Apply color palette if not default (to preserve backward compatibility)
  if (color_palette != "default") {
    p <- p + scale_color_besthr(color_palette) + scale_fill_besthr(color_palette)
  }

  p
}

#' dot plot of score data with technical replicates
#'
#' \code{tech_rep_dot_plot} returns a ggplot object of score data with group on
#' technical replicate on the x-axis, score on the y-axis with point size
#' representing the number of observations at that point. Facets represent
#' individual groups
#' @param hrest the hrest object from \code{estimate}
#' @param score_col quoted score column name
#' @param group_col quoted group column name
#' @param tech_rep_col quoted tech replicate column name
#' @param theme_style character specifying the theme style
#' @param color_palette character specifying the color palette
#' @keywords internal
#' ImportFrom rlang .data
tech_rep_dot_plot <- function(hrest, score_col, group_col, tech_rep_col,
                              theme_style = "modern", color_palette = "okabe_ito") {

  p <- hrest$original_data %>% factorise_cols(list(group_col, tech_rep_col)) %>%
    dplyr::group_by(!!group_col, !!tech_rep_col, !!score_col) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    ggplot2::ggplot() +
    ggplot2::aes(!!tech_rep_col, !!score_col) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = .data$count,
        colour = !!group_col,
        fill = !!group_col
      )
    ) +
    ggplot2::facet_wrap(ggplot2::vars(!!group_col), strip.position = "bottom", nrow = 1)

  # Apply theme
  p <- p + theme_besthr(theme_style) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.placement = "outside")

  # Apply color palette if not default (to preserve backward compatibility)
  if (color_palette != "default") {
    p <- p + scale_color_besthr(color_palette) + scale_fill_besthr(color_palette)
  }

  p
}
