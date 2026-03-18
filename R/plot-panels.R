#' Build the observation panel
#'
#' Creates a ggplot showing either ranked observations (averaged tech reps) or
#' raw scores with technical replicates displayed.
#'
#' @param data_view A besthr_data_view object
#' @param config A besthr_plot_config object
#' @param which Character specifying panel type: "rank_simulation" for averaged
#'   ranked data, or "just_data" for raw scores with tech reps.
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' dv <- besthr_data_view(hr)
#' cfg <- besthr_plot_config()
#'
#' build_observation_panel(dv, cfg, "rank_simulation")
#'
build_observation_panel <- function(data_view, config, which = "rank_simulation") {
  if (!which %in% c("rank_simulation", "just_data")) {
    stop("which must be 'rank_simulation' or 'just_data'")
  }

  group_col <- data_view$group_col

  # Determine if we have tech reps and user wants just_data
  has_tech_reps <- length(data_view$column_info) == 3

  if (has_tech_reps && which == "just_data") {
    # Tech rep dot plot (raw scores)
    p <- build_tech_rep_panel(data_view, config)
  } else {
    # Standard ranked dot plot
    p <- build_ranked_panel(data_view, config)
  }

  p
}

#' Build the ranked observation panel
#'
#' Internal function to build the panel showing ranked observations.
#'
#' @param data_view A besthr_data_view object
#' @param config A besthr_plot_config object
#'
#' @return A ggplot object
#'
#' @keywords internal
#'
build_ranked_panel <- function(data_view, config) {
  group_col <- data_view$group_col

  # Compute count summary for dot sizes
  dot_data <- data_view$ranked %>%
    dplyr::group_by(!!group_col, rank) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  p <- ggplot2::ggplot(dot_data) +
    ggplot2::aes(x = !!group_col, y = rank) +
    ggplot2::geom_point(
      ggplot2::aes(size = count, colour = !!group_col, fill = !!group_col),
      alpha = config$point_alpha
    ) +
    ggplot2::geom_hline(
      data = data_view$group_means,
      mapping = ggplot2::aes(yintercept = mean, colour = !!group_col),
      linetype = config$mean_line_type,
      linewidth = config$mean_line_width
    ) +
    ggplot2::scale_size_continuous(
      range = config$point_size_range,
      guide = ggplot2::guide_legend(
        direction = "horizontal",
        title.position = "left",
        nrow = 1
      )
    ) +
    ggplot2::ylim(data_view$rank_limits)

  # Apply theme
  p <- p + theme_besthr(config$theme_style)

  # Apply color palette with drop = FALSE for consistent colors
  if (config$color_palette != "default") {
    p <- p +
      scale_color_besthr(config$color_palette, drop = FALSE) +
      scale_fill_besthr(config$color_palette, drop = FALSE)
  } else {
    p <- p +
      ggplot2::scale_colour_discrete(drop = FALSE) +
      ggplot2::scale_fill_discrete(drop = FALSE)
  }

  p
}

#' Build the technical replicate panel
#'
#' Internal function to build the panel showing raw scores with tech reps.
#'
#' @param data_view A besthr_data_view object
#' @param config A besthr_plot_config object
#'
#' @return A ggplot object
#'
#' @keywords internal
#'
build_tech_rep_panel <- function(data_view, config) {
  score_col <- data_view$column_info[[1]]
  group_col <- data_view$column_info[[2]]
  tech_rep_col <- data_view$column_info[[3]]

  # Compute count summary for dot sizes
  dot_data <- data_view$original %>%
    factorise_cols(list(group_col, tech_rep_col)) %>%
    dplyr::group_by(!!group_col, !!tech_rep_col, !!score_col) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  p <- ggplot2::ggplot(dot_data) +
    ggplot2::aes(x = !!tech_rep_col, y = !!score_col) +
    ggplot2::geom_point(
      ggplot2::aes(size = count, colour = !!group_col, fill = !!group_col),
      alpha = config$point_alpha
    ) +
    ggplot2::scale_size_continuous(range = config$point_size_range) +
    ggplot2::facet_wrap(
      ggplot2::vars(!!group_col),
      strip.position = "bottom",
      nrow = 1
    )

  # Apply theme
  p <- p + theme_besthr(config$theme_style) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside"
    )

  # Apply color palette with drop = FALSE for consistent colors
  if (config$color_palette != "default") {
    p <- p +
      scale_color_besthr(config$color_palette, drop = FALSE) +
      scale_fill_besthr(config$color_palette, drop = FALSE)
  } else {
    p <- p +
      ggplot2::scale_colour_discrete(drop = FALSE) +
      ggplot2::scale_fill_discrete(drop = FALSE)
  }

  p
}

#' Build the bootstrap distribution panel
#'
#' Creates a ggplot showing ridge density plots of bootstrap distributions with
#' confidence interval shading.
#'
#' @param data_view A besthr_data_view object
#' @param config A besthr_plot_config object
#'
#' @return A ggplot object
#'
#' @export
#' @importFrom ggplot2 after_stat
#'
#' @examples
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' dv <- besthr_data_view(hr)
#' cfg <- besthr_plot_config()
#'
#' build_bootstrap_panel(dv, cfg)
#'
build_bootstrap_panel <- function(data_view, config) {
  group_col <- data_view$group_col
  ci_colors <- derive_ci_colors(config$color_palette, config$theme_style)

  low <- data_view$quantiles["low"]
  high <- data_view$quantiles["high"]


  # Get consistent factor levels from ranked data to ensure color matching
  group_levels <- unique(as.character(data_view$ranked[[data_view$group_col_name]]))

  # Apply factor levels to bootstrap and ci data for color consistency
  bootstrap_data <- data_view$bootstrap
  bootstrap_data[[data_view$group_col_name]] <- factor(
    bootstrap_data[[data_view$group_col_name]],
    levels = group_levels
  )

  ci_data <- data_view$ci
  ci_data[[data_view$group_col_name]] <- factor(
    ci_data[[data_view$group_col_name]],
    levels = group_levels
  )

  if (config$density_style == "points") {
    # Points style: jittered dots instead of density
    p <- ggplot2::ggplot(bootstrap_data) +
      ggplot2::aes(x = mean, y = !!group_col, colour = !!group_col) +
      ggplot2::geom_jitter(
        width = 0,
        height = 0.15,
        size = 0.5,
        alpha = 0.3
      ) +
      ggplot2::geom_pointrange(
        data = ci_data,
        mapping = ggplot2::aes(
          x = mean, y = !!group_col,
          xmin = low, xmax = high,
          colour = !!group_col
        ),
        size = 0.6,
        linewidth = 1
      ) +
      ggplot2::xlim(data_view$rank_limits) +
      ggplot2::coord_flip() +
      theme_besthr(config$theme_style) +
      ggplot2::guides(colour = "none")

    # Apply color palette with drop = FALSE to maintain color consistency
    if (config$color_palette != "default") {
      p <- p + scale_color_besthr(config$color_palette, drop = FALSE)
    } else {
      p <- p + ggplot2::scale_colour_discrete(drop = FALSE)
    }

  } else if (config$density_style == "solid") {
    # Solid style: single color density without CI shading
    p <- ggplot2::ggplot(bootstrap_data) +
      ggplot2::aes(x = mean, y = !!group_col, fill = !!group_col) +
      ggplot2::xlim(data_view$rank_limits) +
      ggridges::geom_density_ridges(
        alpha = config$density_alpha,
        scale = 0.9
      ) +
      ggplot2::geom_vline(
        data = ci_data,
        mapping = ggplot2::aes(xintercept = low),
        linetype = 2,
        alpha = 0.5
      ) +
      ggplot2::geom_vline(
        data = ci_data,
        mapping = ggplot2::aes(xintercept = high),
        linetype = 2,
        alpha = 0.5
      ) +
      ggplot2::coord_flip() +
      theme_besthr(config$theme_style) +
      ggplot2::guides(fill = "none")

    # Apply color palette with drop = FALSE to maintain color consistency
    if (config$color_palette != "default") {
      p <- p + scale_fill_besthr(config$color_palette, drop = FALSE)
    } else {
      p <- p + ggplot2::scale_fill_discrete(drop = FALSE)
    }

  } else {
    # Gradient style: density in group color with faded tails outside CI
    # Base density at low alpha (faded tails), then overlay CI region highlight

    # Build data for fading the tails - add rectangles with white overlay
    # for regions outside CI bounds
    x_limits <- data_view$rank_limits
    tail_alpha <- 0.5  # Semi-transparent white to fade tails

    p <- ggplot2::ggplot(bootstrap_data) +
      ggplot2::aes(x = mean, y = !!group_col, fill = !!group_col) +
      ggplot2::xlim(x_limits) +
      # Main density in group color
      ggridges::geom_density_ridges(
        alpha = config$density_alpha,
        scale = 0.9,
        colour = NA
      ) +
      ggplot2::coord_flip() +
      theme_besthr(config$theme_style) +
      ggplot2::guides(fill = "none")

    # Add semi-transparent white overlay on the tails (outside CI)
    # This creates the "faded" effect for regions outside the CI
    p <- p +
      # Left tail (below low CI bound)
      ggplot2::geom_rect(
        data = ci_data,
        mapping = ggplot2::aes(
          xmin = x_limits[1],
          xmax = low,
          ymin = as.numeric(!!group_col) - 0.45,
          ymax = as.numeric(!!group_col) + 0.45
        ),
        fill = "white",
        alpha = tail_alpha,
        inherit.aes = FALSE
      ) +
      # Right tail (above high CI bound)
      ggplot2::geom_rect(
        data = ci_data,
        mapping = ggplot2::aes(
          xmin = high,
          xmax = x_limits[2],
          ymin = as.numeric(!!group_col) - 0.45,
          ymax = as.numeric(!!group_col) + 0.45
        ),
        fill = "white",
        alpha = tail_alpha,
        inherit.aes = FALSE
      )

    # Apply color palette with drop = FALSE to maintain color consistency
    if (config$color_palette != "default") {
      p <- p + scale_fill_besthr(config$color_palette, drop = FALSE)
    } else {
      p <- p + ggplot2::scale_fill_discrete(drop = FALSE)
    }
  }

  p
}

#' Compose besthr panels
#'
#' Combines observation and bootstrap panels using patchwork with proper
#' alignment and shared legends.
#'
#' @param panels A list of ggplot objects to compose
#' @param config A besthr_plot_config object
#'
#' @return A patchwork object
#'
#' @export
#'
#' @examples
#' d <- make_data()
#' hr <- estimate(d, score, group)
#' dv <- besthr_data_view(hr)
#' cfg <- besthr_plot_config()
#'
#' p1 <- build_observation_panel(dv, cfg)
#' p2 <- build_bootstrap_panel(dv, cfg)
#' compose_besthr_panels(list(p1, p2), cfg)
#'
compose_besthr_panels <- function(panels, config) {
  if (length(panels) < 2) {
    stop("At least two panels are required for composition")
  }

  p <- patchwork::wrap_plots(panels) +
    patchwork::plot_layout(
      guides = "collect",
      widths = config$panel_widths
    ) &
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.spacing.x = ggplot2::unit(0.8, "cm"),
      legend.spacing.y = ggplot2::unit(0, "cm"),
      legend.key.size = ggplot2::unit(0.4, "cm")
    )

  p
}
