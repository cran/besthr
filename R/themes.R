#' besthr color palettes
#'
#' Returns a color palette suitable for besthr visualizations. The default
#' palette uses Okabe-Ito colorblind-safe colors.
#'
#' @param palette Character string specifying the palette. Options are:
#'   \itemize{
#'     \item "default" - Original besthr colors
#'     \item "okabe_ito" - Colorblind-safe Okabe-Ito palette
#'     \item "viridis" - Viridis color scale
#'   }
#' @param n Number of colors to return. If NULL, returns all colors in palette.
#'
#' @return A character vector of hex color codes
#' @export
#'
#' @examples
#' besthr_palette()
#' besthr_palette("okabe_ito", 3)
#'
besthr_palette <- function(palette = "default", n = NULL) {
  # For viridis, generate colors spread across the full range
  if (palette == "viridis") {
    if (is.null(n)) n <- 8
    # Use option "D" (viridis) and spread colors evenly across the range
    return(viridisLite::viridis(n, begin = 0.1, end = 0.9))
  }

  palettes <- list(
    default = c(
      "#F8766D", "#00BA38", "#619CFF", "#F564E3",
      "#00BFC4", "#B79F00", "#FF6C91", "#00B0F6"
    ),
    okabe_ito = c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#999999"
    )
  )

  pal <- palettes[[palette]]
  if (is.null(pal)) {
    stop("Unknown palette: ", palette,
         ". Choose from: ", paste(names(palettes), collapse = ", "), ", viridis")
  }

  if (!is.null(n)) {
    if (n > length(pal)) {
      pal <- grDevices::colorRampPalette(pal)(n)
    } else {
      pal <- pal[seq_len(n)]
    }
  }

  pal
}

#' besthr ggplot2 theme
#'
#' A custom theme for besthr plots. The "classic" theme matches the original
#' besthr appearance, while "modern" provides a cleaner, more contemporary look.
#'
#' @param style Character string specifying the theme style. Options are:
#'   \itemize{
#'     \item "classic" - Original besthr theme (theme_minimal)
#'     \item "modern" - Clean, contemporary style with refined typography
#'   }
#' @param base_size Base font size (default 11)
#' @param base_family Base font family
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_besthr("modern")
#'
theme_besthr <- function(style = "classic", base_size = 11, base_family = "") {
  if (style == "classic") {
    return(ggplot2::theme_minimal(base_size = base_size, base_family = base_family))
  }

  if (style == "modern") {
    return(
      ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
        ggplot2::theme(
          # Typography
          plot.title = ggplot2::element_text(
            size = ggplot2::rel(1.2),
            face = "bold",
            margin = ggplot2::margin(b = 10)
          ),
          axis.title = ggplot2::element_text(
            size = ggplot2::rel(1.0),
            face = "bold"
          ),
          axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
          legend.title = ggplot2::element_text(face = "bold"),

          # Grid
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(
            color = "grey90",
            linewidth = 0.3
          ),

          # Legend
          legend.position = "bottom",
          legend.box = "horizontal",

          # Spacing
          plot.margin = ggplot2::margin(15, 15, 15, 15),

          # Strip (for facets)
          strip.text = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.0)),
          strip.background = ggplot2::element_blank()
        )
    )
  }

  stop("Unknown style: ", style, ". Choose from: classic, modern")
}

#' Discrete color scale for besthr
#'
#' A discrete color scale using besthr palettes.
#'
#' @param palette Character string specifying the palette (see \code{\link{besthr_palette}})
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#'
#' @return A ggplot2 discrete color scale
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_besthr("okabe_ito")
#'
scale_color_besthr <- function(palette = "default", ...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = function(n) besthr_palette(palette, n),
    ...
  )
}

#' @rdname scale_color_besthr
#' @export
scale_colour_besthr <- scale_color_besthr

#' Discrete fill scale for besthr
#'
#' A discrete fill scale using besthr palettes.
#'
#' @param palette Character string specifying the palette (see \code{\link{besthr_palette}})
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}
#'
#' @return A ggplot2 discrete fill scale
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_besthr("okabe_ito")
#'
scale_fill_besthr <- function(palette = "default", ...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = function(n) besthr_palette(palette, n),
    ...
  )
}

#' Confidence interval fill colors
#'
#' Returns the fill colors used for confidence interval regions in bootstrap
#' distribution plots.
#'
#' @param style Character string: "default" for original colors, "modern" for
#'   updated colors
#'
#' @return A named character vector of hex colors for low, middle, high regions
#' @keywords internal
#'
ci_fill_colors <- function(style = "default") {
  if (style == "default") {
    return(c("#0000FFA0", "#A0A0A0A0", "#FF0000A0"))
  }

  if (style == "modern") {
    return(c("#2166AC99", "#F7F7F799", "#B2182B99"))
  }


  stop("Unknown style: ", style)
}

#' Derive CI colors based on palette and theme
#'
#' Computes confidence interval fill colors that harmonize with the selected
#' color palette and theme style. This ensures visual consistency between the
#' observation panel colors and the bootstrap density shading.
#'
#' @param palette Character string specifying the color palette: "default",
#'   "okabe_ito", or "viridis"
#' @param theme_style Character string specifying the theme: "classic" or "modern"
#'
#' @return A character vector of three hex colors with alpha for low, middle,
#'   and high CI regions
#'
#' @export
#'
#' @examples
#' derive_ci_colors("default", "classic")
#' derive_ci_colors("okabe_ito", "modern")
#' derive_ci_colors("viridis", "classic")
#'
derive_ci_colors <- function(palette = "default", theme_style = "classic") {
  # For backward compatibility, default palette uses original CI colors
  if (palette == "default") {
    return(ci_fill_colors(if (theme_style == "modern") "modern" else "default"))
  }

  # Okabe-Ito derived colors (colorblind-safe)
  if (palette == "okabe_ito") {
    return(c(
      "#0072B2AA",  # Blue (low)
      "#999999AA",  # Gray (middle)
      "#D55E00AA"   # Vermillion (high)
    ))
  }

  # Viridis derived colors
  if (palette == "viridis") {
    # viridis returns #RRGGBBAA format, extract RGB and add our alpha
    viridis_cols <- viridisLite::viridis(3, alpha = 0.67)
    return(viridis_cols)
  }

  # Fallback to default
  ci_fill_colors("default")
}

#' Apply besthr theme consistently
#'
#' Applies the besthr theme and color scales to a ggplot object based on
#' configuration settings. This ensures consistent theming across all plot
#' components.
#'
#' @param p A ggplot object
#' @param config A besthr_plot_config object
#' @param include_fill Logical, whether to apply fill scale (default TRUE)
#' @param include_color Logical, whether to apply color scale (default TRUE)
#'
#' @return The ggplot object with theme and scales applied
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'   geom_point()
#' cfg <- besthr_plot_config(theme_style = "modern", color_palette = "okabe_ito")
#' apply_besthr_theme(p, cfg)
#'
apply_besthr_theme <- function(p, config, include_fill = TRUE, include_color = TRUE) {
  # Apply theme
  p <- p + theme_besthr(config$theme_style)

  # Apply color scales if not default (to preserve backward compatibility)
  if (config$color_palette != "default") {
    if (include_color) {
      p <- p + scale_color_besthr(config$color_palette)
    }
    if (include_fill) {
      p <- p + scale_fill_besthr(config$color_palette)
    }
  }

  p
}

#' Get a preset plot style
#'
#' Returns a pre-configured \code{besthr_plot_config} object with sensible
#' defaults for common use cases. This is the easiest way to customize
#' besthr plot appearance without understanding all the configuration options.
#'
#' @param style Character string specifying the style preset:
#'   \itemize{
#'     \item "default" - Modern theme with colorblind-safe colors (recommended)
#'     \item "classic" - Original besthr appearance for backward compatibility
#'     \item "publication" - Clean style suitable for journal figures
#'     \item "presentation" - Larger elements for slides
#'     \item "density" - Uses gradient density instead of points for bootstrap
#'   }
#'
#' @return A \code{besthr_plot_config} object
#'
#' @export
#'
#' @examples
#' d <- make_data()
#' hr <- estimate(d, score, group)
#'
#' # Quick styling with presets
#' plot(hr, config = besthr_style("publication"))
#' plot(hr, config = besthr_style("presentation"))
#' plot(hr, config = besthr_style("density"))
#'
#' # Same as default
#' plot(hr, config = besthr_style("default"))
#'
besthr_style <- function(style = "default") {
  styles <- list(
    default = besthr_plot_config(
      theme_style = "modern",
      color_palette = "okabe_ito"
    ),
    classic = besthr_plot_config(
      theme_style = "classic",
      color_palette = "default"
    ),
    publication = besthr_plot_config(
      theme_style = "modern",
      color_palette = "okabe_ito",
      point_size_range = c(2, 6),
      point_alpha = 0.9,
      density_alpha = 0.6
    ),
    presentation = besthr_plot_config(
      theme_style = "modern",
      color_palette = "okabe_ito",
      point_size_range = c(4, 12),
      point_alpha = 0.9,
      mean_line_width = 1.5
    ),
    density = besthr_plot_config(
      theme_style = "modern",
      color_palette = "okabe_ito",
      density_style = "gradient"
    )
  )

  if (!style %in% names(styles)) {
    stop("Unknown style: '", style, "'. ",
         "Choose from: ", paste(names(styles), collapse = ", "))
  }

  styles[[style]]
}

#' List available style presets
#'
#' Shows all available preset styles that can be used with \code{besthr_style()}.
#'
#' @return A character vector of style names (invisibly)
#'
#' @export
#'
#' @examples
#' list_besthr_styles()
#'
list_besthr_styles <- function() {
  cat("Available besthr style presets:\n\n")
  cat("  'default'       Modern theme with colorblind-safe colors (recommended)\n")
  cat("  'classic'       Original besthr appearance\n")
  cat("  'publication'   Clean style for journal figures\n")
  cat("  'presentation'  Larger elements for slides\n")
  cat("  'density'       Gradient density display for bootstrap\n")
  cat("\nUsage: plot(hr, config = besthr_style('publication'))\n")
  invisible(c("default", "classic", "publication", "presentation", "density"))
}
