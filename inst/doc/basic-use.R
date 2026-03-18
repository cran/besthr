## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.width=8, fig.height=6------------------------------------------------
library(besthr)

hr_data_1_file <- system.file("extdata", "example-data-1.csv", package = "besthr")
hr_data_1 <- readr::read_csv(hr_data_1_file)
head(hr_data_1)

hr_est_1 <- estimate(hr_data_1, score, group, control = "A")
hr_est_1

plot(hr_est_1)

## ----fig.width=8, fig.height=6------------------------------------------------
estimate(hr_data_1, score, group, control = "B" ) %>%
  plot()

## ----fig.width=8, fig.height=6------------------------------------------------
estimate(hr_data_1, score, group, control = "A", nits = 1000, low = 0.4, high = 0.6) %>%
  plot()

## ----fig.width=8, fig.height=6------------------------------------------------

hr_data_3_file <- system.file("extdata", "example-data-3.csv", package = "besthr")
hr_data_3 <- readr::read_csv(hr_data_3_file)
head(hr_data_3)

hr_est_3 <- estimate(hr_data_3, score, sample, rep, control = "A")

hr_est_3

plot(hr_est_3)


## ----fig.width=8, fig.height=6------------------------------------------------

hr_est_3 %>%
  plot(which = "just_data")

## ----fig.width=8, fig.height=6------------------------------------------------
# Modern theme (default)
plot(hr_est_1)

# Classic theme (original style)
plot(hr_est_1, theme = "classic")

## ----fig.width=8, fig.height=6------------------------------------------------
# Default is already colorblind-safe
plot(hr_est_1)

# Original colors
plot(hr_est_1, colors = "default")

# Viridis palette
plot(hr_est_1, colors = "viridis")

## ----fig.width=8, fig.height=6------------------------------------------------
# Publication-ready style
plot(hr_est_1, config = besthr_style("publication"))

# Presentation style (larger elements)
plot(hr_est_1, config = besthr_style("presentation"))

# Density style (gradient density instead of points)
plot(hr_est_1, config = besthr_style("density"))

# See all available styles
list_besthr_styles()

## -----------------------------------------------------------------------------
# Get palette colors
besthr_palette("okabe_ito", n = 4)

# Available palettes
besthr_palette("default", n = 3)
besthr_palette("viridis", n = 3)

## ----fig.width=8, fig.height=6------------------------------------------------
# Modern look with colorblind-safe colors (this is the default)
plot(hr_est_1, theme = "modern", colors = "okabe_ito")

# Classic appearance (original besthr style)
plot(hr_est_1, theme = "classic", colors = "default")

# Viridis color scheme
plot(hr_est_1, colors = "viridis")

## ----fig.width=8, fig.height=6------------------------------------------------
library(patchwork)

p <- plot(hr_est_1)

p + plot_annotation(
  title = 'HR Score Analysis',
  subtitle = "Control vs Treatment",
  caption = 'Generated with besthr'
)

## -----------------------------------------------------------------------------
# Create a custom configuration
cfg <- besthr_plot_config(
  panel_widths = c(2, 1),        # Make data panel wider than bootstrap panel
  point_size_range = c(3, 10),   # Larger points
  point_alpha = 0.9,             # More opaque points
  mean_line_width = 1.5,         # Thicker mean lines
  theme_style = "modern",
  color_palette = "okabe_ito"
)

print(cfg)

## ----fig.width=8, fig.height=6------------------------------------------------
# Use the configuration in plot
plot(hr_est_1, config = cfg)

## ----fig.width=8, fig.height=6------------------------------------------------
# Modify just one setting
cfg2 <- update_config(cfg, panel_widths = c(1, 2))
plot(hr_est_1, config = cfg2)

## -----------------------------------------------------------------------------
# Create a data view
dv <- besthr_data_view(hr_est_1)
print(dv)

# Access the data
head(dv$ranked)
head(dv$bootstrap)
dv$rank_limits

## ----fig.width=10, fig.height=5-----------------------------------------------
library(patchwork)

# Create panels
cfg <- besthr_plot_config(theme_style = "modern", color_palette = "okabe_ito")
dv <- besthr_data_view(hr_est_1, cfg)

p1 <- build_observation_panel(dv, cfg, "rank_simulation")
p2 <- build_bootstrap_panel(dv, cfg)

# Custom composition with different widths
p1 + p2 + plot_layout(widths = c(3, 1))

## ----fig.width=6, fig.height=5------------------------------------------------
plot_raincloud(hr_est_1)

## ----fig.width=6, fig.height=5------------------------------------------------
# With modern styling
plot_raincloud(hr_est_1, theme = "modern", colors = "okabe_ito")

## ----fig.width=6, fig.height=5------------------------------------------------
plot_bootstrap_raincloud(hr_est_1)

## -----------------------------------------------------------------------------
# Default CI colors
derive_ci_colors("default", "classic")

# Okabe-Ito derived CI colors
derive_ci_colors("okabe_ito", "modern")

# Viridis derived CI colors
derive_ci_colors("viridis", "classic")

## -----------------------------------------------------------------------------
# Create example data with 3 groups and realistic variation
set.seed(42)
d_effect <- data.frame(
  score = c(
    sample(1:4, 12, replace = TRUE),   # Group A: low scores (control)
    sample(4:8, 12, replace = TRUE),   # Group B: medium-high scores
    sample(6:10, 12, replace = TRUE)   # Group C: high scores
  ),
  group = rep(c("A", "B", "C"), each = 12)
)
hr_effect <- estimate(d_effect, score, group, control = "A", nits = 1000)

## ----fig.width=8, fig.height=6------------------------------------------------
plot(hr_effect, show_significance = TRUE)

## ----fig.width=8, fig.height=6------------------------------------------------
plot(hr_effect, show_effect_size = TRUE)

## -----------------------------------------------------------------------------
# Compute significance
compute_significance(hr_effect)

# Compute effect sizes
compute_effect_size(hr_effect)

## -----------------------------------------------------------------------------
# Default tibble format
besthr_table(hr_effect)

# With significance stars
besthr_table(hr_effect, include_significance = TRUE)

# Markdown format
besthr_table(hr_est_1, format = "markdown")

## ----eval=FALSE---------------------------------------------------------------
# # Save to PNG (default 300 DPI)
# save_besthr(hr_est_1, "figure1.png")
# 
# # Save to PDF
# save_besthr(hr_est_1, "figure1.pdf", width = 10, height = 8)
# 
# # Save raincloud plot
# save_besthr(hr_est_1, "raincloud.png", type = "raincloud")
# 
# # With custom options
# save_besthr(hr_est_1, "figure1.png",
#             theme = "modern",
#             colors = "okabe_ito",
#             width = 10,
#             height = 6,
#             dpi = 600)

