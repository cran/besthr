## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.width=8, fig.height=6-----------------------------------------------
library(besthr)

hr_data_1_file <- system.file("extdata", "example-data-1.csv", package = "besthr")
hr_data_1 <- readr::read_csv(hr_data_1_file)
head(hr_data_1)

hr_est_1 <- estimate(hr_data_1, score, group, control = "A")
hr_est_1

plot(hr_est_1)

## ---- fig.width=8, fig.height=6-----------------------------------------------
estimate(hr_data_1, score, group, control = "B" ) %>%
  plot()

## ---- fig.width=8, fig.height=6-----------------------------------------------
estimate(hr_data_1, score, group, control = "A", nits = 1000, low = 0.4, high = 0.6) %>%
  plot()

## ---- fig.width=8, fig.height=6-----------------------------------------------

hr_data_3_file <- system.file("extdata", "example-data-3.csv", package = "besthr")
hr_data_3 <- readr::read_csv(hr_data_3_file)
head(hr_data_3)

hr_est_3 <- estimate(hr_data_3, score, sample, rep, control = "A")

hr_est_3

plot(hr_est_3)


## ---- fig.width=8, fig.height=6-----------------------------------------------

hr_est_3 %>% 
  plot(which = "just_data")

## ---- fig.width=8, fig.height=6-----------------------------------------------
library(patchwork)

p <- plot(hr_est_1)

p + plot_annotation(title = 'A stylish besthr plot', 
                    subtitle = "better than ever", 
                    caption = 'Though this example is not meaningful')
p



## ---- fig.width=8, fig.height=6-----------------------------------------------
library(ggplot2)
p[[1]] <- p[[1]] + theme(axis.title.y = element_text(family = "Times", colour="blue", size=24))
p

## ---- fig.width=8, fig.height=6-----------------------------------------------
p[[1]] <- p[[1]] + scale_colour_manual(values = c("blue", "#440000"))
p

p[[1]] <- p[[1]] + scale_colour_viridis_d()
p

p[[1]] <- p[[1]] + scale_colour_brewer(type="qual", palette="Accent")
p

## ---- fig.width=8, fig.height=6-----------------------------------------------
p[[2]] <- p[[2]] + scale_fill_manual(
  values = c("blue", "pink", "yellow"),
  name = "bootstrap percentile", labels=c("lower", "non-significant", "higher"),
  guide = guide_legend(reverse=TRUE)
  )
p

