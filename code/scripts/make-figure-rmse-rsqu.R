#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script:
## Desc: RMSE and Rsqu plots
## Date: April 2024

# module load R/4.3.0

# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("yardstick")
library("ggtext")

set.seed(123)



# get data ----------------------------------------------------------------

results <- readRDS(here("data", "derived", "results.rds")) %>%
  filter(restrict_confounder == FALSE) %>%
  mutate(
    assignment = recode_factor(
    assignment,
    random = "Random",
    correlated_altitude = "Correlated\nwith altitude",
    correlated_region = "Correlated\nwith region",
    .ordered = TRUE
    ),
    test_plot_location = recode_factor(
      test_plot_location,
      random = "Random",
      centre = "Core",
      edge = "Edge",
      .ordered = TRUE
    ),
    learner = recode_factor(
      learner,
      s = "Single model",
      t = "Two models",
      x = "Crossed models",
      .ordered = TRUE
    ))


# make function -----------------------------------------------------------

plot_rmse <- function(data,
                      y_var, y_lab,
                      x_var, x_lab,
                      x_breaks = wavier(),
                      x_cont = FALSE) {
  if (x_cont == TRUE) {
    data %>%
      ggplot(aes(x = x_var, y = y_var, colour = learner)) +
      stat_summary(fun = mean,
                   geom = "line",
                   linewidth = 0.3) +
      stat_summary(
        geom = "pointrange",
        size = 0.25,
        fill = "white",
        shape = 21,
        stroke = 0.5,
        linewidth = 0.5
      ) +
      scale_colour_manual(values = c("#009E73", "#E69F00", "#0072B2")) +
      scale_x_continuous(breaks = x_breaks) +
      xlab(x_lab) +
      ylab(y_lab) +
      theme_classic(base_size = 6) +
      theme(axis.title.y = element_markdown(),
            legend.title = element_blank())

  } else if (x_cont == FALSE) {
    data %>%
      ggplot(aes(x = x_var, y = y_var, colour = learner, group = learner)) +
      stat_summary(fun = mean,
                   geom = "line",
                   linewidth = 0.3) +
      stat_summary(
        geom = "pointrange",
        size = 0.25,
        fill = "white",
        shape = 21,
        stroke = 0.5,
        linewidth = 0.5
      ) +
      scale_colour_manual(values = c("#009E73", "#E69F00", "#0072B2")) +
      xlab(x_lab) +
      ylab(y_lab) +
      theme_classic(base_size = 6) +
      theme(axis.title.y = element_markdown(),
            legend.title = element_blank())
  }

}


# make plots --------------------------------------------------------------

plot_rmse(data = results,
          y_var = results$rmse,
          y_lab = "RMSE",
          x_var = results$n_train,
          x_lab = "Sample size",
          x_breaks = c(0, 62, 125, 250, 500, 1000),
          x_cont = TRUE) +

  plot_rmse(data = results,
            y_var = results$rsq,
            y_lab = "R<sup>2</sup>",
            x_var = results$n_train,
            x_lab = "Sample size",
            x_breaks = c(0, 62, 125, 250, 500, 1000),
            x_cont = TRUE) +

  plot_rmse(data = results,
            y_var = results$rmse,
            y_lab = "RMSE",
            x_var = results$prop_not_treated,
            x_lab = "Proportion not treated\n(treatment imbalance)",
            x_breaks = c(0.3, 0.5, 0.7),
            x_cont = TRUE) +

  plot_rmse(data = results,
            y_var = results$rsq,
            y_lab = "R<sup>2</sup>",
            x_var = results$prop_not_treated,
            x_lab = "Proportion not treated\n(treatment imbalance)",
            x_breaks = c(0.3, 0.5, 0.7),
            x_cont = TRUE) +

  plot_rmse(data = results,
            y_var = results$rmse,
            y_lab = "RMSE",
            x_var = results$assignment,
            x_lab = "Treatment assignment") +


  plot_rmse(data = results,
            y_var = results$rsq,
            y_lab = "R<sup>2</sup>",
            x_var = results$assignment,
            x_lab = "Treatment assignment") +

  plot_rmse(data = results,
            y_var = results$rmse,
            y_lab = "RMSE",
            x_var = results$var_omit,
            x_lab = "Omission of important variable") +

  plot_rmse(data = results,
            y_var = results$rsq,
            y_lab = "R<sup>2</sup>",
            x_var = results$var_omit,
            x_lab = "Omission of important variable") +

  plot_rmse(data = results,
            y_var = results$rmse,
            y_lab = "RMSE",
            x_var = results$test_plot_location,
            x_lab = "Location of test plots") +

  plot_rmse(data = results,
            y_var = results$rsq,
            y_lab = "R<sup>2</sup>",
            x_var = results$test_plot_location,
            x_lab = "Location of test plots") +

  plot_layout(guides = "collect", ncol = 2) &
  theme(legend.position = "bottom")


ggsave(here::here("output","figures","results-rmse-rsqu.png"),
       width = 1000, height = 2000, units = "px")
