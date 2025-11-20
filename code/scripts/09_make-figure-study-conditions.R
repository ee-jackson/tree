#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-study-conditions.R
## Desc: makes figure 4
## Date: April 2024


# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")

set.seed(123)


# get data ----------------------------------------------------------------

models_out <- readRDS(here("data", "derived", "all_runs.rds")) %>%
  filter(restrict_confounder == FALSE) %>%
  mutate(
    learner = recode_factor(
    learner,
    s = "S-learner",
    t = "T-learner",
    x = "X-learner",
    .ordered = TRUE
  ),
  test_plot_location = recode_factor(
    test_plot_location,
    random = "Random",
    edge = "Edge",
    core = "Core",
    .ordered = TRUE
  ),
  var_omit = factor(var_omit, levels = c("TRUE", "FALSE"))
  )


# make functions ----------------------------------------------------------

plot_real_pred <- function(data, col_var, lab_var) {
  data %>%
    ggplot(aes(x = cate_real, y = cate_pred, colour = as.ordered(col_var))) +
    geom_hline(yintercept = 0, colour = "grey", linetype = 2, linewidth = 0.25) +
    geom_vline(xintercept = 0, colour = "grey", linetype = 2, linewidth = 0.25) +
    geom_point(size = 0.5, alpha = 0.5, shape = 16) +
    geom_abline(intercept = 0, slope = 1, colour = "blue", linewidth = 0.25) +
    scale_colour_viridis_d() +
    xlim(-25, 25) +
    ylim(-25, 25) +
    theme_classic(base_size = 6) +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.justification = "left",
          legend.title.position = "top",
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 5),
          legend.key.size = unit(0.01, "mm"),
          strip.text.x = element_text(hjust = 0, margin = margin(l=0.5)),
          strip.background = element_blank()
          ) +
    labs(y = "predicted ITE", x = "true ITE",
         colour = lab_var) +
    facet_wrap(~learner, ncol = 1)
}

# subset data -------------------------------------------------------------

models_out %>%
  filter(
    assignment == "random",
    prop_not_treated == 0.5,
    var_omit == FALSE,
    test_plot_location == "Random"
    ) %>%
  unnest(df_out) -> df_n_train

models_out %>%
  filter(
    assignment == "random",
    prop_not_treated == 0.5,
    var_omit == FALSE,
    n_train == 1000,
  ) %>%
  unnest(df_out) -> df_test_loc

models_out %>%
  filter(
    assignment == "random",
    prop_not_treated == 0.5,
    n_train == 1000,
    test_plot_location == "Random"
  ) %>%
  unnest(df_out) -> df_var_omit


# make plots --------------------------------------------------------------

plot_real_pred(
  data = df_n_train,
  col_var = df_n_train$n_train,
  lab_var = "Training sample size"
  ) +
  plot_real_pred(
    data = df_test_loc,
    col_var = df_test_loc$test_plot_location,
    lab_var = "Spatial overlap of test\nand training data"
  ) +
  plot_real_pred(
    data = df_var_omit,
    col_var = df_var_omit$var_omit,
    lab_var = "Covariate omission"
  ) +
  plot_layout(ncol = 3)

ggsave(here::here("output","figures","results-study-conditions.png"),
       width = 1500, height = 1500, units = "px")

