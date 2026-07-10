#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-3.R
## Desc: True vs predicted ITE surfaces across covariate space
## Date: July 2026


# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("ggtext")


# Set constants -----------------------------------------------------------

cols <- c(
  "S-learner" = "#E69F00",
  "T-learner" = "#009E73",
  "X-learner" = "#0072B2",
  "DR-learner" = "#CC79A7",
  "True ITE" = "black"
)

covariate_labels <- c(
  soil_moist_code     = "Soil moisture",
  soil_carbon_initial = "Initial soil carbon",
  no_of_stems         = "Stem density",
  mat_5yr             = "Mean annual temperature",
  map_5yr             = "Mean annual precipitation",
  volume_pine         = "Volume of pine",
  volume_spruce       = "Volume of spruce"
)


# Helper functions --------------------------------------------------------

recode_plot_vars <- function(data) {
  data %>%
    mutate(
      learner = recode_factor(
        learner,
        s = "S-learner",
        t = "T-learner",
        x = "X-learner",
        dr = "DR-learner",
        .ordered = TRUE
      ),
      assignment = recode_factor(
        assignment,
        random = "Random",
        non_random = "Non-random",
        .ordered = TRUE
      ),
      var_omit = recode_factor(
        var_omit,
        none = "No omission",
        omit_covariate = "Omit covariate",
        .ordered = TRUE
      )
    )
}


make_covariate_long <- function(data, covariates) {
  data %>%
    mutate(no_of_stems = log(no_of_stems)) %>%
    select(
      learner,
      assignment,
      var_omit,
      cate_real,
      cate_pred,
      all_of(names(covariates))
    ) %>%
    pivot_longer(
      cols = all_of(names(covariates)),
      names_to = "cov",
      values_to = "cov_val"
    ) %>%
    mutate(
      cov = factor(covariates[cov], levels = covariates)
    )
}


summarise_discrete_covariate <- function(data,
                                         covariate,
                                         response,
                                         group_vars = NULL) {

  data %>%
    filter(cov == covariate) %>%
    group_by(
      assignment,
      var_omit,
      cov,
      cov_val,
      across(all_of(group_vars))
    ) %>%
    summarise(
      y = mean(.data[[response]], na.rm = TRUE),
      .groups = "drop"
    )
}


plot_ite_surfaces <- function(data_cont,
                              true_discrete,
                              pred_discrete,
                              colours) {
  rug_data <- long_cont %>%
    distinct(assignment, var_omit, cov, cov_val)

  ggplot(mapping = aes(x = cov_val)) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey70"
    ) +
    geom_rug(
      data = rug_data,
      aes(x = cov_val),
      sides = "b",
      alpha = 0.3,
      length = unit(0.025, "npc"),
      colour = "grey30",
      inherit.aes = FALSE
    ) +
    geom_smooth(
      data = data_cont,
      aes(y = cate_real, colour = "True ITE"),
      method = "loess",
      se = FALSE,
      linewidth = 1
    ) +
    geom_smooth(
      data = data_cont,
      aes(y = cate_pred, colour = learner),
      method = "loess",
      se = FALSE,
      linewidth = 0.7
    ) +
    geom_line(
      data = true_discrete,
      aes(y = y, colour = "True ITE"),
      linewidth = 1
    ) +
    geom_line(
      data = pred_discrete,
      aes(y = y, colour = learner),
      linewidth = 0.7
    ) +
    scale_colour_manual(values = colours, name = NULL) +
    facet_grid(
      rows = vars(
        `Treatment assignment\n` = assignment,
        `Covariate omission\n` = var_omit
      ),
      cols = vars(cov),
      labeller = labeller(
        .rows = label_both,
        .cols = label_value
      ),
      scales = "free_x"
    ) +
    labs(
      x = "Covariate value",
      y = "ITE (t C ha<sup>-1</sup>)",
      subtitle = "Training sample size: 1000, Treatment imbalance: 0.3, Treatment assignment: Random",
    ) +
    theme_bw(base_size = 6) +
    theme(
      legend.position = "bottom",
      plot.tag = element_text(face = "italic"),
      plot.subtitle = element_text(face = "italic"),
      strip.text.y = element_text(angle = 270),
      axis.title.y = element_markdown(),
      legend.text = element_text(size = 6)
    )
}


# Get data ----------------------------------------------------------------

all_runs <- readRDS(here::here("data", "derived", "all_runs.rds")) %>%
  filter(
    test_plot_location == "stratified",
    prop_not_treated == 0.3,
    n_train == 1000
  )


# Prepare data ------------------------------------------------------------

dout <- all_runs %>%
  select(
    learner,
    assignment,
    var_omit,
    df_out
  ) %>%
  unnest(df_out) %>%
  recode_plot_vars()


long <- dout %>%
  make_covariate_long(covariates = covariate_labels)

long_cont <- long %>%
  filter(cov != "Soil moisture")

moist_true <- long %>%
  summarise_discrete_covariate(
    covariate = "Soil moisture",
    response = "cate_real"
  )

moist_pred <- long %>%
  summarise_discrete_covariate(
    covariate = "Soil moisture",
    response = "cate_pred",
    group_vars = "learner"
  )


# Make plot ---------------------------------------------------------------

fig_3 <- plot_ite_surfaces(
  data_cont = long_cont,
  true_discrete = moist_true,
  pred_discrete = moist_pred,
  colours = cols
)

fig_3


# Save plot ---------------------------------------------------------------

ggsave(
  here::here("output", "figures", "results-figure3.png"),
  fig_3,
  width = 2000,
  height = 1600,
  units = "px"
)
