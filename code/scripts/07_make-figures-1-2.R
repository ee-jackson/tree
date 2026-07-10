#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figures-1-2.R
## Desc: make figures 1 and 2 - performance metrics
## Date: July 2026


# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("yardstick")
library("ggtext")

set.seed(123)


# get data ----------------------------------------------------------------

results <- readRDS(here("data", "derived", "results.rds")) %>%
  mutate(
    assignment = recode_factor(
    assignment,
    random = "Random",
    non_random = "Non-random",
    .ordered = TRUE
    ),
    test_plot_location = recode_factor(
      test_plot_location,
      stratified = "Random",
      core = "Core",
      edge = "Edge",
      .ordered = TRUE
    ),
    learner = recode_factor(
      learner,
      s = "S-learner",
      t = "T-learner",
      x = "X-leaner",
      dr = "DR-leaner",
      .ordered = TRUE
    ),
    var_omit = recode_factor(
      var_omit,
      none = "No omission",
      omit_covariate = "Omit covariate",
      .ordered = TRUE
    ))


# make functions ----------------------------------------------------------

mean_sd <- function(x) {
  data.frame(
    y = mean(x, na.rm = TRUE),
    ymin = mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE),
    ymax = mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)
  )
}

plot_metric <- function(data,
                      y_var, y_lab,
                      x_var, x_lab,
                      x_breaks = waiver(),
                      x_cont = FALSE,
                      x_jitter = 0.15) {

  pos_jit <- position_jitter(width = x_jitter, height = 0, seed = 1)

  if (x_cont == TRUE) {
    data %>%
      ggplot(aes(x = .data[[x_var]],
                 y = .data[[y_var]],
                 colour = learner,
                 group = learner)) +
      # the connecting line
      stat_summary(fun = mean,
                   geom = "line",
                   linewidth = 0.3,
                   position = pos_jit) +
      # the point and linerange
      stat_summary(
        fun.data = mean_sd,
        geom = "pointrange",
        size = 0.25,
        fill = "white",
        shape = 21,
        stroke = 0.5,
        linewidth = 0.5,
        position = pos_jit
      ) +
      scale_colour_manual(values = c("#E69F00", "#009E73", "#0072B2", "#CC79A7")) +
      scale_x_continuous(breaks = x_breaks) +
      xlab(x_lab) +
      ylab(y_lab) +
      theme_classic(base_size = 6) +
      theme(axis.title.y = element_markdown(),
            legend.title = element_blank())

  } else if (x_cont == FALSE) {
    data %>%
      ggplot(aes(x = .data[[x_var]],
                 y = .data[[y_var]],
                 colour = learner,
                 group = learner)) +
      stat_summary(fun = mean,
                   geom = "line",
                   linewidth = 0.3,
                   position = pos_jit) +
      stat_summary(
        fun.data = mean_sd,
        geom = "pointrange",
        size = 0.25,
        fill = "white",
        shape = 21,
        stroke = 0.5,
        linewidth = 0.5,
        position = pos_jit
      ) +
      scale_colour_manual(values = c("#E69F00", "#009E73", "#0072B2", "#CC79A7")) +
      xlab(x_lab) +
      ylab(y_lab) +
      theme_classic(base_size = 6) +
      theme(axis.title.y = element_markdown(),
            legend.title = element_blank())
  }

}


# define y vars -----------------------------------------------------------

y_specs <- tibble(
  y_var = c(
    "autoc",
    "top_k",
    "spearman",
    "rmse"
  ),
  y_lab = c(
    "Targeting error<br>(1- AUTOC)",
    "Targeting imprecision<br>(1- top-k precision)",
    "Ranking error<br>(1- Spearman's &rho;)",
    "Estimation error<br>(RMSE)"
  )
)


# define x vars -----------------------------------------------------------

x_specs <- tibble(
  x_var = c(
    "assignment",
    "n_train",
    "prop_not_treated",
    "test_plot_location",
    "var_omit"
  ),
  x_lab = c(
    "Treatment assignment",
    "Training sample size",
    "Treatment imbalance",
    "Spatial overlap of test\nand training data",
    "Covariate omission"
  ),
  x_breaks = list(
    waiver(),
    c(0, 250, 500, 1000),
    c(0.3, 0.5, 0.7),
    waiver(),
    waiver()
  ),
  x_cont = c(
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    FALSE
  ),
  x_jitter = c(
    0.1,
    20,
    0.01,
    0.1,
    0.1
  )
)


# make figure 1 -----------------------------------------------------------

plot_specs <- expand_grid(x_specs, y_specs)

plots <- purrr::pmap(
  plot_specs,
  function(x_var, x_lab, x_breaks, x_cont, x_jitter, y_var, y_lab) {
    plot_metric(
      data = results,
      y_var = y_var,
      y_lab = y_lab,
      x_var = x_var,
      x_lab = x_lab,
      x_breaks = x_breaks,
      x_cont = x_cont,
      x_jitter = x_jitter
    )
  }
)

wrap_plots(plots, ncol = length(y_specs$y_var), guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "italic"))

ggsave(here::here("output","figures","results-figure1.png"),
       width = 2000, height = 2000, units = "px")


# make figure 2  ----------------------------------------------------------

plot_facet_metric <- function(data,
                              y_var,
                              y_lab,
                              x_var = "var_omit",
                              x_lab = "Covariate omission",
                              facet_rows = "prop_not_treated",
                              facet_cols = "assignment",
                              x_jitter = 0.1,
                              y_breaks = waiver()) {

  pos_jit <- position_jitter(width = x_jitter, height = 0, seed = 1)

  if (y_var == "spearman") {
    y_breaks = c(0.2, 0.3, 0.4)
  }

  data %>%
    ggplot(aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      colour = learner,
      group = learner
    )) +
    stat_summary(
      fun = mean,
      geom = "line",
      linewidth = 0.3,
      position = pos_jit
    ) +
    stat_summary(
      fun.data = mean_sd,
      geom = "pointrange",
      size = 0.25,
      fill = "white",
      shape = 21,
      stroke = 0.5,
      linewidth = 0.5,
      position = pos_jit
    ) +
    scale_colour_manual(values = c("#E69F00", "#009E73", "#0072B2", "#CC79A7")) +
    facet_grid(
      rows = vars(`Treatment\nimbalance` = .data[[facet_rows]]),
      cols = vars(`Treatment assignment\n` = .data[[facet_cols]]),
      labeller = label_both
    ) +
    scale_y_continuous(breaks = y_breaks) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_bw(base_size = 6) +
    theme(
      axis.title.y = element_markdown(),
      legend.title = element_blank()
    )
}

results_2 <-
  results %>%
  filter(n_train == 1000, test_plot_location == "Random")

plots_2 <- pmap(
  y_specs,
  function(y_var, y_lab) {
    plot_facet_metric(
      data = results_2,
      y_var = y_var,
      y_lab = y_lab
    )
  }
)


wrap_plots(plots_2, ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")",
                  subtitle =
                  "Training sample size: 1000, Spatial overlap of test and training data: Random") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "italic"),
        plot.subtitle = element_text(face = "italic", size = 6))

ggsave(here::here("output","figures","results-figure2.png"),
       width = 1600, height = 2000, units = "px")
