#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: plot-all-true-vs-predictions.R
## Desc: Make large PDF with plots for every combination of study conditions
## Date: March 2024

# module load R/4.3.0

# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("yardstick")
library("ggtext")

all_runs <-
  readRDS(here::here("data", "derived", "all_runs.rds")) %>%
  filter(restrict_confounder == FALSE)

keys <- expand.grid(
  assignment = c("random", "correlated_region",
                 "correlated_altitude"),
  prop_not_treated = c(0.3, 0.5, 0.7),
  n_train = c(62, 125, 250, 500, 1000),
  var_omit = c(TRUE, FALSE),
  test_plot_location = c("random", "edge", "core")) %>%
  rowid_to_column(var = "id")

# function to make rmse and r2 labels
make_labels <- function(dat) {
  rmse <- paste("RMSE = ", round(yardstick::rmse_vec(truth = dat$cate_real,
                                                 estimate = dat$cate_pred), 3))
  r2 <- paste("R<sup>2</sup> =", round(yardstick::rsq_vec(truth = dat$cate_real,
                                                          estimate = dat$cate_pred), 3))
  data.frame(rmse = rmse, r2 = r2, stringsAsFactors = FALSE)
}

# plotting function
plot_real_pred <- function(treat_as, sample_imbalance,
                           sample_size, variable_omit, plot_location,
                           id, out, max_col) {
  out %>%
    filter(
      assignment == treat_as,
      prop_not_treated == sample_imbalance,
      n_train == sample_size,
      var_omit == variable_omit,
      test_plot_location == plot_location,
      restrict_confounder == FALSE
    ) -> out_subset

  out_subset %>%
    unnest(df_out) %>%
    mutate(Error = cate_pred - cate_real) -> plot_dat

  plot_dat %>%
    group_by(learner) %>%
    do(make_labels(.)) -> labels

  plot_dat %>%
    select(description, cate_pred, cate_real, learner, Error) %>%
    rename(`true ITE` = cate_real, `predicted ITE` = cate_pred) %>%
    rowid_to_column() %>%
    pivot_longer(cols = c(`predicted ITE`, `true ITE`)) %>%
    mutate(name = factor(name, levels = c("true ITE", "predicted ITE"))) %>%
    arrange(rowid, name)  -> pivot_dat

  plot_dat %>%
    ggplot(aes(x = cate_real, y = cate_pred, colour = Error)) +
    geom_hline(yintercept = 0, colour = "grey", linetype = 2) +
    geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
    geom_point(size = 0.25) +
    geom_abline(intercept = 0, slope = 1, colour = "blue", linewidth = 0.25) +
    scale_colour_gradientn(colours = colorspace::divergingx_hcl(n = 10,palette = "RdYlBu"),
                           limits = c(-50, 50)) +
    xlim(-40, 40) +
    ylim(-40, 40) +
    theme_classic(base_size = 7) +
    facet_wrap(~ learner) +
    geom_richtext(data = labels, aes(label = rmse),
                  x = -44, y = 40, hjust = 0, colour = "blue",
                  label.colour = NA, size = 2, label.size = 0, fill = NA) +
    geom_richtext(data = labels, aes(label = r2),
                  x = -44, y = 34, hjust = 0, colour = "blue",
                  label.colour = NA, size = 2, label.size = 0, fill = NA) +
    labs(y = "predicted ITE", x = "true ITE",
         subtitle = paste("Selection bias = ", treat_as, ", ",
                          "Sample imbalance = ", sample_imbalance, ", ",
                          "n = ", sample_size, ", ",
                          "Covariate omission = ", variable_omit, ", ",
                          "Test data location = ", plot_location,
                          sep = ""))-> p1


  pivot_dat %>%
    ggplot(aes(x = name, y = value, colour = Error)) +
    ggdist::stat_slab(orientation = "x", side = "both", normalize = "groups") +
    geom_point(size = 0.25) +
    geom_line(aes(group = interaction(Error, rowid)),
              linewidth = 0.25,
              alpha = 0.4) +
    scale_colour_gradientn(colours = colorspace::divergingx_hcl(n = 10, palette = "RdYlBu"),
                           limits = c(-50, 50)) +
    ylim(-40, 40) +
    labs(x = "", y = "") +
    theme_classic(base_size = 7) +
    geom_hline(yintercept = 0, colour = "grey", linetype = 2) +
    facet_wrap(~ learner) -> p2


  p1 / p2 +
    plot_layout(heights = c(2, 2), guides = "collect")
}

all_runs %>%
  mutate(learner = case_when(
    learner == "s" ~ "S-learner",
    learner == "t" ~ "T-learner",
    learner == "x" ~ "X-learner")) %>%
  mutate(learner = fct_relevel(learner,
                               c("S-learner",
                               "T-learner",
                               "X-learner")
                             )
         ) -> all_runs_rlvl

# test one plot
plot_real_pred(
  out = all_runs_rlvl,
  treat_as = "random",
  sample_imbalance = 0.3,
  sample_size = 62,
  variable_omit = TRUE,
  plot_location = "random",
  max_col = 55
)

# create many plots
plot_list <- purrr::pmap(
  list(
    treat_as = keys$assignment,
    sample_imbalance = keys$prop_not_treated,
    sample_size = keys$n_train,
    variable_omit = keys$var_omit,
    plot_location = keys$test_plot_location,
    id = keys$id
  ),
  plot_real_pred,
  out = all_runs_rlvl,
  max_col = 55,
  .progress = TRUE
)

# split into groups of 4 plots
plot_list_split <-
  split(plot_list, rep(
    seq_along(plot_list),
    each = 4,
    length.out = length(plot_list)
  ))

# wrap 6 plots in each group for 1 pdf page
plots_wrapped <-
  purrr::map(
    .x = plot_list_split,
    .f = wrap_plots,
    nrow = 2,
    ncol = 2
  )

# make pdf - slow to run!
pdf(
  width = 12,
  height = 8,
  file = here::here("output", "figures", "all-true-v-pred.pdf")
)
plots_wrapped
dev.off()
