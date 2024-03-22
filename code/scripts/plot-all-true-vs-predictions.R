#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script:
## Desc:
## Date: March 2024

# module load R/4.3.0

# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("yardstick")

all_runs <-
  readRDS(here::here("data", "derived", "all_runs.rds"))

keys <- expand.grid(
  assignment = c("random", "correlated_region",
                 "correlated_altitude"),
  prop_not_treated = c(0.3, 0.5, 0.7),
  n_train = c(62, 125, 250, 500, 1000),
  var_omit = c(TRUE, FALSE),
  test_plot_location = c("random", "edge", "centre")
) %>%
  rowid_to_column(var = "id")

# function to make rmse and r2 labels
make_labels <- function(dat) {
  rmse <- paste("RMSE = ", round(yardstick::rmse_vec(truth = dat$cate_real,
                                                 estimate = dat$cate_pred), 3))
  r2 <- paste("R^2 =", round(yardstick::rsq_vec(truth = dat$cate_real,
                                                           estimate = dat$cate_pred), 3))
  data.frame(rmse = rmse, r2 = r2, stringsAsFactors = FALSE)
}

# plotting function
plot_real_pred <- function(treat_as, sample_imbalance,
                           sample_size, variable_omit, plot_location, id,
                           out, max_col) {
  out %>%
    filter(
      assignment == treat_as,
      prop_not_treated == sample_imbalance,
      n_train == sample_size,
      var_omit == variable_omit,
      test_plot_location == plot_location
    ) -> out_subset

  out_subset %>%
    unnest(df_out) %>%
    mutate(error = abs(cate_pred - cate_real)) -> plot_dat

  plot_dat %>%
    group_by(learner) %>%
    do(make_labels(.)) -> labels

  plot_dat %>%
    ggplot(aes(x = cate_real, y = cate_pred, colour = error)) +
    geom_hline(yintercept = 0, colour = "grey", linetype = 2) +
    geom_vline(xintercept = 0, colour = "grey", linetype = 2) +
    geom_point(size = 0.25) +
    geom_abline(intercept = 0, slope = 1, colour = "blue", linewidth = 0.25) +
    scale_color_gradient(low = "lightblue", high = "red3",
                         limits = c(0, max_col)) +
    xlim(-40, 40) +
    ylim(-40, 40) +
    theme_classic(base_size = 7) +
    labs(subtitle = paste("assignment = ", treat_as, ", ",
                          "sampling imbalance = ", sample_imbalance, ", ",
                          "n = ", sample_size, ", ",
                          "variable omission = ", variable_omit, ", ",
                          "test data location = ", plot_location,
                          sep = "")) +
    facet_wrap(~ learner) +
    geom_text(data = labels, aes(label = rmse),
              x = -40, y = 40, hjust = 0, colour = "blue", size = 2) +
    geom_text(data = labels, aes(label = r2),
              x = -40, y = 36, hjust = 0, colour = "blue", size = 2)
}

# test one plot
plot_real_pred(
  out = all_runs,
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
  out = all_runs,
  max_col = 55,
  .progress = TRUE
)

# split into groups of 6 plots
plot_list_split <-
  split(plot_list, rep(
    seq_along(plot_list),
    each = 6,
    length.out = length(plot_list)
  ))

# wrap 6 plots in each group for 1 pdf page
plots_wrapped <-
  purrr::map(
    .x = plot_list_split,
    .f = wrap_plots,
    nrow = 3,
    ncol = 2
  )

# make pdf
pdf(
  width = 12,
  height = 8,
  file = here::here("output", "figures", "all-true-v-pred.pdf")
)
plots_wrapped
dev.off()
