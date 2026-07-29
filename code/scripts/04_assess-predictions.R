#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: assess-predictions.R
## Desc: Takes the output of get-ite-predictions.R and calculates
##       AUTOC, RMSE, top-k precision, spearman and median absolute errors
## Date: February 2024

library("tidyverse")
library("yardstick")
library("here")

# get my functions
function_dir <- list.files(here::here("code", "functions"),
                           full.names = TRUE)

sapply(function_dir, source)

# get data
all_runs <-
  readRDS(here::here("data", "derived", "all_runs.rds"))


# RMSE --------------------------------------------------------------------

all_runs <- all_runs %>%
  mutate(rmse = purrr::map(
    .x = df_out,
    .f = ~ yardstick::rmse_vec(truth = .x$cate_real,
                               estimate = .x$cate_pred)
  )) %>%
  unnest(rmse)


# AUTOC -------------------------------------------------------------------

all_runs <- all_runs %>%
  mutate(autoc = purrr::map(
    .x = df_out,
    .f = ~ autoc_norm_vec(truth = .x$cate_real,
                          estimate = .x$cate_pred)
  )) %>%
  unnest(autoc) %>%
  mutate(autoc = 1 - autoc)


# Spearman's rank correlation ---------------------------------------------

all_runs <- all_runs %>%
  mutate(spearman = purrr::map(
    .x = df_out,
    .f = ~ cor(x = .x$cate_real,
               y = .x$cate_pred,
               method = "spearman")
  )) %>%
  unnest(spearman) %>%
  mutate(spearman = 1 - spearman)


# Precision at k ----------------------------------------------------------

all_runs <- all_runs %>%
  mutate(top_k = purrr::map(
    .x = df_out,
    .f = ~ top_k_vec(truth = .x$cate_real,
                     estimate = .x$cate_pred)
  )) %>%
  unnest(top_k) %>%
  mutate(top_k = 1 - top_k)


# median error ------------------------------------------------------------

all_runs <- all_runs %>%
  mutate(
    df_out = map(df_out, ~ mutate(.x, error = cate_pred - cate_real)),
    median_error = map_dbl(df_out, ~ median(abs(.x$error), na.rm = TRUE)),
    mean_error   = map_dbl(df_out, ~ mean(abs(.x$error), na.rm = TRUE))
  )


# tidy --------------------------------------------------------------------

all_runs %>%
  select(run_id,
         assignment,
         prop_not_treated,
         n_train,
         var_omit,
         test_plot_location,
         learner,
         median_error,
         mean_error,
         rmse,
         autoc,
         spearman,
         top_k) %>%
  saveRDS(here::here("data", "derived", "results.rds"))


# make summary output -----------------------------------------------------

# mean and sd for each performance metric, across every comb of study conditions

fmt3 <- function(x) format(round(x, digits = 3), nsmall = 3)

summary_table <- all_runs %>%
  select(-df_out) %>%
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
      x = "X-learner",
      dr = "DR-learner",
      .ordered = TRUE
    ),
    var_omit = recode_factor(
      var_omit,
      none = "No omission",
      omit_covariate = "Omit covariate",
      .ordered = TRUE
    )
  ) %>%
  group_by(
    assignment,
    prop_not_treated,
    n_train,
    var_omit,
    test_plot_location,
    learner
  ) %>%
  summarise(
    across(
      c(rmse, autoc, spearman, top_k),
      list(
        mean = ~ fmt3(mean(.x, na.rm = TRUE)),
        sd = ~ fmt3(sd(.x, na.rm = TRUE)),
        ci95 = ~ {
          n <- sum(!is.na(.x))
          m <- mean(.x, na.rm = TRUE)
          s <- sd(.x, na.rm = TRUE)
          se <- s / sqrt(n)
          tcrit <- qt(0.975, df = n - 1)
          paste0(
            "[",
            fmt3(m - tcrit * se),
            ", ",
            fmt3(m + tcrit * se),
            "]"
          )
        }
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  select(
    assignment,
    n_train,
    prop_not_treated,
    test_plot_location,
    learner,
    var_omit,
    autoc_mean, autoc_sd, autoc_ci95,
    top_k_mean, top_k_sd, top_k_ci95,
    spearman_mean, spearman_sd, spearman_ci95,
    rmse_mean, rmse_sd, rmse_ci95
  ) %>%
  mutate(`n Virtual studies` = 50) %>%
  rename(
    `Estimation error mean` = rmse_mean,
    `Estimation error SD` = rmse_sd,
    `Estimation error 95% CI` = rmse_ci95,
    `Ranking error mean` = spearman_mean,
    `Ranking error SD` = spearman_sd,
    `Ranking error 95% CI` = spearman_ci95,
    `Targeting imprecision mean` = top_k_mean,
    `Targeting imprecision SD` = top_k_sd,
    `Targeting imprecision 95% CI` = top_k_ci95,
    `Targeting error mean` = autoc_mean,
    `Targeting error SD` = autoc_sd,
    `Targeting error 95% CI` = autoc_ci95,
    `Treatment assignment` = assignment,
    `Training sample size` = n_train,
    `Treatment imbalance` = prop_not_treated,
    `Spatial overlap of test and training data` = test_plot_location,
    `Covariate omission` = var_omit,
    `Meta-learner` = learner
  )

write_csv(summary_table, here::here("output", "results", "summary_results.csv"))


# -------------------------------------------------------------------------

# now for each meta-leaner across all virtual studies

summary_table2 <- all_runs %>%
  select(-df_out) %>%
  mutate(
    learner = recode_factor(
      learner,
      s = "S-learner",
      t = "T-learner",
      x = "X-learner",
      dr = "DR-learner",
      .ordered = TRUE
    ),
    var_omit = recode_factor(
      var_omit,
      none = "No omission",
      omit_covariate = "Omit covariate",
      .ordered = TRUE
    )) %>%
  group_by(
    learner
  ) %>%
  summarise(
    across(
      c(rmse, autoc, spearman, top_k),
      list(
        mean = ~ fmt3(mean(.x, na.rm = TRUE)),
        sd = ~ fmt3(sd(.x, na.rm = TRUE)),
        ci95 = ~ {
          n <- sum(!is.na(.x))
          m <- mean(.x, na.rm = TRUE)
          s <- sd(.x, na.rm = TRUE)
          se <- s / sqrt(n)
          tcrit <- qt(0.975, df = n - 1)
          paste0(
            "[",
            fmt3(m - tcrit * se),
            ", ",
            fmt3(m + tcrit * se),
            "]"
          )
        }
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  select( c(
    learner,
    autoc_mean, autoc_sd, autoc_ci95,
    top_k_mean, top_k_sd, top_k_ci95,
    spearman_mean, spearman_sd, spearman_ci95,
    rmse_mean, rmse_sd, rmse_ci95
  )) %>%
  rename(
    `Estimation error mean` = rmse_mean,
    `Estimation error SD` = rmse_sd,
    `Estimation error 95% CI` = rmse_ci95,
    `Ranking error mean` = spearman_mean,
    `Ranking error SD` = spearman_sd,
    `Ranking error 95% CI` = spearman_ci95,
    `Targeting imprecision mean` = top_k_mean,
    `Targeting imprecision SD` = top_k_sd,
    `Targeting imprecision 95% CI` = top_k_ci95,
    `Targeting error mean` = autoc_mean,
    `Targeting error SD` = autoc_sd,
    `Targeting error 95% CI` = autoc_ci95,
    `Meta-learner` = learner
  )

write_csv(summary_table2, here::here("output", "results", "summary_results2.csv"))

