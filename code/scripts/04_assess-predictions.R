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
  unnest(autoc)


# Spearman's rank correlation ---------------------------------------------

all_runs <- all_runs %>%
  mutate(spearman = purrr::map(
    .x = df_out,
    .f = ~ cor(x = .x$cate_real,
               y = .x$cate_pred,
               method = "spearman")
  )) %>%
  unnest(spearman)


# Precision at k ----------------------------------------------------------

all_runs <- all_runs %>%
  mutate(top_k = purrr::map(
    .x = df_out,
    .f = ~ top_k_vec(truth = .x$cate_real,
                     estimate = .x$cate_pred)
  )) %>%
  unnest(top_k)


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
