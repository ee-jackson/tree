#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: get-ite-predictions.R
## Desc: Takes the clean data and implements functions in code/functions/
##       to generate the ITEs and median error
## Date: December 2023

# on the cluster use: module load R/4.3.0

library("tidyverse")
library("here")

set.seed(123)

# get my functions
function_dir <- list.files(here::here("code", "functions"),
                           full.names = TRUE)

sapply(function_dir, source)


clean_data <-
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time_clim_squ.rds"))


# create keys -------------------------------------------------------------

keys <- expand.grid(
  assignment = c("random", "correlated_region",
                 "correlated_altitude"),
  prop_not_treated = c(0.3, 0.5, 0.7),
  learner = c("s", "t", "x"),
  n_train = c(250, 500, 750, 1000),
  var_omit = c(TRUE, FALSE),
  test_plot_location = c("random", "edge", "centre")
  ) %>%
  # add replicates
  slice(rep(1:n(), each = 5))


# assign treatments -------------------------------------------------------

purrr::map(
  .f = assign_treatment,
  .x = as.vector(keys$assignment),
  df_clean = clean_data) -> assigned_data

keys %>%
  mutate(df_assigned = assigned_data) -> keys


# sample training data ----------------------------------------------------

purrr::pmap(list(df_assigned = keys$df_assigned,
                 prop_not_treated = keys$prop_not_treated,
                 n_train = keys$n_train
                 ),
            sample_data) -> sample_out

keys %>%
  mutate(df_train = sample_out) -> keys

# fit metalearners --------------------------------------------------------

purrr::pmap(list(df_train = keys$df_train,
                 df_assigned = keys$df_assigned,
                 learner = keys$learner,
                 var_omit = keys$var_omit,
                 test_plot_location = keys$test_plot_location
                 ),
            fit_metalearner) -> model_out

keys %>%
  mutate(df_out = model_out) %>%
  mutate(run_id = row_number()) -> keys

saveRDS(keys, here::here("data", "derived", "all_runs.rds"))
