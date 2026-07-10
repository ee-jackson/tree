#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: get-ite-predictions.R
## Desc: Takes the clean data and implements functions in code/functions/
##       to generate the ITEs and median error.
##       Takes ~ 5hrs to run on high-performance computing cluster

library("tidyverse")
library("tidymodels")
library("here")
library("ranger")

# Optional: avoid accidental nested parallelism when running many simulations.
# Parallelise over rows of `keys` instead of inside each random forest if needed.
options(ranger.num.threads = 1)


# get my functions --------------------------------------------------------

function_dir <- list.files(here::here("code", "functions"),
                           full.names = TRUE)

sapply(function_dir, source)

clean_data <-
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time_clim_squ.rds"))


# fixed learner settings --------------------------------------------------
# For the full simulation, avoid tuning each base learner.

rf_settings <- list(
  trees = 500,
  mtry = 4,
  min_n = 5,
  num_threads = 1,
  trim = 0.01,
  return_model = FALSE
)


# create keys -------------------------------------------------------------

keys <- expand.grid(
  assignment = c("random", "non_random"),
  prop_not_treated = c(0.3, 0.5, 0.7),
  n_train = c(250, 500, 1000),
  learner = c("s", "t", "x", "dr"),
  var_omit = c("none", "omit_covariate"),
  test_plot_location = c("stratified", "edge", "core")
) %>%
  # add replicates
  slice(rep(1:n(), each = 50)) %>%
  mutate(run_id = row_number())


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
                 n_train = keys$n_train),
            sample_data) -> sample_out

keys %>%
  mutate(df_train = sample_out) -> keys


# fit metalearners --------------------------------------------------------

purrr::pmap(
  list(
    df_train = keys$df_train,
    df_assigned = keys$df_assigned,
    learner = keys$learner,
    var_omit = keys$var_omit,
    test_plot_location = keys$test_plot_location,
    seed = keys$run_id
  ),
  fit_metalearner,
  trees = rf_settings$trees,
  mtry = rf_settings$mtry,
  min_n = rf_settings$min_n,
  num_threads = rf_settings$num_threads,
  trim = rf_settings$trim,
  return_model = rf_settings$return_model
) -> model_out


keys %>%
  mutate(df_out = model_out) %>%
  select(- c(df_train, df_assigned)) -> keys

saveRDS(keys, here::here("data", "derived", "all_runs.rds"))
