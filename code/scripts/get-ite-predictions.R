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
  assignment = c("random", "blocked_ordered",
                 "blocked_random", "correlated_altitude"),
  prop_not_treated = c(0.3, 0.5, 0.7),
  learner = c("s", "t", "x"),
  n_train = c(250, 500, 750, 1000),
  var_omit = c(TRUE, FALSE),
  random_test_plots = c(TRUE, FALSE)
  ) %>%
  # add replicates
  slice(rep(1:n(), each = 5))


# assign treatments -------------------------------------------------------

purrr::map(
  .f = assign_treatment,
  .x = as.vector(keys$assignment),
  df = clean_data) -> assigned_data

keys %>%
  mutate(df_assigned = assigned_data) -> keys_assigned


# fit metalearners --------------------------------------------------------

purrr::pmap(list(df = keys_assigned$df_assigned,
          prop_not_treated = keys_assigned$prop_not_treated,
          learner = keys_assigned$learner,
          n_train = keys_assigned$n_train,
          var_omit = keys_assigned$var_omit,
          random_test_plots = keys_assigned$random_test_plots
), fit_metalearner) -> model_out

keys %>%
  mutate(df_out = model_out) -> keys_out

saveRDS(keys_out, here::here("data", "derived", "models_out.rds"))


# get median error --------------------------------------------------------

keys_out %>%
  unite(col = "test_id", remove = FALSE,
        assignment, prop_not_treated, n_train, learner, var_omit) %>%
  unnest(df_out) %>%
  group_by(test_id,
           assignment, prop_not_treated, n_train, learner, var_omit) %>%
  summarise(median_error = median(diff), median_abs_error = median(abs(diff)),
            .groups = "drop") -> results

saveRDS(results, here::here("data", "derived", "median_errors.rds"))
