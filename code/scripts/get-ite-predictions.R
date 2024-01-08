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
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time_clim.rds"))


# create keys -------------------------------------------------------------

keys <- expand.grid(
  assignment = c("random", "blocked", "correlated"),
  proportion_not_treated = c(0.5, 0.25, 0.75),
  learner = c("s", "t", "x"),
  n_train = c(100, 200, 400, 800, 1600),
  var_omit = c(TRUE, FALSE)
  ) %>%
  # proportion_not_treated is not applicable when assignment is blocked
  filter(
    case_when(
      assignment == "blocked" & !proportion_not_treated == 0.5 ~ FALSE,
      .default = TRUE
    )
  ) %>%
  # add replicates
  slice(rep(1:n(), each = 3))


# assign treatments -------------------------------------------------------

purrr::map2(
  .f = assign_treatment,
  .x = as.vector(keys$assignment),
  .y = as.vector(keys$proportion_not_treated),
  df = clean_data) -> assigned_data

keys %>%
  mutate(df_assigned = assigned_data) -> keys_assigned


# fit metalearners --------------------------------------------------------

purrr::pmap(list(df = keys_assigned$df_assigned,
          learner = keys_assigned$learner,
          n_train = keys_assigned$n_train,
          var_omit = keys_assigned$var_omit
), fit_metalearner) -> model_out

keys %>%
  mutate(df_out = model_out) -> keys_out

saveRDS(keys_out, here::here("data", "derived", "models_out.rds"))

# get median error --------------------------------------------------------

keys_out %>%
  unite(col = "test_id", remove = FALSE,
        assignment, proportion_not_treated, n_train, learner, var_omit) %>%
  unnest(df_out) %>%
  group_by(test_id,
           assignment, proportion_not_treated, n_train, learner, var_omit) %>%
  summarise(median_error = median(diff),
            .groups = "drop") -> results

saveRDS(results, here::here("data", "derived", "results.rds"))
