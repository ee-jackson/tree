#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: assess-predictions.R
## Desc: Takes the output of get-ite-predictions.R and calculates median
##       propensity scores, RMSE and median absolute errors
## Date: February 2024

library("tidyverse")
library("yardstick")
library("here")

all_runs <-
  readRDS(here::here("data", "derived", "all_runs.rds"))


# propensity score --------------------------------------------------------

get_ps <- function(df_train, var_omit) {

  ditch_lvls <- df_train %>%
    summarise(n = n_distinct(ditch))

  if (var_omit == FALSE & ditch_lvls$n > 1) {

    mod <- glm(as.factor(tr) ~
                 soil_carbon_initial +
                 as.ordered(soil_moist_code) +
                 altitude +
                 mat_5yr +
                 map_5yr +
                 as.factor(ditch) +
                 no_of_stems +
                 volume_pine +
                 volume_spruce +
                 volume_birch +
                 volume_aspen +
                 volume_oak +
                 volume_beech +
                 volume_southern_broadleaf +
                 volume_larch,
               family = binomial(),
               data = df_train)

  } else if (var_omit == TRUE & ditch_lvls$n > 1) {

    mod <- glm(as.factor(tr) ~
                 as.ordered(soil_moist_code) +
                 altitude +
                 mat_5yr +
                 map_5yr +
                 as.factor(ditch) +
                 no_of_stems +
                 volume_pine +
                 volume_spruce +
                 volume_birch +
                 volume_aspen +
                 volume_oak +
                 volume_beech +
                 volume_southern_broadleaf +
                 volume_larch,
               family = binomial(),
               data = df_train)

  } else if (var_omit == FALSE & ditch_lvls$n == 1) {

    mod <- glm(as.factor(tr) ~
                 soil_carbon_initial +
                 as.ordered(soil_moist_code) +
                 altitude +
                 mat_5yr +
                 map_5yr +
                 no_of_stems +
                 volume_pine +
                 volume_spruce +
                 volume_birch +
                 volume_aspen +
                 volume_oak +
                 volume_beech +
                 volume_southern_broadleaf +
                 volume_larch,
               family = binomial(),
               data = df_train)

  } else if (var_omit == TRUE & ditch_lvls$n == 1) {

    mod <- glm(as.factor(tr) ~
                 as.ordered(soil_moist_code) +
                 altitude +
                 mat_5yr +
                 map_5yr +
                 no_of_stems +
                 volume_pine +
                 volume_spruce +
                 volume_birch +
                 volume_aspen +
                 volume_oak +
                 volume_beech +
                 volume_southern_broadleaf +
                 volume_larch,
               family = binomial(),
               data = df_train)

  } else {

    print("`var_omit` should be either `TRUE` or `FALSE`")

  }

  ps_out <- df_train %>%
    mutate(propensity_score = predict(mod, type = "response")) %>%
    filter(tr == 1) %>%
    summarise(mean_ps = mean(propensity_score),
              median_ps = median(propensity_score))

  return(ps_out)
}


ps_out <- purrr::pmap(list(df_train = all_runs$df_train,
                           var_omit = all_runs$var_omit),
                      get_ps)

all_runs <- ps_out %>%
  bind_rows() %>%
  bind_cols(all_runs)


# RMSE --------------------------------------------------------------------

all_runs <- all_runs %>%
  mutate(rmse = purrr::map(
    .x = df_out,
    .f = ~ yardstick::rmse_vec(truth = .x$cate_real,
                               estimate = .x$cate_pred)
  )) %>%
  unnest(rmse)


# median error ------------------------------------------------------------

get_error <- function(df_out) {
  df_out %>%
    mutate(error = cate_pred - cate_real) %>%
    summarise(median_error = median(abs(error)),
              mean_error = mean(abs(error)))
}

error_out <- purrr::map(.x = all_runs$df_out,
                      get_error)

all_runs <- error_out %>%
  bind_rows() %>%
  bind_cols(all_runs)


# tidy --------------------------------------------------------------------

all_runs %>%
  select(run_id,
         assignment,
         prop_not_treated,
         n_train,
         var_omit,
         test_plot_location,
         learner,
         mean_ps,
         median_ps,
         median_error,
         mean_error,
         rmse) %>%
  saveRDS(here::here("data", "derived", "results.rds"))
