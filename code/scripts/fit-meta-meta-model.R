#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: fit-meta-meta-model.R
## Desc: Takes our `results.rds` and fits a RF to predict RMSE
## Date: March 2023

library("tidyverse")
library("here")
library("tidymodels")
library("vip")

set.seed(123)

results <- readRDS(here("data", "derived", "results.rds")) %>%
  filter(n_train != 31)


# test-train split --------------------------------------------------------

data_split <- initial_split(results, prop = 1/3)
train_data <- training(data_split)
test_data <- testing(data_split)


# tune hyperparameters ----------------------------------------------------

rf_tune <- rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", num.threads = 3) %>%
  set_mode("regression")

tree_grid <- grid_regular(mtry(c(1, 6)),
                          min_n(),
                          levels = 5)

rf_recipe <- recipe(rmse ~
                      assignment + prop_not_treated + test_plot_location +
                      n_train + learner + var_omit,
                    data = train_data)

rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_tune)

# create a set of cross-validation resamples to use for tuning
trees_folds <- vfold_cv(train_data, v = 25)

rf_tune_res <-
  tune_grid(rf_workflow,
            resamples = trees_folds,
            grid = tree_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))

best_auc <- select_best(rf_tune_res, "rmse")

final_rf <- finalize_model(
  rf_tune,
  best_auc
)


# variable importance -----------------------------------------------------

vi_df <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(rmse ~
        assignment + prop_not_treated + test_plot_location +
        n_train + learner + var_omit,
      data = results
  ) %>%
  vi()

vi_df %>%
  mutate(Variable = case_when(
    Variable == "n_train" ~ "Training sample size",
    Variable == "learner" ~ "Meta-learner",
    Variable == "var_omit" ~ "Omission of important variable",
    Variable == "assignment" ~ "Treatment assignment",
    Variable == "prop_not_treated" ~ "Treatment imbalance",
    Variable == "test_plot_location" ~ "Location of test plots",
  ) ) %>%
vip(geom = "point", aesthetics = list(size = 3)) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.y = element_text(colour = "black")
  )

ggsave(here::here("output","figures","meta-meta-vip.png"),
       width = 1476, height = 1000, units = "px")

# fit model ---------------------------------------------------------------

final_wf <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(data_split)

final_res %>%
  collect_metrics()
