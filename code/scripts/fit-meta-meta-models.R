#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: fit-meta-meta-model.R
## Desc: Takes our `results.rds` and fits a RF to predict RMSE
## Date: March 2023

library("tidyverse")
library("here")
library("tidymodels")
library("vip")
library("patchwork")

set.seed(123)

results <- readRDS(here("data", "derived", "results.rds"))

get_vip <- function(df, plot_title, show_y_var) {
  df <- df %>%
    filter(restrict_confounder == FALSE)

  # test-train split
  data_split <- initial_split(df, prop = 1/3)
  train_data <- training(data_split)
  test_data <- testing(data_split)

  # tune hyperparameters
  rf_tune <- rand_forest(mtry = tune(), min_n = tune()) %>%
    set_engine("ranger", num.threads = 3) %>%
    set_mode("regression")

  tree_grid <- grid_regular(mtry(c(1, 5)),
                            min_n(),
                            levels = 5)

  rf_recipe <- recipe(rmse ~
                        assignment + prop_not_treated + test_plot_location +
                        n_train + var_omit,
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

  # variable importance
  vi_df <- final_rf %>%
    set_engine("ranger", importance = "permutation") %>%
    fit(rmse ~
          assignment + prop_not_treated + test_plot_location +
          n_train + var_omit,
        data = df
    ) %>%
    vi()

  if (show_y_var == FALSE){
    vi_df %>%
      mutate(Variable = case_when(
        Variable == "n_train" ~ "Training sample size",
        Variable == "var_omit" ~ "Omission of important variable",
        Variable == "assignment" ~ "Treatment assignment",
        Variable == "prop_not_treated" ~ "Treatment imbalance",
        Variable == "test_plot_location" ~ "Location of test plots"
      ) ) %>%
      mutate(Variable = fct_relevel(Variable,
                                    c("Treatment assignment",
                                      "Treatment imbalance",
                                      "Omission of important variable",
                                      "Location of test plots",
                                      "Training sample size"
                                    ))
      ) %>%
      ggplot(aes(y = Variable, x = Importance)) +
      geom_segment(aes(y = Variable, x = 0, xend = Importance),
                   linetype = 2, linewidth = 0.25) +
      geom_point(size = 2, aes(colour = Variable)) +
      theme_classic(base_size = 5) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none"
      ) +
      scale_x_continuous(expand = expansion(mult = c(0, .1))) +
      scale_colour_manual(values =
                            c("Training sample size" = "#E69f00",
                              "Omission of important variable" = "#56B4E9",
                              "Treatment assignment" = "#0072B2",
                              "Treatment imbalance" = "#F0E442",
                              "Location of test plots" = "#009E73")) +
      labs(title = plot_title) -> ps

    } else {
      vi_df %>%
        mutate(Variable = case_when(
          Variable == "n_train" ~ "Training sample size",
          Variable == "var_omit" ~ "Omission of important variable",
          Variable == "assignment" ~ "Treatment assignment",
          Variable == "prop_not_treated" ~ "Treatment imbalance",
          Variable == "test_plot_location" ~ "Location of test plots"
        ) ) %>%
        mutate(Variable = fct_relevel(Variable,
                                      c("Treatment assignment",
                                        "Treatment imbalance",
                                        "Omission of important variable",
                                        "Location of test plots",
                                        "Training sample size"
                                      ))
        ) %>%
        ggplot(aes(y = Variable, x = Importance)) +
        geom_segment(aes(y = Variable, x = 0, xend = Importance),
                     linetype = 2, linewidth = 0.2) +
        geom_point(size = 2, aes(colour = Variable)) +
        theme_classic(base_size = 5) +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_text(colour = "black"),
          legend.position = "none"
        ) +
        scale_x_continuous(expand = expansion(mult = c(0, .1))) +
        scale_colour_manual(values =
                              c("Training sample size" = "#E69f00",
                                "Omission of important variable" = "#56B4E9",
                                "Treatment assignment" = "#0072B2",
                                "Treatment imbalance" = "#F0E442",
                                "Location of test plots" = "#009E73")) +
        labs(title = plot_title) -> ps

    }

  return(ps)
}


vip_s <- get_vip(df = filter(results, learner == "s"),
                 plot_title = "Single model",
                 show_y_var = TRUE)

vip_t <- get_vip(df = filter(results, learner == "t"),
                 plot_title = "Two models",
                 show_y_var = FALSE)

vip_x <- get_vip(df = filter(results, learner == "x"),
                 plot_title = "Crossed models",
                 show_y_var = FALSE)

vip_s + vip_t + vip_x +
  patchwork::plot_layout(guides = "collect")

ggsave(here::here("output","figures","meta-meta-vip-stx.png"),
       width = 1476, height = 600, units = "px")
