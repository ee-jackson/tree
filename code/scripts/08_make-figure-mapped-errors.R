#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figure-mapped-errors.R
## Desc: Make a figure of S, T and X predictions from the same sample of plots
## Date: April 2024


# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("yardstick")
library("ggtext")
library("readxl")
library("maps")

set.seed(123)

# get my functions
function_dir <- list.files(here::here("code", "functions"),
                           full.names = TRUE)

sapply(function_dir, source)

clean_data <-
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time_clim_squ.rds"))


# create keys -------------------------------------------------------------

keys <- expand.grid(
  n_train = 1000,
  learner = c("s", "t", "x"),
  var_omit = FALSE,
  test_plot_location = "random"
)


# assign treatments -------------------------------------------------------

df_assigned_rand <- assign_treatment(clean_data, "random")


# sample training data ----------------------------------------------------

sample_out_1000 <- sample_data(df_assigned = df_assigned_rand,
            prop_not_treated = 0.5,
            n_train = 1000)

keys %>%
  mutate(df_train = case_when(
    n_train == 1000 ~ list(sample_out_1000)
  ) ) %>%
  mutate(df_assigned = list(df_assigned_rand)) -> keys


# fit metalearners --------------------------------------------------------

purrr::pmap(list(df_train = keys$df_train,
                 df_assigned = keys$df_assigned,
                 learner = keys$learner,
                 var_omit = keys$var_omit,
                 test_plot_location = keys$test_plot_location
),
fit_metalearner, seed = 123) -> model_out

keys %>%
  mutate(df_out = model_out) %>%
  mutate(assignment = "random",
         prop_not_treated = 0.5,
         restrict_confounder = FALSE,
         run_id = row_number()) -> all_runs

nfi_coords <- readxl::read_excel(
  here::here("data", "raw", "NFI_plot_coords_NFI_2016-2020.xlsx"),
  col_types = c("numeric", "text", "text","numeric","numeric","numeric",
                "numeric","numeric","numeric","numeric","numeric","numeric","numeric")
) %>%
  select(Description, Ost_WGS84, Nord_WGS84) %>%
  janitor::clean_names()


# make plotting functions -------------------------------------------------

make_labels <- function(dat) {
  rmse <- paste("RMSE = ", round(yardstick::rmse_vec(truth = dat$cate_real,
                                                     estimate = dat$cate_pred), 3))
  r2 <- paste("R<sup>2</sup> =", round(yardstick::rsq_vec(truth = dat$cate_real,
                                                estimate = dat$cate_pred), 3))
  data.frame(rmse = rmse, r2 = r2, stringsAsFactors = FALSE)
}

# plotting function
plot_real_pred <- function(treat_as, sample_imbalance,
                           sample_size, variable_omit, plot_location,
                           meta_learner,
                           id, out, plot_title, coord_df) {
  out %>%
    filter(
      assignment == treat_as,
      prop_not_treated == sample_imbalance,
      n_train == sample_size,
      var_omit == variable_omit,
      test_plot_location == plot_location,
      learner == meta_learner,
      restrict_confounder == FALSE
    ) -> out_subset

  out_subset %>%
    unnest(df_out) %>%
    mutate(Error = cate_pred - cate_real) %>%
    mutate(learner = str_to_upper(learner)) %>%
    left_join(coord_df, by = join_by(description)) -> plot_dat

  plot_dat %>%
    group_by(learner) %>%
    do(make_labels(.)) -> labels

  plot_dat %>%
    select(description, cate_pred, cate_real, Error) %>%
    rename(`true\nITE` = cate_real, `predicted\nITE` = cate_pred) %>%
    rowid_to_column() %>%
    pivot_longer(cols = c(`predicted\nITE`, `true\nITE`)) %>%
    mutate(name = factor(name, levels = c("true\nITE", "predicted\nITE"))) %>%
    arrange(rowid, name)  -> pivot_dat

  plot_dat %>%
    ggplot(aes(ost_wgs84, nord_wgs84, colour = Error)) +
    borders("world", regions = "sweden", linewidth = 0.25) +
    geom_point(size = 0.5, alpha = 0.9, shape = 16) +
    coord_quickmap() +
    theme_void(base_size = 6) +
    theme(legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_colour_gradientn(
      colours = colorspace::divergingx_hcl(n = 10, palette = "RdYlBu"),
      limits = c(-20, 20)) -> p3

  plot_dat %>%
    ggplot(aes(x = cate_real, y = cate_pred, colour = Error)) +
    geom_hline(yintercept = 0, colour = "grey",
               linetype = 2, linewidth = 0.25) +
    geom_vline(xintercept = 0, colour = "grey",
               linetype = 2, linewidth = 0.25) +
    geom_point(size = 0.25) +
    geom_abline(intercept = 0, slope = 1, colour = "blue", linewidth = 0.25) +
    scale_colour_gradientn(
      colours = colorspace::divergingx_hcl(n = 10, palette = "RdYlBu"),
      limits = c(-20, 20)) +
    xlim(-25, 25) +
    ylim(-25, 25) +
    theme_classic(base_size = 6) +
    # geom_richtext(data = labels, aes(label = rmse),
    #           x = -28, y = 25, hjust = 0, colour = "blue",
    #           size = 1.5, label.size = 0, fill = NA) +
    # geom_richtext(data = labels, aes(label = r2),
    #           x = -28, y = 21, hjust = 0, colour = "blue",
    #           size = 1.5, label.size = 0, fill = NA) +
    labs(y = "predicted ITE", x = "true ITE") +
    labs(title = plot_title) +
    theme(plot.title = element_text(hjust = -0.4)) -> p1


  pivot_dat %>%
    ggplot(aes(x = name, y = value, colour = Error)) +
    ggdist::stat_slab(orientation = "x", side = "both", normalize = "all") +
    geom_point(size = 0.25) +
    geom_line(aes(group = interaction(Error, rowid)),
              linewidth = 0.25,
              alpha = 0.4) +
    scale_colour_gradientn(colours = colorspace::divergingx_hcl(n = 10, palette = "RdYlBu"),
                           limits = c(-20, 20)) +
    ylim(-25, 25) +
    labs(x = "", y = "") +
    theme_classic(base_size = 6) +
    geom_hline(yintercept = 0, colour = "grey", linetype = 2) -> p2


  p1 + p2 + p3 +
    plot_layout(guides = "collect",
                widths = c(2,1,1))
}


# make plots --------------------------------------------------------------

(plot_real_pred(
  out = all_runs,
  treat_as = "random",
  sample_imbalance = 0.5,
  sample_size = 1000,
  variable_omit = FALSE,
  plot_location = "random",
  meta_learner = "s",
  plot_title = "a  S-learner",
  coord_df = nfi_coords
) ) /
  (plot_real_pred(
    out = all_runs,
    treat_as = "random",
    sample_imbalance = 0.5,
    sample_size = 1000,
    variable_omit = FALSE,
    plot_location = "random",
    meta_learner = "t",
    plot_title = "b  T-learner",
    coord_df = nfi_coords
  ) ) /
  (plot_real_pred(
    out = all_runs,
    treat_as = "random",
    sample_imbalance = 0.5,
    sample_size = 1000,
    variable_omit = FALSE,
    plot_location = "random",
    meta_learner = "x",
    plot_title = "c  X-learner",
    coord_df = nfi_coords
  ))

ggsave(here::here("output","figures","results-mapped-errors.png"),
       width = 1000, height = 1500, units = "px")
