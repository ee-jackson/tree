#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script:
## Desc:
## Date: March 2024

# module load R/4.0.3

# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("ggmap")
library("janitor")
library("factoextra")
library("vip")

# Load data ---------------------------------------------------------------

nfi_coords <- readxl::read_excel(
  here::here("data", "raw", "NFI plot coords NFI 2016-2020.xlsx"),
  col_types = c("numeric", "text", "text","numeric","numeric","numeric",
                "numeric","numeric","numeric","numeric","numeric","numeric","numeric")
) %>%
  select(-"...1") %>%
  clean_names()

clean_data <- readRDS(
  here::here("data", "derived", "ForManSims_RCP0_same_time_clim_squ.rds")
) %>%
  left_join(nfi_coords)

all_years <-
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time.rds")
          ) %>%
  mutate(description = str_replace_all(description, " ", "")) %>%
  #filter(description %in% clean_data$description) %>%
  left_join(nfi_coords)

results <- readRDS(here("data", "derived", "results.rds"))

# Make map ----------------------------------------------------------------

# choose plots
ids <- c("201956012030",
         "201935332050",
         "201735932051",
         "202047602081",
         "201726721121",
         "202025262061")

map <- all_years %>%
  filter(period == 0) %>%
  filter(description %in% ids) %>%
  mutate(lab = rep(letters[1:26],length = 6)) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             label = lab)) +
  borders("world", regions = "sweden") +
  geom_text(size = 5) +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


# Make carbon~year plots --------------------------------------------------

plot_soilc <- function(plot_id, data) {
  data %>%
    filter(description == plot_id) %>%
    mutate(Year = period + taxar) %>%
    mutate(control_category_name =
             case_when(control_category_name == "BAU - NoThinning" ~ "Business as usual",
                       control_category_name == "SetAside (Unmanaged)" ~ "Set aside",
                       .default = control_category_name)) %>%
    ggplot(aes(x = Year, y = total_soil_carbon,
               colour = as.factor(control_category_name),
               shape = as.factor(control_category_name))) +
    geom_point(size = 1) +
    geom_line(linewidth = 0.3) +
    labs(y = "Soil carbon\n(ton C/ha)") +
    scale_color_manual(values = c( "#E69F00", "#009E73", "#0072B2")) +
    scale_shape_manual(values = c(16, 17, 4)) +
    theme_classic(base_size = 8) +
    theme(legend.text = element_text(size = 8))
}

plots <- lapply(ids, plot_soilc, data = all_years)


# Combine -----------------------------------------------------------------

layout <- "
AABBBCC
DDBBBEE
FFBBBGG
"

plots[[1]] + map + plots[[2]] + plots[[3]] + plots[[4]] + plots[[5]] + plots[[6]] +
  plot_annotation(tag_levels = list(c("a", " ", "b", "c", "d", "e", "f"))) +
  plot_layout(design = layout, guides = "collect") &
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave(here::here("output","figures","soil-carbon-year.png"),
       width = 1476, height = 1600, units = "px")


# Environmental variables plot ---------------------------------------------

all_years %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = altitude)) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Altitude (m)") -> ep1

all_years %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = total_soil_carbon)) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Initial soil\ncarbon (ton C/ha)") -> ep2

all_years %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = as.ordered(soil_moist_code))) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_d() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Soil moisture") -> ep3

clean_data %>%
  select(description, mat_5yr, map_5yr) %>%
  left_join(all_years_xy) %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = mat_5yr)) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Mean annual\ntemperature (°C)") -> ep4

clean_data %>%
  select(description, mat_5yr, map_5yr) %>%
  left_join(all_years_xy) %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = map_5yr)) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Mean annual\nprecipitation (mm)") -> ep5

all_years %>%
  filter(period == 0) %>%
  mutate(region = case_when(region == 22 ~ 2,
                            region == 21 ~ 2,
                            .default = region)) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = as.ordered(region))) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_d() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Region") -> ep6

ep1 + ep2 + ep3 + ep4 + ep5 + ep6 +
  plot_layout(ncol = 2) &
  theme(legend.key.width = unit(0.3, "lines"))

ggsave(here::here("output","figures","env-var-map.png"),
       width = 1000, height = 1500, units = "px")


# test plot location fig --------------------------------------------------

feat_list <- c("total_soil_carbon", "altitude",
               "mat_5yr", "map_5yr", "ditch", "no_of_stems", "volume_pine",
               "volume_spruce", "volume_birch", "volume_aspen",
               "volume_oak", "volume_beech", "soil_moist_code",
               "volume_southern_broadleaf", "volume_larch")

clean_data %>%
  filter(period == 0) %>%
  mutate(sampling_location =
           recode(sampling_location,
             centre = "Core",
             edge = "Edge",
             other = "Other"
             )
         ) -> clean_data_0

corr_matrix <- clean_data_0 %>%
  column_to_rownames(var = "description") %>%
  select(all_of(feat_list)) %>%
  scale()

data_pca <- princomp(corr_matrix)

fviz_mca_ind(data_pca,
             geom = "point", alpha = 0.5, shape = 16, pointsize = 0.7,
             col.ind = as.factor(clean_data_0$sampling_location),
             addEllipses = FALSE,
             palette = c("orange", "purple",  "lightgrey"),
             title = "") +
  theme(legend.title = element_blank()) -> pca

clean_data %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84, colour = sampling_location)) +
  geom_point(alpha = 0.5, shape = 16, size = 0.7) +
  scale_colour_manual(values = c("orange", "purple",  "lightgrey")) +
  borders("world", regions = "sweden") +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none") -> pca_map

pca_map + pca

ggsave(here::here("output","figures","pca-map.png"),
       width = 1500, height = 1000, units = "px")


# vip across all meta-learners --------------------------------------------

get_vip_t <- function(df) {
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
                            levels = 6)

  rf_recipe <- recipe(rmse ~
                        assignment + prop_not_treated + test_plot_location +
                        n_train + var_omit + learner,
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
          n_train + var_omit + learner,
        data = df
    ) %>%
    vi()

  vi_df %>%
    mutate(Variable = case_when(
      Variable == "n_train" ~ "Training sample size",
      Variable == "var_omit" ~ "Omission of important variable",
      Variable == "assignment" ~ "Treatment assignment",
      Variable == "prop_not_treated" ~ "Treatment imbalance",
      Variable == "test_plot_location" ~ "Location of test plots",
      Variable == "learner" ~ "Meta-learner algorithm"
    ) ) %>%
    mutate(Variable = fct_relevel(Variable,
                                  c("Treatment assignment",
                                    "Treatment imbalance",
                                    "Omission of important variable",
                                    "Location of test plots",
                                    "Meta-learner algorithm",
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
      axis.text.y = element_text(colour = "black"),
      legend.position = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, .1))) +
    scale_colour_manual(values =
                          c("Training sample size" = "#E69f00",
                            "Meta-learner algorithm" = "#D55E00",
                            "Omission of important variable" = "#56B4E9",
                            "Treatment assignment" = "#0072B2",
                            "Treatment imbalance" = "#F0E442",
                            "Location of test plots" = "#009E73")) -> ps
}

vip_a <- get_vip_t(df = results)

ggsave(here::here("output","figures","meta-meta-vip-all.png"),
       width = 600, height = 600, units = "px")


# vip for s-learner -------------------------------------------------------

one_run <- readRDS(here("data", "derived", "all_runs.rds")) %>%
  filter(learner == "s",
         n_train == 1000,
         assignment == "random",
         prop_not_treated == 0.5,
         test_plot_location == "random",
         var_omit == FALSE) %>%
  slice_sample(n = 1)

get_vip_t <- function(df) {

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
                            levels = 16)

  rf_recipe <- recipe(soil_carbon_obs ~
                        tr  + soil_moist_code + mat_5yr + soil_carbon_initial +
                        map_5yr + altitude + no_of_stems  + ditch  +
                        volume_pine + volume_spruce  + volume_birch  +
                        volume_aspen  + volume_oak  + volume_beech  +
                        volume_southern_broadleaf  + volume_larch,
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
    fit(soil_carbon_obs ~
        tr  + soil_moist_code  + mat_5yr  + soil_carbon_initial  +
        map_5yr  + altitude  + no_of_stems  + ditch  +
        volume_pine  + volume_spruce  + volume_birch  +
        volume_aspen  + volume_oak  + volume_beech  +
        volume_southern_broadleaf  + volume_larch,
        data = df
    ) %>%
    vi()

  vi_df %>%
    mutate(Variable = case_when(
      Variable == "tr" ~ "Treatment",
      Variable == "soil_moist_code" ~ "Soil moisture",
      Variable == "mat_5yr" ~ "Average temperature",
      Variable == "soil_carbon_initial" ~ "Initial soil carbon",
      Variable == "map_5yr" ~ "Average precipitation",
      Variable == "altitude" ~ "Altitude",
      Variable == "no_of_stems" ~ "Number of stems",
      Variable == "ditch" ~ "Ditch",
      Variable == "volume_pine" ~ "Volume Pine",
      Variable == "volume_spruce" ~ "Volume Spruce",
      Variable == "volume_birch" ~ "Volume Birch",
      Variable == "volume_aspen" ~ "Volume Aspen",
      Variable == "volume_oak" ~ "Volume Oak",
      Variable == "volume_beech" ~ "Volume Oak",
      Variable == "volume_southern_broadleaf" ~ "Volume Southen Broadleaf",
      Variable == "volume_larch" ~ "Volume Larch"
    ) ) %>%
    ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
    geom_segment(aes(y = Variable, x = 0, xend = Importance),
                 linetype = 2, linewidth = 0.25) +
    geom_point(size = 2, aes(colour = Variable)) +
    theme_classic(base_size = 5) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_text(colour = "black"),
      legend.position = "none"
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, .1))) -> ps
}

vip_s <- get_vip_t(df = one_run$df_assigned[[1]])

ggsave(here::here("output","figures","vip-s.png"),
       width = 600, height = 600, units = "px")
