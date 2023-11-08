#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: clean-and-filter.R
## Desc: Takes the raw data files from Tord and combines and filters the data.
##       Needs to be run on the cluster.
## Date: October 2023

# module load R/4.0.3

# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("janitor")


# Get data ----------------------------------------------------------------

file_names <-
  as.list(dir(path = here::here("data", "raw"),
              pattern = "ForManSims*", full.names = TRUE))

data_list <-
  lapply(X = file_names, FUN = read.csv, sep = ";", skipNul = TRUE)


names(data_list) <-
  lapply(file_names, basename)

new_data <-
  bind_rows(data_list, .id='df') %>%
  clean_names()

rm(data_list)

og_data <-
  readRDS(here::here("data", "derived", "subset_ForManSims_2016_RCP0.rds")) %>%
  mutate(df = "ForManSims_2016_RCP0.csv") %>%
  filter(control_category_name == "SetAside (Unmanaged)" |
           control_category_name == "BAU - NoThinning" |
           control_category_name == "Initial state")

bind_rows(new_data, og_data) %>%
  distinct(across(-df), .keep_all = TRUE) -> all_data

rm(new_data)

rm(og_data)


# Remove peat plots -------------------------------------------------------

all_data %>%
  filter(peat == 0) -> all_data_no_peat


# Select plots with a starting age of 40 (+/- 10 years) -------------------

# get a list of plots with a starting age of 40 (+/- 10 years)
all_data_no_peat %>%
  filter(period == 0 & age < 50 & age > 30) %>%
  select(description) %>%
  distinct() -> list_40

# filter all data to only include plots with starting age of 40
all_data_no_peat %>%
  filter(description %in% list_40$description) -> all_data_no_peat_40


# Select plots which have “treatment” and “control” -----------------------

all_data_no_peat_40 %>%
  group_by(description) %>%
  summarise(n = n_distinct(control_category_name),
            .groups = "drop") %>%
  filter(n == 3) %>%
  select(description) -> plot_list

all_data_no_peat_40 %>%
  filter(description %in% plot_list$description) -> plots_bau_sa

saveRDS(plots_bau_sa,
        file = here::here("data", "derived", "ForManSims_RCP0_all.rds"))


# Select alternative_nos which hit their peak in the same period ----------

plots_bau_sa %>%
  filter(control_category_name == "BAU - NoThinning") ->  plots_bau

plots_bau %>%
  filter(period == 12) %>%
  group_by(description, alternative_no, .drop = FALSE) %>%
  summarise(total_soil_carbon_max = max(total_soil_carbon)) %>%
  slice_max(total_soil_carbon_max, n = 1) %>%
  ungroup() %>%
  inner_join(plots_bau) -> plots_bau_12

plots_bau_sa %>%
  filter(control_category_name != "BAU - NoThinning") %>%
  bind_rows(plots_bau_12) -> test_plots


# Same carbon at period one -----------------------------------------------

test_plots %>%
  filter(period == 1) %>%
  group_by(description) %>%
  mutate(dupes = as.integer(n_distinct(total_soil_carbon) == 1)) %>%
  filter(dupes == 1) %>%
  select(description) -> list_same_start

test_plots %>%
  filter(description %in% list_same_start$description) -> test_plots


# save! -------------------------------------------------------------------

n_distinct(test_plots$description)

saveRDS(test_plots,
        file = here::here("data", "derived", "ForManSims_RCP0_same_time.rds"))
