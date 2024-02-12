#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: identify-test-plots.R
## Desc: Identifies plots to be used as test data. Plots are geographically
##       distinct but nested in covariate space.
##       See notebooks/2024-01-22_find-test-area.md
## Date: February 2024

# module load R/4.0.3

# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("sf")


# Load data ---------------------------------------------------------------

clean_data <-
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time_clim.rds")) %>%
  filter(period == 0)


# Make a grid -------------------------------------------------------------

# make data a sf object
data_sf <- st_as_sf(clean_data,
                    coords = c("ost_wgs84", "nord_wgs84"),
                    crs = "WGS84")

# project the points
data_sf %>%
  st_transform(crs = st_crs(3152)) -> data_projected

# make grid
fishnet <- st_make_grid(
  data_projected,
  cellsize = c(140000, 130000), # units are meters
  what = "polygons",
  square = TRUE,
  crs = st_crs(3152))

# convert polygons to sf object and add id column
fishnet %>%
  st_sf(crs = st_crs(3152)) %>%
  mutate(net_id = row_number()) -> fishnet_sf


# Find intersecting plots -------------------------------------------------

# calculate which plots are in which squares
joined <- st_intersection(data_projected, fishnet_sf)

# get plots in net_22
plot_list <- joined %>%
  filter(net_id == 22) %>%
  st_drop_geometry() %>%
  select(description) %>%
  distinct()

readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time_clim.rds")) %>%
  mutate(in_square = ifelse(
    description %in% plot_list$description, TRUE, FALSE
  )) %>% saveRDS(
    here::here("data", "derived", "ForManSims_RCP0_same_time_clim_squ.rds")
    )
