#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: get-climate-vars.R
## Desc: Downloads CRU TS climate data and calculates MAT and MAP for each NFI
##       plot in data/derived/ForManSims_RCP0_same_time.rds
## Date: November 2023


# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("ncdf4")
library("R.utils")
library("janitor")
library("sf")
library("zoo")


# Download data -----------------------------------------------------------

url_list <- c("https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/cruts.2304141047.v4.07/tmp/cru_ts4.07.2011.2020.tmp.dat.nc.gz",
              "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/cruts.2304141047.v4.07/tmp/cru_ts4.07.2021.2022.tmp.dat.nc.gz",
              "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/cruts.2304141047.v4.07/pre/cru_ts4.07.2011.2020.pre.dat.nc.gz",
              "https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.07/cruts.2304141047.v4.07/pre/cru_ts4.07.2021.2022.pre.dat.nc.gz")

# create destination directory
ifelse(!dir.exists(file.path(here::here("data", "raw", "climate_data"))),
       dir.create(file.path(here::here("data", "raw", "climate_data"))), FALSE)

# download files
for (url in url_list) {
  download.file(url, destfile = here::here("data", "raw", "climate_data", basename(url)))
}

file_names <- as.list(dir(path = here::here("data", "raw", "climate_data"),
                          pattern = "cru_ts4*", full.names = TRUE))

# unzip files
lapply(file_names, gunzip)

# read in data
file_names <- as.list(dir(path = here::here("data", "raw", "climate_data"),
                          pattern = "cru_ts4*", full.names = TRUE))

cru_ts4_list <- lapply(file_names, nc_open)


# Flatten data ------------------------------------------------------------

make_2d <- function(data_slice, lon, lat) {
  lonlat <- as.matrix(expand.grid(lon, lat))
  data_vec <- as.vector(data_slice)
  cbind(lonlat, data_vec) %>%
    data.frame()
}

flatten_data <- function(data, clim_var) {
  lon <- ncvar_get(data,"lon")
  lat <- ncvar_get(data,"lat")
  array <- ncvar_get(data, clim_var)
  fill_value <- ncatt_get(data, clim_var, attname = "_FillValue")

  array <- array %>%
    na_if(fill_value$value)

  date <- ncvar_get(data,"time") %>%
    as.Date(origin = "1900-1-1")

  lonlat <- as.matrix(expand.grid(lon, lat))

  data_vec <- as.vector(array)

  data_list <- apply(X = array, MARGIN = 3, FUN = make_2d,
                     lon = lon, lat = lat)

  names(data_list) <- date

  bind_rows(data_list, .id = "date") %>%
    rename({{clim_var}} := "data_vec",
           "lon" = "Var1",
           "lat" = "Var2") %>%
    drop_na()
}

cru_ts4_list_flat <- map2(.f = flatten_data,
                          cru_ts4_list,
                          c("pre", "tmp", "pre", "tmp"))

pre_data <- bind_rows(cru_ts4_list_flat[[1]], cru_ts4_list_flat[[3]])
tmp_data <- bind_rows(cru_ts4_list_flat[[2]], cru_ts4_list_flat[[4]])


# Calculate annual means --------------------------------------------------

mat_data <- tmp_data %>%
  mutate(year = substr(date, 1, 4)) %>%
  group_by(lon, lat, year) %>%
  summarise(mat_1yr = mean(tmp), .groups = "drop") %>%
  group_by(lon, lat) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(mat_5yr = zoo::rollmean(mat_1yr, 5, fill = NA, align = "right")) %>%
  ungroup() %>%
  filter(year %in% c("2016", "2017", "2018", "2019", "2020") )

map_data <- pre_data %>%
  mutate(year = substr(date, 1, 4)) %>%
  group_by(lon, lat, year) %>%
  summarise(map_1yr = mean(pre), .groups = "drop") %>%
  group_by(lon, lat) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(map_5yr = zoo::rollmean(map_1yr, 5, fill = NA, align = "right")) %>%
  ungroup() %>%
  filter(year %in% c("2016", "2017", "2018", "2019", "2020") )


# Match to NFI plots ------------------------------------------------------

nfi_coords <- readxl::read_excel(
  here::here("data", "raw", "NFI plot coords NFI 2016-2020.xlsx"),
  col_types = c("numeric", "text", "text","numeric","numeric","numeric",
    "numeric","numeric","numeric","numeric","numeric","numeric","numeric")
) %>%
  select(-"...1") %>%
  clean_names()

nfi_data <- readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time.rds")) %>%
  mutate(description = str_replace_all(description, " ", "")) %>%
  distinct(description) %>%
  inner_join(nfi_coords)

# convert coordinates to sf objects
nfi_data_sf <- st_as_sf(x = nfi_data,
                        coords = c("ost_wgs84", "nord_wgs84"),
                        crs = "WGS84", remove = FALSE)

# match mat and map based on year data collected and nearest climate station
match_data <- function(yr, plot_data, clim_data) {
  clim_data_sf <- clim_data %>%
    filter(year == yr) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = "WGS84")

  plot_data %>%
    filter(taxar == yr) %>%
    st_join(
    clim_data_sf,
    join = st_nearest_feature)
}


mat_plots <- lapply(unique(nfi_data_sf$taxar),
                    match_data,
                    plot_data = nfi_data_sf,
                    clim_data = mat_data) %>%
  bind_rows() %>%
  st_drop_geometry()

map_plots <- lapply(unique(nfi_data_sf$taxar),
                    match_data,
                    plot_data = nfi_data_sf,
                    clim_data = map_data) %>%
  bind_rows() %>%
  st_drop_geometry()

full_join(mat_plots, map_plots) %>%
  select(-year) -> plots_0


# no double thinned plots -------------------------------------------------

readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time.rds")) %>%
  filter(control_category_name == "BAU - NoThinning") %>%
  filter(period == 20 |
           period == 19) %>%
  pivot_wider(id_cols = description,
              values_from = total_soil_carbon,
              names_from = period) %>%
  mutate(jump = `20` - `19`) %>%
  filter(jump < 5 & jump > -5) %>%
  select(description) -> small_jump_plots


# only pine dominated plots ---------------------------------------------

readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time.rds")) %>%
  filter(period == 0) %>%
  mutate(prop_pine = volume_pine/ standing_volume) %>%
  filter(prop_pine >= 0.5) %>%
  select(description) -> pine_dom_plots


# filter ------------------------------------------------------------------

readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time.rds")) %>%
  filter(description %in% small_jump_plots$description) %>%
  filter(description %in% pine_dom_plots$description) %>%
  mutate(description = str_replace_all(description, " ", "")) %>%
  filter(period == 0 | period == 20) %>%
  filter(soil_moist_code != 4) %>%
  inner_join(plots_0) %>%
  group_by(description) %>%
  arrange(period, .by_group = TRUE) %>%
  ungroup() -> filtered_data


# select columns ----------------------------------------------------------

filtered_data %>%
  select(description, ost_wgs84, nord_wgs84, taxar, region, altitude, mat_5yr, map_5yr,
         period, control_category_name, total_soil_carbon, soil_moist_code,
         ditch, no_of_stems, standing_volume, volume_pine, volume_spruce,
         volume_birch, volume_aspen, volume_oak, volume_beech,
         volume_southern_broadleaf, volume_contorta, volume_other_broadleaf, volume_larch) %>%
  arrange(taxar) %>%
  arrange(description) %>%
  saveRDS(file =
            here::here("data", "derived", "ForManSims_RCP0_same_time_clim.rds"))
