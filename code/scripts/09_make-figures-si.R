#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-si-figures.R
## Desc: make all figures for the SI
## Date: March 2024


# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")
library("ggmap")
library("janitor")
library("factoextra")
library("ggtext")

# Load data ---------------------------------------------------------------

nfi_coords <- readxl::read_excel(
  here::here("data", "raw", "NFI_plot_coords_NFI_2016-2020.xlsx"),
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
    mutate(control_category_name =
             case_when(control_category_name == "BAU - NoThinning" ~ "Business as usual (control)",
                       control_category_name == "SetAside (Unmanaged)" ~ "Set aside (treated)",
                       .default = control_category_name)) %>%
    ggplot(aes(x = period, y = total_soil_carbon,
               colour = as.factor(control_category_name),
               shape = as.factor(control_category_name))) +
    geom_point(size = 1) +
    geom_line(linewidth = 0.3) +
    labs(y = "Soil carbon\n(ton C/ha)",
         x = "Time period") +
    scale_color_manual(values = c("#0072B2", "#009E73", "#E69F00")) +
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

ggsave(here::here("output","figures","si-soil-carbon-year.png"),
       width = 1476, height = 1600, units = "px")


# Environmental variables plot ---------------------------------------------

clean_data %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = altitude)) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Altitude (m)") -> ep1

clean_data %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = total_soil_carbon)) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Initial soil\ncarbon (ton C/ha)") -> ep2

clean_data %>%
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
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = map_5yr)) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_c() +
  coord_quickmap() +
  theme_void(base_size = 6) +
  labs(colour = "Mean annual\nprecipitation (mm)") -> ep5

clean_data %>%
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

ggsave(here::here("output","figures","si-env-var-map.png"),
       width = 1000, height = 1500, units = "px")

# logged
clean_data %>%
  filter(period == 0) %>%
  select(ost_wgs84, nord_wgs84, starts_with("volume_")) %>%
  select(- volume_beech, - volume_larch) %>%  # there is no beech, only 2 plots with larch
  pivot_longer(cols = starts_with("volume_")) %>%
  mutate(name = str_remove(name, "volume_")) %>%
  mutate(name = str_replace(name, "_", "\n")) %>%
  mutate(name = str_to_title(name)) %>%
  ggplot(aes(value,
             colour = name,
             fill = name)) +
  geom_density(
    alpha = 0.5) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(n.breaks = 3) +
  scale_x_log10() +
  theme_classic(base_size = 6) +
  theme(legend.position = "none",
        axis.title.x = element_markdown()) +
  facet_wrap(~name, ncol = 2) +
  labs(x = " log<sub>10</sub> Volume m<sup>3</sup>/ha" )

ggsave(here::here("output","figures","methods-sp-vol-log.png"),
       width = 500, height = 600, units = "px")

clean_data %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84,
             colour = no_of_stems)) +
  borders("world", regions = "sweden") +
  geom_point(shape = 16, alpha = 0.7, size = 0.5) +
  scale_colour_viridis_c(transform = "log10", breaks = c(100,400,1600,6400)) +
  coord_quickmap() +
  theme_void(base_size = 6) + labs(colour = "No. of stems") +
  theme(legend.key.width = unit(0.3, "lines"))


# test plot location fig --------------------------------------------------

feat_list <- c("total_soil_carbon", "soil_moist_code", "mat_5yr",
               "map_5yr", "altitude", "no_of_stems", "ditch",
               "volume_pine", "volume_spruce", "volume_birch",
               "volume_aspen", "volume_oak", "volume_beech",
               "volume_southern_broadleaf", "volume_contorta",
               "volume_other_broadleaf", "volume_larch")

clean_data %>%
  filter(period == 0) %>%
  mutate(sampling_location =
           recode(sampling_location,
             core = "Core",
             edge = "Edge",
             stratified = "Random",
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
             palette = c("orange", "purple", "lightgrey","forestgreen"),
             title = "") +
  theme(legend.title = element_blank()) -> pca

clean_data %>%
  filter(period == 0) %>%
  ggplot(aes(ost_wgs84, nord_wgs84, colour = sampling_location)) +
  geom_point(alpha = 0.5, shape = 16, size = 0.7) +
  scale_colour_manual(values = c("orange", "purple", "lightgrey","forestgreen")) +
  borders("world", regions = "sweden") +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none") -> pca_map

pca_map + pca

ggsave(here::here("output","figures","si-pca-map.png"),
       width = 1500, height = 1000, units = "px")
