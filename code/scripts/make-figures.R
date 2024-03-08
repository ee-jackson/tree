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


# Load data ---------------------------------------------------------------

clean_data <- readRDS(
  here::here("data", "derived", "ForManSims_RCP0_same_time_clim_squ.rds")
)

all_years <-
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time.rds")
          ) %>%
  filter(description %in% clean_data$description)

nfi_coords <- readxl::read_excel(
  here::here("data", "raw", "NFI plot coords NFI 2016-2020.xlsx"),
  col_types = c("numeric", "text", "text","numeric","numeric","numeric",
                "numeric","numeric","numeric","numeric","numeric","numeric","numeric")
) %>%
  select(-"...1") %>%
  clean_names()

all_years_xy <- all_years %>%
  mutate(description = str_replace_all(description, " ", "")) %>%
  left_join(nfi_coords)

# choose plots
ids <- c("201956012030",
         "201935332050",
         "201735932051",
         "202047602081",
         "201726721121",
         "202025262061")


# Make map ----------------------------------------------------------------

map <- all_years_xy %>%
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
    labs(y = "Soil carbon") +
    scale_color_manual(values = c( "#E69F00", "#009E73", "#0072B2")) +
    scale_shape_manual(values = c(16, 17, 4)) +
    theme_classic(base_size = 7)
}

plots <- lapply(ids, plot_soilc, data = all_years_xy)


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
