#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 06_make-figures-methods.R
## Desc: Makes the small figures used in Table 1 of the methods section
## Date: April 2024

# module load R/4.3.0

# Load packages -----------------------------------------------------------

library("tidyverse")
library("here")
library("patchwork")

all_runs <-
  readRDS(here::here("data", "derived", "all_runs.rds")) %>%
  mutate(
    learner = recode_factor(
      learner,
      s = "Single model",
      t = "Two models",
      x = "Crossed models",
      .ordered = TRUE
    ),
    assignment = recode_factor(
      assignment,
      random = "Random",
      correlated_altitude = "Correlated\nwith altitude",
      correlated_region = "Correlated\nwith region",
      .ordered = TRUE
    )
  )

nfi_coords <- readxl::read_excel(
  here::here("data", "raw", "NFI_plot_coords_NFI_2016-2020.xlsx"),
  col_types = c("numeric", "text", "text","numeric","numeric","numeric",
                "numeric","numeric","numeric","numeric","numeric","numeric","numeric")
  ) %>%
  select(Description, Ost_WGS84, Nord_WGS84) %>%
  janitor::clean_names()


# treatment assignment ----------------------------------------------------

all_runs %>%
  filter(prop_not_treated == 0.5,
         n_train == 500) %>%
  group_by(assignment) %>%
  sample_n(1) %>%
  unnest(df_train) %>%
  left_join(nfi_coords, by = "description") %>%
  mutate(tr = recode_factor(tr, `0` = "Control", `1` = "Treated")) %>%
  ggplot(aes(ost_wgs84, nord_wgs84, colour = tr)) +
  borders("world", regions = "sweden", linewidth = 0.25) +
  geom_point(size = 0.25, alpha = 0.7, shape = 16) +
  coord_quickmap() +
  theme_void(base_size = 6) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.key.size = unit(0.01, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_colour_manual(values = c("Treated" = "#E69F00", "Control" = "#0072B2")) +
  facet_wrap(~ assignment, nrow = 1)


ggsave(here::here("output","figures","methods-assignment.png"),
       width = 1000, height = 500, units = "px")


# sampling imbalance ------------------------------------------------------

all_runs %>%
  filter(assignment == "Random",
         n_train == 250) %>%
  group_by(prop_not_treated) %>%
  sample_n(1) %>%
  unnest(df_train) %>%
  left_join(nfi_coords, by = "description") %>%
  mutate(tr = recode_factor(tr, `0` = "Control", `1` = "Treated")) %>%
  ggplot(aes(ost_wgs84, nord_wgs84, colour = tr)) +
  borders("world", regions = "sweden", linewidth = 0.25) +
  geom_point(size = 0.25, alpha = 0.9, shape = 16) +
  coord_quickmap() +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_colour_manual(values = c("Treated" = "#E69F00", "Control" = "#0072B2")) +
  facet_wrap(~ prop_not_treated, nrow = 1)

ggsave(here::here("output","figures","methods-prop_not_treated.png"),
       width = 1000, height = 500, units = "px")


# sample size -------------------------------------------------------------

all_runs %>%
  filter(assignment == "Random",
         prop_not_treated == 0.5) %>%
  mutate(n_train = as.factor(n_train)) %>%
  group_by(n_train) %>%
  sample_n(1) %>%
  unnest(df_train) %>%
  left_join(nfi_coords, by = "description") %>%
  mutate(tr = recode_factor(tr, `0` = "Control", `1` = "Treated")) %>%
  ggplot(aes(ost_wgs84, nord_wgs84, colour = tr)) +
  borders("world", regions = "sweden", linewidth = 0.25) +
  geom_point(size = 0.25, alpha = 0.7, shape = 16) +
  coord_quickmap() +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_colour_manual(values = c("Treated" = "#E69F00", "Control" = "#0072B2")) +
  facet_wrap(~ n_train, nrow = 1)

ggsave(here::here("output","figures","methods-n_train.png"),
       width = 1000, height = 500, units = "px")


# test data location ------------------------------------------------------

all_runs %>%
  filter(assignment == "Random",
         prop_not_treated == 0.5) %>%
  sample_n(3) %>%
  unnest(df_assigned) %>%
  group_by(sampling_location) %>%
  slice_sample(n = 162) %>%
  mutate(sampling_location = recode_factor(
    sampling_location,
    other = "Random",
    centre = "Core",
    edge = "Edge",
    .ordered = TRUE
  )) %>%
  left_join(nfi_coords, by = "description") %>%
  mutate(tr = recode_factor(tr, `0` = "Control", `1` = "Treated")) %>%
  ggplot(aes(ost_wgs84, nord_wgs84, colour = tr)) +
  borders("world", regions = "sweden", linewidth = 0.25) +
  geom_point(size = 0.25, alpha = 0.7, shape = 16) +
  coord_quickmap() +
  theme_void(base_size = 9) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_colour_manual(values = c("Treated" = "#E69F00", "Control" = "#0072B2")) +
  facet_wrap(~ sampling_location, nrow = 1)

ggsave(here::here("output","figures","methods-test_plot_location.png"),
       width = 1000, height = 500, units = "px")
