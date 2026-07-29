Trying AUTOC and Qini as criterions
================
eleanorjackson
02 July, 2026

``` r
library("tidyverse")
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library("here")
```

    ## here() starts at /Users/user/Library/CloudStorage/OneDrive-Nexus365/tree

``` r
library("patchwork")
library("ggtext")
```

``` r
results <- readRDS(here("data", "derived", "results.rds")) %>%
  mutate(
    assignment = recode_factor(
    assignment,
    random = "Random",
    correlated_altitude = "Correlated\nwith altitude",
    correlated_region = "Correlated\nwith region",
    .ordered = TRUE
    ),
    test_plot_location = recode_factor(
      test_plot_location,
      stratified = "Random",
      core = "Core",
      edge = "Edge",
      .ordered = TRUE
    ),
    learner = recode_factor(
      learner,
      s = "S-learner",
      t = "T-learner",
      x = "X-leaner",
      .ordered = TRUE
    ))
```

``` r
mean_sd <- function(x) {
  data.frame(
    y = mean(x, na.rm = TRUE),
    ymin = mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE),
    ymax = mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)
  )
}
```

``` r
# make function -----------------------------------------------------------

plot_rmse <- function(data,
                      y_var, y_lab,
                      x_var, x_lab,
                      x_breaks = wavier(),
                      x_cont = FALSE) {
  if (x_cont == TRUE) {
    data %>%
      ggplot(aes(x = x_var, y = y_var, colour = learner)) +
      stat_summary(fun = mean,
                   geom = "line",
                   linewidth = 0.3) +
      stat_summary(
        fun.data = mean_sd,
        geom = "pointrange",
        size = 0.25,
        fill = "white",
        shape = 21,
        stroke = 0.5,
        linewidth = 0.5
      ) +
      scale_colour_manual(values = c("#009E73", "#E69F00", "#0072B2")) +
      scale_x_continuous(breaks = x_breaks) +
      xlab(x_lab) +
      ylab(y_lab) +
      theme_classic(base_size = 6) +
      theme(axis.title.y = element_markdown(),
            legend.title = element_blank())

  } else if (x_cont == FALSE) {
    data %>%
      ggplot(aes(x = x_var, y = y_var, colour = learner, group = learner)) +
      stat_summary(fun = mean,
                   geom = "line",
                   linewidth = 0.3) +
      stat_summary(
        geom = "pointrange",
        fun.data = mean_sd,
        size = 0.25,
        fill = "white",
        shape = 21,
        stroke = 0.5,
        linewidth = 0.5
      ) +
      scale_colour_manual(values = c("#009E73", "#E69F00", "#0072B2")) +
      xlab(x_lab) +
      ylab(y_lab) +
      theme_classic(base_size = 6) +
      theme(axis.title.y = element_markdown(),
            legend.title = element_blank())
  }

}
```

``` r
# make plots --------------------------------------------------------------

plot_rmse(data = results,
          y_var = results$qini,
          y_lab = "Qini",
          x_var = results$assignment,
          x_lab = "Treatment assignment") +


  plot_rmse(data = results,
            y_var = results$autoc,
            y_lab = "AUTOC",
            x_var = results$assignment,
            x_lab = "Treatment assignment") +

plot_rmse(data = results,
          y_var = results$qini,
          y_lab = "Qini",
          x_var = results$n_train,
          x_lab = "Training sample size",
          x_breaks = c(0, 62, 125, 250, 500, 1000),
          x_cont = TRUE) +

  plot_rmse(data = results,
            y_var = results$autoc,
            y_lab = "AUTOC",
            x_var = results$n_train,
            x_lab = "Training sample size",
            x_breaks = c(0, 62, 125, 250, 500, 1000),
            x_cont = TRUE) +

  plot_rmse(data = results,
            y_var = results$qini,
            y_lab = "Qini",
            x_var = results$prop_not_treated,
            x_lab = "Treatment imbalance",
            x_breaks = c(0.3, 0.5, 0.7),
            x_cont = TRUE) +

  plot_rmse(data = results,
            y_var = results$autoc,
            y_lab = "AUTOC",
            x_var = results$prop_not_treated,
            x_lab = "Treatment imbalance",
            x_breaks = c(0.3, 0.5, 0.7),
            x_cont = TRUE) +

  plot_rmse(data = results,
            y_var = results$qini,
            y_lab = "Qini",
            x_var = results$test_plot_location,
            x_lab = "Spatial overlap of test\nand training data") +

  plot_rmse(data = results,
            y_var = results$autoc,
            y_lab = "AUTOC",
            x_var = results$test_plot_location,
            x_lab = "Spatial overlap of test\nand training data") +

  plot_rmse(data = results,
            y_var = results$qini,
            y_lab = "Qini",
            x_var = results$var_omit,
            x_lab = "Covariate omission") +

  plot_rmse(data = results,
            y_var = results$autoc,
            y_lab = "AUTOC",
            x_var = results$var_omit,
            x_lab = "Covariate omission") +

  plot_layout(guides = "collect", ncol = 2) +
  plot_annotation(tag_levels = "a", 
                  tag_prefix = "(",
                  tag_suffix = ")") &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "italic"),
        text = element_text(family = "Times New Roman")) 
```

![](figures/2026-07-01_qini-autoc-criterions/unnamed-chunk-5-1.png)<!-- -->
