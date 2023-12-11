Testing the `assign_treatment` function
================
eleanorjackson
11 December, 2023

``` r
library("tidyverse")
library("patchwork")
library("here")
set.seed(123)
```

``` r
function_dir <- list.files(here::here("code", "functions"),
                           full.names = TRUE)

sapply(function_dir, source)
```

``` r
data <-
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time_clim.rds"))
```

``` r
data_random <- 
  assign_treatment(df = data, assignment = "random")

data_blocked <-
  assign_treatment(df = data, assignment = "blocked")

data_correlated <-
  assign_treatment(df = data, assignment = "correlated")
```

``` r
plot_altitude <- function(df, name) {
  ggplot(data = df) +
   geom_density(aes(x = altitude, 
                   group = as.factor(tr), 
                   fill = as.factor(tr)),
               alpha = 0.6) +
    ggtitle(name)
}

plot_altitude(data_random, "random") + 
  plot_altitude(data_blocked, "blocked") +
  plot_altitude(data_correlated, "correlated") 
```

![](figures/2023-12-11_test-treatment-assignment-function/unnamed-chunk-4-1.png)<!-- -->

``` r
plot_region <- function(df, name) {
  df %>% 
    select(description, region, tr) %>% 
    mutate(tr = as.factor(tr),
           region = as.factor(region)) %>% 
  distinct() %>% 
  ggplot(aes(x = region, fill = tr)) +
    geom_bar(stat = "count") +
    ggtitle(name)
}

plot_region(data_random, "random") + 
  plot_region(data_blocked, "blocked") +
  plot_region(data_correlated, "correlated") 
```

![](figures/2023-12-11_test-treatment-assignment-function/unnamed-chunk-5-1.png)<!-- -->
