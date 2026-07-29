Check models are trained on vars at time = 0
================
eleanorjackson
06 July, 2026

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
```

Can we be 100% certain that no. stems is a pre-treatment variable at
time = 0?

``` r
all_runs <- readRDS(here::here("data", "derived", "all_runs.rds"))
```

``` r
clean_data <-
  readRDS(here::here("data", "derived", "ForManSims_RCP0_same_time_clim_squ.rds"))
```

``` r
all_runs$df_train[[1]] %>% glimpse
```

    ## Rows: 62
    ## Columns: 24
    ## $ description               <chr> "201926181060", "201720293150", "20204664408…
    ## $ tr                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ soil_carbon_initial       <dbl> 65.18635, 75.50119, 91.03160, 65.05943, 65.5…
    ## $ soil_carbon_0             <dbl> 81.53082, 78.53805, 67.22488, 71.94175, 68.1…
    ## $ soil_carbon_1             <dbl> 65.51192, 70.50894, 81.43774, 61.47451, 58.4…
    ## $ soil_carbon_obs           <dbl> 81.53082, 78.53805, 67.22488, 71.94175, 68.1…
    ## $ sampling_location         <fct> other, other, other, other, other, other, ot…
    ## $ soil_moist_code           <int> 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ altitude                  <int> 180, 302, 48, 278, 337, 9, 62, 97, 145, 489,…
    ## $ mat_5yr                   <dbl> 3.351667, 1.748333, 7.456667, 4.020000, 2.30…
    ## $ map_5yr                   <dbl> 53.99500, 50.80333, 53.60500, 51.36667, 48.5…
    ## $ ditch                     <int> 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    ## $ no_of_stems               <dbl> 2323.6570, 4417.3050, 238.1898, 1883.8510, 5…
    ## $ volume_pine               <dbl> 139.229700, 129.156100, 105.439500, 119.6294…
    ## $ volume_spruce             <dbl> 22.193030, 0.000000, 0.000000, 30.131330, 5.…
    ## $ volume_birch              <dbl> 10.429710, 7.365758, 24.060390, 0.000000, 21…
    ## $ volume_aspen              <dbl> 0.00000, 0.00000, 20.00577, 0.00000, 0.00000…
    ## $ volume_oak                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ volume_beech              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ volume_southern_broadleaf <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ volume_contorta           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ volume_other_broadleaf    <dbl> 2.817883, 0.000000, 0.000000, 0.000000, 0.00…
    ## $ volume_larch              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ wet                       <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

So for plot “201926181060” no stems is 2323.6570 in the training data

``` r
clean_data %>% 
  filter(description == "201926181060") %>% glimpse
```

    ## Rows: 3
    ## Columns: 27
    ## $ description               <chr> "201926181060", "201926181060", "20192618106…
    ## $ ost_wgs84                 <dbl> 19.23442, 19.23442, 19.23442
    ## $ nord_wgs84                <dbl> 63.8887, 63.8887, 63.8887
    ## $ taxar                     <dbl> 2019, 2019, 2019
    ## $ region                    <int> 21, 21, 21
    ## $ altitude                  <int> 180, 180, 180
    ## $ mat_5yr                   <dbl> 3.351667, 3.351667, 3.351667
    ## $ map_5yr                   <dbl> 53.995, 53.995, 53.995
    ## $ period                    <int> 0, 20, 20
    ## $ control_category_name     <chr> "Initial state", "SetAside (Unmanaged)", "BA…
    ## $ total_soil_carbon         <dbl> 65.18635, 81.53082, 65.51192
    ## $ soil_moist_code           <int> 2, 2, 2
    ## $ ditch                     <int> 0, 0, 0
    ## $ wet                       <dbl> 0, 0, 0
    ## $ no_of_stems               <dbl> 2323.657, 1170.256, 2020.934
    ## $ standing_volume           <dbl> 174.6703, 685.3577, 226.1774
    ## $ volume_pine               <dbl> 139.2297, 440.4208, 191.4015
    ## $ volume_spruce             <dbl> 22.193030, 203.425300, 7.607306
    ## $ volume_birch              <dbl> 10.42971, 34.91724, 14.11867
    ## $ volume_aspen              <dbl> 0, 0, 0
    ## $ volume_oak                <dbl> 0, 0, 0
    ## $ volume_beech              <dbl> 0, 0, 0
    ## $ volume_southern_broadleaf <dbl> 0, 0, 0
    ## $ volume_contorta           <dbl> 0, 0, 0
    ## $ volume_other_broadleaf    <dbl> 2.817883, 6.594333, 0.344466
    ## $ volume_larch              <dbl> 0, 0, 0
    ## $ sampling_location         <fct> other, other, other
