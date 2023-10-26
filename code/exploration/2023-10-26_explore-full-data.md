Explore new data
================
eleanorjackson
26 October, 2023

Tord has been able to send us the other 4/5th of the data :tada:

Let’s take a look at it and compare it to the data we already had.

``` r
library("tidyverse")
library("here")
library("janitor")
```

Load new data

``` r
file_names <- 
  as.list(dir(path = here::here("data", "raw"),
                         pattern = "ForManSims*", full.names = TRUE))

# don't load the first big file we recieved
file_names[[1]] <- NULL

data_list <- 
  lapply(X = file_names, FUN = read.csv, sep = ";", skipNul = TRUE)


names(data_list) <- 
  lapply(file_names, basename)

new_data <- 
  bind_rows(data_list, .id='df') %>%
  clean_names() 

rm(data_list)
glimpse(new_data)  
```

    ## Rows: 4,636,306
    ## Columns: 83
    ## $ df                                <chr> "ForManSims_2017_RCP0.csv", "ForManS…
    ## $ description                       <chr> "2016 2018 2100", "2016 2018 2100", …
    ## $ period                            <int> 19, 20, 1, 4, 5, 7, 9, 11, 11, 13, 1…
    ## $ alternative_no                    <int> 97, 82, 55, 51, 55, 55, 65, 40, 65, …
    ## $ represented_area                  <dbl> 2716.980, 2716.980, 2716.980, 2716.9…
    ## $ control_category_name             <chr> "SetAside (Unmanaged)", "BAU - NoThi…
    ## $ forest_domain_name                <chr> "Pine dominated_North (Contorta poss…
    ## $ altitude                          <int> 89, 89, 82, 82, 82, 82, 82, 82, 82, …
    ## $ county                            <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ ditch                             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ peat                              <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ region                            <int> 21, 21, 21, 21, 21, 21, 21, 21, 21, …
    ## $ soil_moist_code                   <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
    ## $ basal_area                        <dbl> 35.6604200, 32.0626500, 14.7639900, …
    ## $ closure                           <dbl> 0.9, 1.1, 0.4, 0.5, 0.5, 0.8, 0.8, 0…
    ## $ hgv                               <dbl> 23.063350, 14.977600, 20.937490, 21.…
    ## $ age                               <dbl> 233.499400, 63.775110, 160.266500, 1…
    ## $ no_of_stems                       <dbl> 727.1240, 1699.1170, 211.5101, 298.8…
    ## $ volume_excl_overstory             <dbl> 375.834100, 237.241300, 142.453100, …
    ## $ standing_volume                   <dbl> 375.83410, 255.41310, 142.45310, 168…
    ## $ volume_pine                       <dbl> 244.080800, 221.449100, 142.365500, …
    ## $ volume_spruce                     <dbl> 123.87450000, 5.73112900, 0.02025305…
    ## $ volume_birch                      <dbl> 7.75399300, 9.72846500, 0.05106011, …
    ## $ volume_aspen                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ volume_oak                        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ volume_beech                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ volume_southern_broadleaf         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ volume_contorta                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ volume_other_broadleaf            <dbl> 0.124764000, 0.332591400, 0.01625204…
    ## $ volume_larch                      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ basal_area_conifer                <dbl> 34.774910, 32.246240, 14.743990, 17.…
    ## $ basal_area_deciduous              <dbl> 0.885504700, 1.560940000, 0.01999992…
    ## $ dead_standing_trees_above20cm     <dbl> 23.7513600, 2.6099640, 1.3617520, 1.…
    ## $ dead_standing_trees_coniferous    <dbl> 27.459420, 9.330870, 1.361752, 1.665…
    ## $ dead_standing_trees_deciduous     <dbl> 0.63455010, 0.26655300, 0.00000000, …
    ## $ downed_deadwood_above20cm         <dbl> 15.1853000, 1.5004300, 0.1643621, 0.…
    ## $ downed_deadwood_coniferous        <dbl> 17.5560200, 5.7974020, 0.1643621, 0.…
    ## $ downed_deadwood_deciduous         <dbl> 0.40569600, 0.17041910, 0.00000000, …
    ## $ dw_volume_decayclass0             <dbl> 7.446847000, 5.797287000, 1.52611400…
    ## $ dw_volume_decayclass1             <dbl> 9.46019400, 5.77052300, 0.00000000, …
    ## $ dw_volume_decayclass2             <dbl> 7.7738550, 1.4407500, 0.0000000, 0.0…
    ## $ dw_volume_decayclass3             <dbl> 10.1283900, 0.4681916, 0.0000000, 0.…
    ## $ dw_volume_decayclass4             <dbl> 11.2464000, 2.0884930, 0.0000000, 0.…
    ## $ dw_volume_birch_decayclass0       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ dw_volume_birch_decayclass1       <dbl> 0.32491930, 0.17898180, 0.00000000, …
    ## $ dw_volume_birch_decayclass2       <dbl> 0.276880600, 0.119896400, 0.00000000…
    ## $ dw_volume_birch_decayclass3       <dbl> 0.2352903000, 0.0662015600, 0.000000…
    ## $ dw_volume_birch_decayclass4       <dbl> 0.1993582000, 0.0317879200, 0.000000…
    ## $ dw_volume_pine_decayclass0        <dbl> 3.22737600, 5.69952900, 1.52611400, …
    ## $ dw_volume_pine_decayclass1        <dbl> 5.45285600, 5.57926100, 0.00000000, …
    ## $ dw_volume_pine_decayclass2        <dbl> 4.3095050, 1.2380680, 0.0000000, 0.0…
    ## $ dw_volume_pine_decayclass3        <dbl> 4.7598750, 0.3854189, 0.0000000, 0.0…
    ## $ dw_volume_pine_decayclass4        <dbl> 7.2578060, 2.0500650, 0.0000000, 0.0…
    ## $ dw_volume_spruce_decayclass0      <dbl> 4.219471000, 0.097757220, 0.00000000…
    ## $ dw_volume_spruce_decayclass1      <dbl> 3.682419, 0.000000, 0.000000, 0.0000…
    ## $ dw_volume_spruce_decayclass2      <dbl> 3.187468000, 0.070971540, 0.00000000…
    ## $ dw_volume_spruce_decayclass3      <dbl> 5.131060000, 0.005623736, 0.00000000…
    ## $ dw_volume_spruce_decayclass4      <dbl> 3.787603000, 0.001578156, 0.00000000…
    ## $ total_soil_carbon                 <dbl> 55.91004, 48.64637, 52.19629, 45.596…
    ## $ total_carbon_stumpsand_roots      <dbl> 28.588280, 20.133330, 11.550890, 13.…
    ## $ total_carbon_trees_above_ground   <dbl> 87.252720, 62.892620, 33.320320, 38.…
    ## $ total_carbon_stock_incl_dead_wood <dbl> 177.40050, 134.28160, 97.34242, 98.1…
    ## $ is_montane                        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ root_rot_potential_volume         <chr> "74,41164", "0,3268815", "0,00035459…
    ## $ spruce_bark_beetle_index          <dbl> 2.13, 0.00, 0.00, 0.00, 0.00, 0.00, …
    ## $ stormindex_lagergren              <dbl> 0.0444797100, 0.0275174400, 0.031087…
    ## $ stormindex_valinger               <dbl> 0.104772500, 0.018433500, 0.05990576…
    ## $ lagre_sprucetrees                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ large_pinetrees                   <dbl> 54.056690, 7.715984, 0.000000, 0.000…
    ## $ large_southern_broadleaves        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ large_other_broadleaves           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ sum_volume_cut_total              <dbl> 0.00000, 0.00000, 0.00000, 156.09880…
    ## $ sum_timber_volume_total           <dbl> 0.00000, 0.00000, 0.00000, 91.45190,…
    ## $ sum_pulp_volume_total             <dbl> 0.00000, 0.00000, 0.00000, 27.38070,…
    ## $ sum_harvest_residues_total        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ sum_harvest_fuelwood_total        <dbl> 0.00000, 0.00000, 0.00000, 14.94586,…
    ## $ sum_harvest_m3fub_total           <dbl> 0.0000, 0.0000, 0.0000, 137.7340, 14…
    ## $ sum_harvest_stumps_total          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    ## $ dead_wood_volume                  <dbl> 46.055686, 15.565245, 1.526114, 2.20…
    ## $ dead_wood_volume_pine             <dbl> 25.007418, 14.952342, 1.526114, 2.20…
    ## $ dead_wood_volume_spruce           <dbl> 20.008021000, 0.175930652, 0.0000000…
    ## $ dead_wood_volume_birch            <dbl> 1.03644840, 0.39686768, 0.00000000, …
    ## $ dead_wood_volume_other_broad_leaf <dbl> 3.797700e-03, 4.010442e-02, 0.000000…

Load original data

``` r
og_data <-
  readRDS(here::here("data", "derived", "subset_ForManSims_2016_RCP0.rds")) %>%
  mutate(df = "ForManSims_2016_RCP0.csv")
```

## Do the new data contain new plots?

The new data contains 25326 unique plots (`description` IDs), and the
original data has 4731.

If we join the data does the number of unique plots = 30057 (25326 +
4731)?

``` r
og_data_0 <- 
  og_data %>% 
  filter(period == 0)

new_data_0 <- 
  new_data %>% 
  filter(period == 0)

bind_rows(og_data_0, new_data_0) %>% 
  distinct(description) %>% 
  glimpse()
```

    ## Rows: 29,939
    ## Columns: 1
    ## $ description <chr> "2016 1008 1180", "2016 1008 2060", "2016 2019 1152", "201…

We get 29,939 distinct plots, so there’s an overlap of 118 plots. Where
are they duplicated?

``` r
inner_join(og_data_0, new_data_0, by = "description") %>% 
  select(df.x, df.y)
```

    ##                         df.x                     df.y
    ## 1   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 2   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 3   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 4   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 5   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 6   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 7   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 8   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 9   ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 10  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 11  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 12  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 13  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 14  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 15  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 16  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 17  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 18  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 19  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 20  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 21  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 22  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 23  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 24  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 25  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 26  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 27  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 28  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 29  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 30  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 31  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 32  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 33  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 34  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 35  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 36  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 37  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 38  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 39  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 40  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 41  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 42  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 43  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 44  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 45  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 46  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 47  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 48  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 49  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 50  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 51  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 52  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 53  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 54  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 55  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 56  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 57  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 58  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 59  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 60  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 61  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 62  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 63  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 64  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 65  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 66  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 67  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 68  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 69  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 70  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 71  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 72  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 73  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 74  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 75  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 76  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 77  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 78  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 79  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 80  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 81  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 82  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 83  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 84  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 85  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 86  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 87  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 88  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 89  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 90  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 91  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 92  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 93  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 94  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 95  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 96  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 97  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 98  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 99  ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 100 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 101 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 102 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 103 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 104 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 105 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 106 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 107 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 108 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 109 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 110 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 111 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 112 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 113 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 114 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 115 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 116 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 117 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv
    ## 118 ForManSims_2016_RCP0.csv ForManSims_2017_RCP0.csv

The overlaps are between `ForManSims_2017_RCP0.csv` and
`ForManSims_2016_RCP0.csv` (the original data).

Are all the variables duplicated or just the plot ID?

``` r
bind_rows(og_data_0, new_data_0) %>% 
  select(-df) %>% 
  get_dupes() %>% 
  nrow()
```

    ## No variable names specified - using all columns.

    ## [1] 236

Looks like the entire rows are duplicated (236/2=118). That’s ok, these
are easy to remove.

``` r
bind_rows(og_data_0, new_data_0) %>% 
  distinct(across(-df), .keep_all = TRUE) -> all_data_0
```

## What management options have been simulated for plots in the new data?

``` r
summary(as.factor(new_data$control_category_name))
```

    ##     BAU - NoThinning        Initial state SetAside (Unmanaged) 
    ##              4130560                25326               480420

Just the ones we want :thumbsup:

## What is our sample size?

Keep getting the `Error: vector memory exhausted (limit reached?)`
message, might have to test this on the cluster.
