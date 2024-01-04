# data-dictionary

## ForManSims_2016_RCP0.csv
Note that file uses a semi-colon delimiter!

|variable                           |definition                        |
|:----------------------------------|:---------------------------------|
|`description`                      |year-trakt-plot |
|`period`                           |Five year time period from `0` to `20`, `0` is observed data and time periods > `0` are predictions from Heureka. |
|`alternative_no`                   ||
|`represented_area`                 ||
|`control_category_name`            |Management?|
|`forest_domain_name`               ||
|`altitude`                         |Height above sea level (m), input variable|
|`county`                           |County code according to [Swedish NFI ("DLänskod")](https://www.heurekaslu.se/wiki/Definition:CountyCode)|
|`ditch`                            ||
|`peat`                             ||
|`region`                           |Region code according to [Swedish NFI](https://www.heurekaslu.se/wiki/Definition:Region)|
|`soil_moist_code`                  ||
|`basal_area`                       ||
|`closure`                          ||
|`hgv`                              |[Basal area weighted mean height (cm)](https://www.heurekaslu.se/wiki/Dictionary#Hgv)|
|`age`                              |Mean tree age?|
|`no_of_stems`                      ||
|`volume_excl_overstory`            ||
|`standing_volume`                  ||
|`volume_pine`                      ||
|`volume_spruce`                    ||
|`volume_birch`                     ||
|`volume_aspen`                     ||
|`volume_oak`                       ||
|`volume_beech`                     ||
|`volume_southern_broadleaf`        ||
|`volume_contorta`                  ||
|`volume_other_broadleaf`           ||
|`volume_larch`                     ||
|`basal_area_conifer`               ||
|`basal_area_deciduous`             ||
|`dead_standing_trees_above20cm`    ||
|`dead_standing_trees_coniferous`   ||
|`dead_standing_trees_deciduous`    ||
|`downed_deadwood_above20cm`        ||
|`downed_deadwood_coniferous`       ||
|`downed_deadwood_deciduous`        ||
|`dw_volume_decayclass0`            ||
|`dw_volume_decayclass1`            ||
|`dw_volume_decayclass2`            ||
|`dw_volume_decayclass3`            ||
|`dw_volume_decayclass4`            ||
|`dw_volume_birch_decayclass0`      ||
|`dw_volume_birch_decayclass1`      ||
|`dw_volume_birch_decayclass2`      ||
|`dw_volume_birch_decayclass3`      ||
|`dw_volume_birch_decayclass4`      ||
|`dw_volume_pine_decayclass0`       ||
|`dw_volume_pine_decayclass1`       ||
|`dw_volume_pine_decayclass2`       ||
|`dw_volume_pine_decayclass3`       ||
|`dw_volume_pine_decayclass4`       ||
|`dw_volume_spruce_decayclass0`     ||
|`dw_volume_spruce_decayclass1`     ||
|`dw_volume_spruce_decayclass2`     ||
|`dw_volume_spruce_decayclass3`     ||
|`dw_volume_spruce_decayclass4`     ||
|`total_soil_carbon`                ||
|`total_carbon_stumpsand_roots`     ||
|`total_carbon_trees_above_ground`  ||
|`total_carbon_stock_incl_dead_wood`||
|`is_montane`                       ||
|`root_rot_potential_volume`        ||
|`spruce_bark_beetle_index`         ||
|`stormindex_lagergren`             ||
|`stormindex_valinger`              ||
|`lagre_sprucetrees`                ||
|`large_pinetrees`                  ||
|`large_southern_broadleaves`       ||
|`large_other_broadleaves`          ||
|`sum_volume_cut_total`             ||
|`sum_timber_volume_total`          ||
|`sum_pulp_volume_total`            ||
|`sum_harvest_residues_total`       ||
|`sum_harvest_fuelwood_total`       ||
|`sum_harvest_m3fub_total`          ||
|`sum_harvest_stumps_total`         ||
|`dead_wood_volume`                 ||
|`dead_wood_volume_pine`            ||
|`dead_wood_volume_spruce`          ||
|`dead_wood_volume_birch`           ||
|`dead_wood_volume_other_broad_leaf`||

## NFI plot coords NFI 2016-2020.xlsx
As defined by Tord.

|variable                           |definition                        |
|:----------------------------------|:---------------------------------|
|`description`                      |Unique plot ID |
|`delyta_inve`                      |Unique plot ID |
|`taxar`                            |Field NFI survey year (simulation year 0, t_0); they survey 1/5 of the plots each year so our sims thus start from each of the five years 2016-2020|
|`trakt_nr`                        |Tract (see the [NFI instructions](https://www.slu.se/globalassets/ew/org/centrb/rt/dokument/faltinst/nfi_fieldwork_instructions_eng.pdf) for def.)|
|`palslag_nr`                      |Plot (radius 10 m)|
