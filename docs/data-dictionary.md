# data-dictionary

## ForManSims_RCP0_same_time_clim.rds

|variable                           |definition                        |
|:----------------------------------|:---------------------------------|
|`description`                      |Unique plot ID |
|`ost_wgs84`                        |Longitudinal coordinates of the plot in wgs84 |
|`nord_wgs84`                       |Latitudinal coordinates of the plot in wgs84 |
|`taxar`                            |Year at period `0`, one of: `2016`, `2017`, `2018`, `2019` or `2020` |
|`region`                           |Region code according to the [Swedish NFI](https://www.heurekaslu.se/wiki/Definition:Region)|
|`altitude`                         |Height above sea level (m), sourced from the Swedish National Forest Inventory.|
|`map_5yr`                          |Mean annual temperature (degrees Celsius), sourced from [CRU TS (Climatic Research Unit gridded Time Series) (v. 4.07).](https://doi.org/10.1038/s41597-020-0453-3) Plots were matched to the nearest climate station and mean annual temperature was averaged across a 5-year period prior to NFI sampling. |
|`map_5yr`                          |Mean annual precipitation (mm), sourced from [CRU TS (Climatic Research Unit gridded Time Series) (v. 4.07).](https://doi.org/10.1038/s41597-020-0453-3) Plots were matched to the nearest climate station and mean annual precipitation was averaged across a 5-year period prior to NFI sampling.|
|`period`                           |Time period, either `0` or `20`. Data at time `0` are observed data and data at time `20` are predictions from [Heureka.](https://www.heurekaslu.se/wiki/About_Heureka) |
|`control_category_name`            |Management regime. One of: `Set aside (Unmanaged)` The forest grows from the initial state, no biomass extraction. The state of the forest develops without any influence of any forestry. `BAU – NoThinning` Even aged forestry with no thinnings  simulated. Max 30 years delay in final felling after reaching minimum final felling age. Regeneration: planting. Retention of 10 trees/ha and leaving 3 high stumps/ha (according to default tree priority list). `Initial state` When `period` == 0, `control_category_name` is always `Initial state`|
|`total_soil_carbon`                |Total amount of soil organic carbon as measured in the Swedish National Forest Inventory at `period` == 0, and corresponding to input data for [Heureka's](https://www.heurekaslu.se/wiki/About_Heureka) soil carbon model. |
|`soil_moist_code`                  |An ordinal variable ranging from `1` (dry) to `5` (wet), sourced from the Swedish National Forest Inventory. `1` = Dry (subsoil water depth >2 m, sv: torr), `2` = Mesic (subsoil water depth = 1-2 m, sv: frisk), `3` = Mesic-moist (subsoil water depth <1 m, sv: frisk-fuktig), `4` = Moist (subsoil water depth <1 m, and pools visible in hollows, sv: fuktig), `5` = Wet (subsoil water pools visible, sv: blöt)|
|`ditch`                            |A binary variable indicating if the site has been ditched to aid water drainage, where `0` = no ditching and `1` = ditched. Sourced from the Swedish National Forest Inventory. |
|`no_of_stems`                      |Total number of stems within the plot (DBH >= 4 cm) sourced from the Swedish National Forest Inventory.|
|`standing_volume`                  |Absolute volume (m<sup>3</sup>/ha) of tree species within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_pine`                      |Absolute volume (m<sup>3</sup>/ha) of Pine within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_spruce`                    |Absolute volume (m<sup>3</sup>/ha) of Spruce within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_birch`                     |Absolute volume (m<sup>3</sup>/ha) of Birch within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_aspen`                     |Absolute volume (m<sup>3</sup>/ha) of Aspen within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_oak`                       |Absolute volume (m<sup>3</sup>/ha) of Oak within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_beech`                     |Absolute volume (m<sup>3</sup>/ha) of Beech within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_southern_broadleaf`        |Absolute volume (m<sup>3</sup>/ha) of Southern Broadleaf within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_contorta`                  |Absolute volume (m<sup>3</sup>/ha) of Contorta within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_other_broadleaf`           |Absolute volume (m<sup>3</sup>/ha) of other Broadleaf species within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_larch`                     |Absolute volume (m<sup>3</sup>/ha) of Larch within the plot, as recorded by the Swedish National Forest Inventory.|


## ForManSims_2016_RCP0.csv
Note that file uses a semi-colon delimiter!

|variable                           |definition                        |
|:----------------------------------|:---------------------------------|
|`description`                      |year-trakt-plot |
|`period`                           |Five year time period from `0` to `20`, `0` is observed data and time periods > `0` are predictions from Heureka. |
|`alternative_no`                   |Simulation replication.|
|`represented_area`                 ||
|`control_category_name`            |Management regime. One of: `Set aside (Unmanaged)` The forest grows from the initial state, no biomass extraction. The state of the forest develops without any influence of any forestry. `BAU – NoThinning` Even aged forestry with no thinnings  simulated. Max 30 years delay in final felling after reaching minimum final felling age. Regeneration: planting. Retention of 10 trees/ha and leaving 3 high stumps/ha (according to default tree priority list). `Initial state` When `period` == 0, `control_category_name` is always `Initial state`|
|`forest_domain_name`               ||
|`altitude`                         |Height above sea level (m), input variable|
|`county`                           |County code according to [Swedish NFI ("DLänskod")](https://www.heurekaslu.se/wiki/Definition:CountyCode)|
|`ditch`                            |A binary variable indicating if the site has been ditched to aid water drainage, where `0` = no ditching and `1` = ditched. Sourced from the Swedish National Forest Inventory.|
|`peat`                             ||
|`region`                           |Region code according to [Swedish NFI](https://www.heurekaslu.se/wiki/Definition:Region)|
|`soil_moist_code`                  ||
|`basal_area`                       ||
|`closure`                          ||
|`hgv`                              |[Basal area weighted mean height (cm)](https://www.heurekaslu.se/wiki/Dictionary#Hgv)|
|`age`                              |Mean tree age?|
|`no_of_stems`                      ||
|`volume_excl_overstory`            ||
|`standing_volume`                  |Absolute volume (m<sup>3</sup>/ha) of tree species within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_pine`                      |Absolute volume (m<sup>3</sup>/ha) of Pine within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_spruce`                    |Absolute volume (m<sup>3</sup>/ha) of Spruce within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_birch`                     |Absolute volume (m<sup>3</sup>/ha) of Birch within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_aspen`                     |Absolute volume (m<sup>3</sup>/ha) of Aspen within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_oak`                       |Absolute volume (m<sup>3</sup>/ha) of Oak within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_beech`                     |Absolute volume (m<sup>3</sup>/ha) of Beech within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_southern_broadleaf`        |Absolute volume (m<sup>3</sup>/ha) of Southern Broadleaf within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_contorta`                  |Absolute volume (m<sup>3</sup>/ha) of Contorta within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_other_broadleaf`           |Absolute volume (m<sup>3</sup>/ha) of other Broadleaf species within the plot, as recorded by the Swedish National Forest Inventory.|
|`volume_larch`                     |Absolute volume (m<sup>3</sup>/ha) of Larch within the plot, as recorded by the Swedish National Forest Inventory.|
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
