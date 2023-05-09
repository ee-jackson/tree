# Notes on SIMO

A book about SIMO can be found [here](https://www.simo-project.org/documentation/SIMObook.pdf) and the manual is [here](https://www.simo-project.org/documentation/SIMO_manual.pdf).

## Input variables used by Triviño *et al* 2022:

### NFI data
- Land class (1-3)
  - divides forestry land into three sub-categories, forest
land (pixel value 1), poorly productive forest land (2), unproductive land (3)
- Site main class (1-4)
  - divides the Land class further into mineral soil (pixel value 1) and peatland site
classes. The peatland site class is sub-divided into three categories spruce
mires (2), pine mires (3) and treeless peatland (4, open bogs and fens)
- Stand age (year)
- Stand mean diameter (cm)
- Stand mean height (dm)
- Stand basal area (m3/ha)
- Volume, pine (m3/ha)
- Volume, spruce (m3/ha)
- Volume, birch (m3/ha)
- Volume, other broad-leaved trees (m3/ha)

### Climate data
Temperature and precipitation change over time projected for three RCP scenarios (RCP 2.6, RCP 4.5 and RCP 8.5), across five GCM (Global Climate Models). Only the CanESM2 GCM results are presented in the paper. 
The climate data was obtained as model output from [CMIP5 (Coupled Model Intercomparison Project Phase 5)](https://pcmdi.llnl.gov/mips/cmip5/) (see [Taylor *et al* 2012](https://doi.org/10.1175/BAMS-D-11-00094.1)). 
