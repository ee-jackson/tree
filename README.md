# tree

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13269917.svg)](https://doi.org/10.5281/zenodo.13269917)

*TREE - TRansferablE Ecology for a changing world*

This repository contains the [research compendium](https://research-compendium.science) for our preprint: 

E. E. Jackson, T. Snäll, E. Gardner, J. M. Bullock & R. Spake. (2025) __Towards causal predictions of site-specific management effects in applied ecology.__ *EcoEvoRxiv* DOI: [10.32942/X2KK95](https://doi.org/10.32942/X2KK95)

Contact: eleanor.elizabeth.j@gmail.com

## Abstract

Targeted environmental management requires knowing where interventions will be most effective. Individual treatment effects (ITEs), which predict how each site would respond to alternative interventions, could help direct limited conservation resources to where management is expected to deliver the greatest benefit. Causal machine learning methods such as meta-learners can predict ITEs, yet most evidence on their performance comes from fields with much larger datasets than are typical in ecology, and with different evaluation criteria. We provide the first decision-relevant test of meta-learners for ecological ITE prediction, using forest-management simulations in which true site-level ITEs are known. We evaluated four meta-learners (S-, T-, X- and DR-learners) across 21,600 virtual observational studies varying in sample size, treatment imbalance, treatment assignment, spatial overlap between training and test data, and covariate omission. We assessed performance using separate metrics for two tasks: regional prioritisation, where accurate site ranking is critical, and local decision-making, where accurate effect-size estimation is required. The X-learner performed best on average, but relative performance varied with study conditions and evaluation metric. Our results show how ecological use of meta-learners can be guided by decision context, sample size and causal assumptions.

## Contents:

### [`code/`](code/)
The [`code/`](code/) directory contains these subdirectories:

[`scripts/`](code/scripts/) contains action scripts, i.e. all the code for cleaning, combining, and analysing the data. 
All paths in the scripts are relative to the root directory (where the `.Rproj` file lives). 
Each `.R` script has a summary at the top of what it does. 
The scripts are numbered in the order in which they would typically be run.

[`functions/`](code/functions/) contains `R` functions which are called by scripts in the `code/scripts/` directory. 
Note that functions were designed to be used only within this project.

[`notebooks/`](code/notebooks/) contains `.Rmd` files that were used for exploratory analysis and note-taking. 
Notebooks are not intended to be reproducible but the `.md` files can be viewed as rendered html (with output) on GitHub.

### `data/`
The original data is stored in the `data/raw/` subdirectory. 
Any data that is produced using code is stored in `data/derived/`. 

### [`output/`](output/)
The [`output/`](output/) directory contains the subdirectory [`figures/`](output/figures/), 
which contains the figures used in the paper.

## Usage
To reproduce results and figures from this project in the [RStudio IDE](https://posit.co/download/rstudio-desktop/), 
first open the `.Rproj` file and call `renv::restore()` to restore the project's R package library. 
Then, run the `.R` scripts in [`code/scripts/`](code/scripts/) in the order in which they are labelled, 
starting from `02_identify-test-plots.R`. 
Note that the first two scripts which clean and filter the data are for reference only, 
since we will be providing the cleaned data in this repository.

Two of the scripts (`03_get-ite-predictions.R` and `10_plot-all-true-vs-predictions.R`)
require a lot of time (~12hrs) and memory (~50GB) to run.
It is recommended to run them on a High-Performance Computing cluster, 
or else run fewer simulations ("virtual studies").

[NetCDF](https://www.unidata.ucar.edu/software/netcdf) 
(required by the R package [{ncdf4}](https://doi.org/10.32614/CRAN.package.ncdf4))
is needed to read the [CRU TS climate data](https://doi.org/10.1038/s41597-020-0453-3).

## License
Code is under a [MIT license](LICENSE.md)
