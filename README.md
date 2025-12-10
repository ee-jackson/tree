# tree

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13269918.svg)](https://doi.org/10.5281/zenodo.13269918)

*TREE - TRansferablE Ecology for a changing world*

This repository contains the [research compendium](https://research-compendium.science) for our preprint: 

E. E. Jackson, T. Snäll, E. Gardner, J. M. Bullock & R. Spake. (2025) __Towards causal predictions of site-specific management effects in applied ecology.__ *EcoEvoRxiv* DOI: [10.32942/X2KK95](https://doi.org/10.32942/X2KK95)

Contact: eleanor.elizabeth.j@gmail.com

## Abstract

With limited resources and the urgent need to reverse biodiversity loss, 
conservation efforts must be targeted to where they will be most effective. 
Targeting actions necessitates new approaches to causal prediction of 
sited-level responses to alternative interventions. 
We present the first application of ‘meta-learner algorithms’ to predict 
‘individual treatment effects’ (ITEs) representing the effects of site-level 
management actions. We compare the performance of three algorithms that differ 
in how they handle selection biases typical to observational data: 
S-, T-, and X-Learners, across 4,050 virtual studies predicting the effect of 
forest management on soil carbon, the ITEs. The X-Learner, an algorithm which 
adjusts for selection bias, consistently yielded the most accurate ITE 
predictions across studies varying in sample size and imbalanced sample sizes 
of treatment and control groups. Our study illustrates how ecologists can begin 
to select and apply causal prediction methods to inform targeted conservation 
action for ecological systems, and makes suggestions for further road-testing 
of these approaches.

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
