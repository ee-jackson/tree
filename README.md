# tree
A [TREE (TRansferablE Ecology for a changing world) lab](https://treelabecology.weebly.com) project.

This repository contains the [research compendium](https://research-compendium.science) for our in-prep manuscript: __Precision ecology in action.__

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
To reproduce results and figures from this project, 
first open the `.Rproj` file and call `renv::restore()` to restore the project's R package library. 
Then, run the `.R` scripts in [`code/scripts/`](code/scripts/) in the order in which they are labelled, 
starting from `02_identify-test-plots.R`. 
Note that the first two scripts are for reference only, 
since they need to be run on a high-performance computing cluster.
