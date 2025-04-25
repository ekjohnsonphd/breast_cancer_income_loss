# Breast Cancer Income Loss Study

This repository contains the full code used in the analysis of income loss following breast cancer diagnosis, including data preparation, matching, and ATT/CATE estimation for heterogeneous effects. The workflow is modular, with dedicated folders for each stage of the pipeline.

---

## Setup and installation instructions
This code is developed to be run on Danish register data. Register data access is governed by the Danish Health Authority and overseen by Statistics Denmark, and data usage agreements are required to access the raw data. If a user has access to all registers used in this project, they should be able to run all code in this project without additional processing.

The following registers are required for this analysis:
- The Danish Civil Registration Register
- The National Patient Register
- The Integrated Student Register
- The National Income Register
- The Danish Breast Cancer Register (provided by the Danish Breast Cancer Group)

All registers except the Breast Cancer Register are provided for research use by Statistics Denmark.

---

## How to run
With access to the register data provided by Statistics Denmark, this code can be run with the following steps:
1. Save all raw register data in parquet format in a directory with the name `data/rawdata`. 
2. Run the `environment_setup.R` file, which will create the necessary directories and load all required packages.
3. Run R scripts in order by number in the folder and file names. 
4. Data will be saved in versioned folders for matching and CATE estimates, and summary plots and tables are created in the results files.

Depending on the years of data available and the server capacity, this code can take hours up to a half of a day to run to completion.

---

## Repository Structure

### `code/01_data_prep/`
- **Purpose**: Merge, format and prepare raw register data.
- **Details**: Includes cleaning, recoding, and harmonizing variables. Variables like comorbidity (CCI), diagnosis codes, and basic demographics are processed here. 2 files do the bulk 
of the data preparation work in this folder:
  - `03_data_prep.R`: Load and merge raw register data into a wide dataframe, with 1 row per person-year of data.
  - `04_match_data_prep.R`: Reformat variable names and values for analysis, including values like income quantile assignment, age group definitions, and naming education and 
  socioeconomic variables in English.
- **Excludes**: Mapping scripts and conversion tools not used in final analysis.

### `code/02_analysis/`
- **Purpose**: Conduct matching and ATT estimation. ATT calculation was separated from matching in order to maintain computational efficiency and allow for iteration on 
CATE estimation (e.g. censoring deaths vs. imputing years after death as years of zero income) without needing to re-match.
- **Details**: Estimation code was designed to be iterative, so matching specification and results as well as CATE estimation could be iterated upon. Each file is described below:
  - `01_match_jobs.R`: Parallelizes over the matching specification that will be run, and creates match groups. Can vary the lookback period (i.e. how many years before diagnosis
  should matching be conducted).
  - `02_match_functions.R`: Functions to conduct matching and ATT estimation, including match group estimation, imputing postmortem years as 0, and estimating ATT.
  - `03_match_att_calculation.R`: Estimates ATT for a given match dataset. Number of years of follow-up and outcome variable must be specified.
  - `04_did.R`: Difference-in-difference estimation script. This script runs an event study difference-in-difference analysis on matched data as a secondary analysis.

### `code/03_writing_results/`
- **Purpose**: Scripts for specific outputs and tables used in the manuscript.
- **Details**:
  - `01_write_cate_tables.R`: Produces all estimates for the main and secondary analyses used in the paper, which are saved in the file `data/heterogeneous_effects.csv`. 
  - `02_table1.R`: Produces descriptive statistics about the exposed and control populations for the analysis, using a sample year of data.
  - `03_number_plugging.R`: Produces all statistical estimates presented in the Results section of the paper, including match group characteristics, income losses, and 
  heterogeneous effects.
  - `04_paper_plots_and_tables.R`: Produces all tables and figures used in the main text and supplementary materials for this publication.

---

## Data

This repository does not contain input data due to privacy restrictions. All analysis is based on Danish register data accessed through secure servers. More information on accessing 
Danish register data for research (given affiliation with a Danish research institution) can be found at https://www.dst.dk/da/TilSalg/data-til-forskning. 
Output data from `03_writing_results/01_write_cate_tables.R`, `03_writing_results/02_table1.R`, and `02_analysis/did.R` is included in the `data` folder, and could be used with the script 
`04_paper_plots_and_tables.R` to recreate all figures and tables.

---

## System requirements
R version 4.4.3
Required packages: arrow_16.1.0, lubridate_1.9.3, forcats_1.0.0, stringr_1.5.1, dplyr_1.1.4, purrr_1.0.2, readr_2.1.5, tidyr_1.3.1, tibble_3.2.1, ggplot2_3.5.1,
tidyverse_2.0.0, data.table_1.15.4, MatchIt_4.7.1, cowplot_1.1.3

---

## Contact and further info
For questions or collaboration, please contact Emily K. Johnson at [ejohnson@health.sdu.dk].
All code in this repository was written by me. AI chatbots were used to help with linting, code review, and improving comments to the code but they did not modify code directly.