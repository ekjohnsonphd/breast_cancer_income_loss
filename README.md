# Breast Cancer Income Loss Study

This repository contains the full code used in the analysis of income loss following breast cancer diagnosis, including data preparation, matching, and ATT/CATE estimation for heterogeneous effects. The workflow is modular, with dedicated folders for each stage of the pipeline.

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
  - `paper1_results.R`: Computes the point estimates and tables referenced in the text.
  - `table1_descriptives.R`: Generates baseline descriptive statistics for the population and DBCG group.
  - `lifepatterns_after_matching.R`: Plots longitudinal trajectories of covariates post-matching.
  - `summarize_cate_outputs.R`: Aggregates and formats heterogeneous treatment effects for subgroups.

---

## Data

This repository does not contain input data due to privacy restrictions. All analysis is based on Danish register data accessed through secure servers. 
Output data from 01_write_cate_tables.R is included in the `data`folder.

---

## Contact and further info
For questions or collaboration, please contact Emily K. Johnson at [ejohnson@health.sdu.dk].
All code in this repository was written by me. AI chatbots were used to help with linting, code review, and adding comments to the code.