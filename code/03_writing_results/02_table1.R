# Create Table 1: Descriptive statistics for control and exposed
# Producing for 2015 so as to not represent controls as duplicates
# over time.

# Load environment and matching functions
source("code/environment_setup.R")
source("code/02_analysis/match_functions.R")
# Load required packages
library(yardstick)
library(GGally)
library(vtable)
# Load pre-matching dataset
df <- open_dataset("data/analysis/01_matching/01_pre_match") %>%
  collect() %>%
  setDT()
# Convert education and socioeconomic status to ordered factors
df$edu <- factor(df$edu, levels = c(
  "Basic", "Upper secondary",
  "Vocational training", "Bachelor", "Higher education"
), ordered = TRUE)
# Convert education and socioeconomic status to ordered factors
df$pre_socio <- factor(df$pre_socio, levels = c(
  "wage_earner", "retired", "in_school", "disab_recipient", "unemp_recipient",
  "early_retirement", "self_employed", "leave_recipient", "unemployed_6mo",
  "working_spouse", "other", "unknown"
), ordered = TRUE)
# Variables to include in descriptive table
desc_cols <- c(
  "alder", "per_ind", "fam_ind", "familie_type",
  "fam_size", "pre_socio", "edu"
)
# Filter to 2015 females and define comparison groups
sumdf <- copy(df[year == 2015 & sex == "female"])
sumdf[, grp := "Overall"]
sumdf <- rbind(
  sumdf,
  copy(sumdf[dbcg == 1 & dbcg_dx_yr == 2016])[, grp := "dbcg"]
)
# Generate Table 1 and save to CSV
st(sumdf,
  vars = desc_cols, summ = c("mean(x)", "median(x)", "sd(x)"),
  group = "grp", out = "csv",
  file = "data/table1.csv"
)
