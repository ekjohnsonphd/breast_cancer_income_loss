# Clear environment and free up memory
rm(list = ls())
gc()

# Load environment setup and required packages
source("code/environment_setup.R")

# Load and filter matching input dataset
df <- open_dataset("data/analysis/01_matching/00_matching_inputs/") %>%
  select(-familie_id) %>%
  filter(alder >= 18) %>%
  collect() %>%
  setDT()

# Rename key income and socioeconomic variables
setnames(
  df, c("famaekvivadisp_13", "famsociogrup_13", "perindkialt_13"),
  c("fam_ind", "fam_socio", "per_ind")
)

# Convert selected variables to factor type
fac_cols <- c(
  "familie_type", "ie_type", "pre_socio", "fam_socio", "edu",
  "age_group", "sex", "fam_size", "pnr", "reg"
)
df[, (fac_cols) := lapply(.SD, as.factor), .SDcols = fac_cols]

# Load CPI and EUR conversion rates to convert incomes to real 2023 EUR
load("data/supplemental_inputs/cpi_dk.RData")
load("data/supplemental_inputs/eur_dkk.RData")

eur_rate <- eur_dkk[year == 2023]$eur_rate

# Adjust income to 2023 EUR
df <- merge(df, cpi_dk, by = "year", allow.cartesian = TRUE)
df[, per_ind := inf_rate * per_ind / eur_rate]
df[, fam_ind := inf_rate * fam_ind / eur_rate]

# Compute income quantiles by year and assign individuals to quartiles
fam_qts <- df[, as.list(quantile(.SD, c(0.25, 0.5, 0.75), na.rm = TRUE)),
  .SDcols = c("fam_ind"), by = c("year")
]
per_qts <- df[, as.list(quantile(.SD, c(0.25, 0.5, 0.75), na.rm = TRUE)),
  .SDcols = c("per_ind"), by = c("year")
]
colnames(fam_qts) <- c("year", "fq1", "fq2", "fq3")
colnames(per_qts) <- c("year", "pq1", "pq2", "pq3")
df <- merge(df, fam_qts, by = "year")
df <- merge(df, per_qts, by = "year")

# Categorize individuals into income quartiles
df[, fam_inc_qt := "q1"][, per_inc_qt := "q1"]
df[fam_ind > fq1 & fam_ind <= fq2, fam_inc_qt := "q2"]
df[fam_ind > fq2 & fam_ind <= fq3, fam_inc_qt := "q3"]
df[fam_ind > fq3, fam_inc_qt := "q4"]
df[per_ind > pq1 & per_ind <= pq2, per_inc_qt := "q2"]
df[per_ind > pq2 & per_ind <= pq3, per_inc_qt := "q3"]
df[per_ind > pq3, per_inc_qt := "q4"]
df[, `:=`(
  fq1 = NULL, fq2 = NULL, fq3 = NULL, pq1 = NULL,
  pq2 = NULL, pq3 = NULL
)]
rm(fam_qts, per_qts)

# Remove income outliers outside the 1.5th and 98.5th percentiles
df[, p_inc_low_o := quantile(per_ind, 0.015, na.rm = TRUE), by = "year"]
df[, p_inc_high_o := quantile(per_ind, 0.985, na.rm = TRUE), by = "year"]
df[, f_inc_low_o := quantile(fam_ind, 0.015, na.rm = TRUE), by = "year"]
df[, f_inc_high_o := quantile(fam_ind, 0.985, na.rm = TRUE), by = "year"]
df[per_ind < p_inc_low_o | per_ind > p_inc_high_o, per_ind := NA]
df[fam_ind < f_inc_low_o | fam_ind > f_inc_high_o, fam_ind := NA]
df[, p_inc_low_o := NULL][, p_inc_high_o := NULL]
df[, f_inc_low_o := NULL][, f_inc_high_o := NULL]

# Create binary retired indicator from socioeconomic status
df[, retired := 0]
df[str_detect(pre_socio, "retired"), retired := 1]

# Convert PNR to numeric ID and drop original PNR
df[, pid := as.numeric(pnr)][, pnr := NULL]

# Deduplicate and drop rows with missing income
df <- unique(df)
df <- df[!is.na(fam_ind)]
df <- df[!is.na(per_ind)]

# Rename disease and comorbidity variables to shorter names
setnames(
  df, c(
    "charlson.index", "aids.hiv", "any.malignancy", "cerebrovascular.disease",
    "chronic.pulmonary.disease", "dementia", "diabetes.with.complications",
    "diabetes.without.complications", "heart.failure", "hemiplegia.paraplegia",
    "leukemia", "lymphoma", "metastatic.solid.tumor", "mild.liver.disease",
    "myocardial.infarction", "peptic.ulcer.disease",
    "peripheral.vascular.disease","renal.disease","rheumatic.disease",
    "severe.liver.disease"
  ),
  c(
    "cci", "dx_hiv", "dx_malig", "dx_cvasc", "dx_copd", "dx_demen", "dx_diabc",
    "dx_diabuc", "dx_hfail", "dx_hemi","dx_leuk", "dx_lymp", "dx_tumor",
    "dx_mliver", "dx_mi", "dx_pud", "dx_pvd", "dx_renal", "dx_rheum",
    "dx_sliver"
  )
)

# Replace NA values in diagnosis/comorbidity columns with 0
dx_cols <- c(
  str_subset(colnames(df), "^dx_"),
  "dbcg", "ddd", "dap", "nab", "cci"
)
df[, (dx_cols) := lapply(.SD, fcoalesce, 0), .SDcols = dx_cols]

# Write pre-matching dataset partitioned by year
df %>%
  group_by(year) %>%
  write_dataset("data/analysis/01_matching/01_pre_match")
