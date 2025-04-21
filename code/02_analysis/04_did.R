# Difference-in-Differences (DiD) analysis for secondary analysis
# Conducts event study DiD analysis using data matched on all
# variables except baseline income quartile.

# Load environment and matching helper functions
source("code/environment_setup.R")
source("code/02_analysis/match_functions.R")

# Load required packages
library(fixest)
# Load required packages
library(broom)

# Define treatment and outcome variables
# treatment to run matching on - dbcg, ddd, dap, nab
treatment_var <- "dbcg"
treatment_year <- paste0(treatment_var, "_dx_yr")

# outcome to run matching on
outcome_var <- "per_ind"

# Set follow-up window
follow_up_yrs <- 10

# Define directory for matched data
model_dir <- paste0(
  "data/analysis/01_matching/02_matched/", treatment_var,
  "/", outcome_var, "/2024-05-25/"
)

print(model_dir)

# Load matched time series dataset for DiD analysis
mdf <- get_match_time_series(
  match_dir = paste0(model_dir, "m10"), treatment_var, treatment_year, 
  outcome_var, follow_up_length = 10,
  lookback = 1, deaths_as_0 = TRUE, agg = FALSE, prematch = TRUE
)

# Reassign treatment indicator for treated individuals
mdf[!is.na(treat_yr), treat := 1]
# Define synthetic blocks for clustering (subclass + year)
mdf[, sbc := .GRP, by = c("subclass", "match_yr")]
# Collapse data by group for DiD input
did_df <- mdf[, .(outcome = mean(outcome), n = .N),
  by = c(
    "match_yr", "sbc", "match_age", "sex", "treat", "per_inc_qt", "fam_inc_qt", "familie_type",
    "fam_size", "edu", "pre_socio", "retired", "year", "index"
  )
]

# Run DiD model with two-way fixed effects and interaction terms
did_es <- feols(outcome ~ i(index, treat, ref = -1) + treat * (1 + match_age + edu + fam_size + familie_type) | year,
  data = did_df, weights = did_df$n
)

# Extract model summary
smry <- summary(did_es)

# Tidy and round coefficient output
smry_coefs <- tidy(smry) %>% setDT()
smry_coefs[, estimate := round(estimate, 2)][, std.error := round(std.error, 2)]
smry_coefs[, statistic := round(statistic, 3)][, p.value := round(p.value, 3)]

# Tidy and round model performance statistics
smry_perf <- glance(smry) %>%
  setDT() %>%
  round(3)

# Save coefficients and model fit statistics to CSV
fwrite(smry_coefs, "data/paper1/did_coefficients.csv")
fwrite(smry_perf, "data/paper1/did_perf.csv")
