# Run ATT calculation on matched breast cancer cohort over follow-up period

# Clear environment and free memory
rm(list = ls())
gc()

# Load environment setup and matching functions
source("code/environment_setup.R")
source("code/02_analysis/match_functions.R")

# Define treatment and outcome variables
# treatment to run matching on - dbcg, ddd, dap, nab
treatment_var <- "dbcg"

# outcome to run matching on
outcome_var <- "fam_ind"

# Construct input and output directories for model and ATT results
model_dir <- paste0(
  "data/analysis/01_matching/02_matched/", treatment_var, "/",
  outcome_var, "/", Sys.Date(), "/"
)
cate_dir <- paste0(
  "data/analysis/01_matching/03_cate/", treatment_var, "/",
  outcome_var, "/", Sys.Date(), "/"
)

# Create directory for output if it doesn't exist
dir.create(cate_dir, recursive = TRUE, showWarnings = FALSE)

print(model_dir)

# Load model parameters
load(paste0(model_dir, "params.RData"))

# Set number of follow-up years and death imputation logic
follow_up_yrs <- 10
if (outcome_var == "per_ind") {
  deaths <- TRUE
} else {
  deaths <- FALSE
}

# Set up parallel processing for ATT estimation
cl <- makeCluster(10)

# Export necessary variables to cluster
clusterExport(cl, c("model_matrix", "model_dir", "cate_dir"))
clusterExport(cl, c(
  "treatment_var", "treatment_year", "outcome_var",
  "deaths", "follow_up_yrs"
))

clusterEvalQ(cl, {
  require(MatchIt)
  source("code/environment_setup.R")
  source("code/02_analysis/match_functions.R")
})

# Run time series construction and ATT calculation for each model
model_comparisons <- parLapplyLB(cl, 1:nrow(model_matrix), function(i) {
  m <- model_matrix[i]

  mdf <- get_match_time_series(paste0(model_dir, m$index), treatment_var,
    treatment_year, outcome_var,
    follow_up_length = follow_up_yrs,
    lookback = m$lookbacks, deaths_as_0 = deaths, agg = TRUE, prematch = FALSE
  )
  fwrite(mdf, paste0(cate_dir, m$index, ".csv"))
  att <- calculate_att(mdf)
  gc()
  return(cbind(m, att))
}) %>% rbindlist()

# Clean up parallel cluster
stopCluster(cl)

# Write final comparison of model ATT results to CSV
fwrite(model_comparisons, paste0(
  model_dir, "/model_comparisons_",
  follow_up_yrs, "y.csv"
))
