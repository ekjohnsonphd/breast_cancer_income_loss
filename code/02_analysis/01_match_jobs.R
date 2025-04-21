# Purpose: Serialized matching by the variables used to do exact matching
# Logs the models run and launches jobs, then writes matched data
source("code/environment_setup.R")  # Load the environment setup
library(MatchIt)

source("code/02_analysis/match_functions.R")  # Load matching functions

# Hardcode treatment: breast cancer (dbcg), but can be changed
treatment_year <- "dbcg_dx_yr"
treatment_match_var <- "dbcg_match_df"

# Outcome to run matching on
outcome_var <- "fam_ind"

# Define the versioned model directory based on treatment and outcome
model_dir <- paste0("data/analysis/01_matching/02_matched/dbcg/",
                    outcome_var, "/", Sys.Date(), "/")

# Define the years for matching based on treatment causes
year_set <- data.table(
  cause = "dbcg",
  st_yr = c(2000, 2013, 2008, 2011),
  ed_yr = 2014
)

# Create model grid ------------------------------------------------------
# Define lookback periods and diagnosis variables
lookbacks <- c(1, 3, 5)
dx_vars <- c(
  "dx_cvasc", "dx_copd", "dx_demen", "dx_diabc", "dx_diabuc",
  "dx_hfail", "dx_hemi", "dx_mliver", "dx_mi", "dx_pud",
  "dx_pvd", "dx_renal", "dx_rheum", "dx_sliver"
)

# Define the equation set for models - 
# used to run comparison models and test matching specifications
eq_set <- c(
  paste0("treat ~ age_group + ", outcome_var,
         " + familie_type + pre_socio + fam_size + edu + reg + ",
         paste0(dx_vars, collapse = " + ")),
  paste0("treat ~ age_group + ", outcome_var,
         " + familie_type + pre_socio + fam_size + edu + ",
         paste0(dx_vars, collapse = " + ")),
  paste0("treat ~ age_group + ", outcome_var,
         " + familie_type + pre_socio + fam_size + edu + reg + cci"),
  paste0("treat ~ age_group + ", outcome_var,
         " + familie_type + pre_socio + fam_size + edu + cci"),
  paste0("treat ~ age_group + ", outcome_var,
         " + familie_type + pre_socio + fam_size + edu + reg"),
  paste0("treat ~ age_group + ", outcome_var,
         " + familie_type + pre_socio + fam_size + edu"),
  "treat ~ age_group + familie_type + pre_socio + fam_size + edu + reg + cci",
  "treat ~ age_group + edu + as.factor(retired)",
  "treat ~ age_group + as.factor(retired)"
)

# Construct the model matrix for matching
model_matrix <- data.frame()

model_matrix <- expand(model_matrix, eq_set, lookbacks)
setDT(model_matrix)

# Update model matrix for specific matching methods
model_matrix[eq_set := str_replace(eq_set, "fam_ind", "fam_inc_qt")]
model_matrix[eq_set := str_replace(eq_set, "per_ind", "per_inc_qt")]

model_matrix <- unique(model_matrix)
model_matrix[, index := paste0("m", .I)]

# Create the model directory if it doesn't exist
dir.create(model_dir, showWarnings = FALSE)

# Save parameters for the matching process
save("dbcg", treatment_year, treatment_match_var, outcome_var, 
     model_matrix, file = paste0(model_dir, "params.RData"))

# Prepare for parallel processing based on years
years <- year_set[cause == "dbcg"]$st_yr:year_set[cause == "dbcg"]$ed_yr
cl <- makeCluster(length(years))  # Create a cluster for parallel processing
clusterExport(cl, c(
  "model_matrix", "model_dir", "outcome_var"
))  # Export necessary variables to the cluster
clusterEvalQ(cl, {
  require(MatchIt)  # Load MatchIt package in each cluster
  source("code/environment_setup.R")  # Load environment setup
  source("code/02_analysis/match_functions.R")  # Load matching functions
})

# Apply the matching process for each year in parallel
clusterApply(cl, years, function(y) {
  # Load the dataset for the specific year
  year_df <- open_dataset("data/analysis/01_matching/01_pre_match") %>%
    filter(year == y) %>%
    collect() %>%
    setDT()

  # Prepare treatment and outcome variables
  year_df$treat <- year_df[["dbcg"]]
  year_df$treat_yr <- year_df[["dbcg_dx_yr"]]
  year_df$outcome <- year_df[[outcome_var]]

  year_df[is.na(treat), treat := 0]  # Handle missing treatment values

  # Iterate through each model in the model matrix
  lapply(seq_len(nrow(model_matrix)), function(i) {
    m <- model_matrix[i]

    if (y + m$lookbacks <= 2015) {
      print(m)  # Print the current model being processed
      ydf <- copy(year_df)[year_df[["dbcg"]] == 0 |
                             year_df[["dbcg_dx_yr"]] >= y + m$lookback]
      ydf$treat <- ydf[["dbcg"]]
      ydf[treat_yr > y + m$lookback, treat := 0]

      # Run the matching process with the specified parameters
      run_matches(
        match_type = m$method, distance_type = m$dist, 
        eqn = m$eq_set, data = ydf, name = m$index, 
        model_dir = model_dir, c_ratio = 3
      )
    }

    gc()  # Trigger garbage collection
  })
})

stopCluster(cl)  # Stop the cluster after processing is complete
