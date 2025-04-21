# Purpose: derive Charlson comorbidity index and diagnoses from LPR data
# Functions loop through LPR hospitalization diagnoses in the last 5 years,
# using the heaven package to derive comorbidity index and diagnoses

# Load environment and package setup (e.g., paths, common libraries)
source("code/environment_setup.R")
# Load heaven package for Charlson comorbidity index calculation
library(heaven)

# Loop through years 1995 to 2015 and load + merge diagnosis data from LPR
lpr <- lapply(1995:2015, function(y) {
  lpr_y <- open_dataset("data/rawdata/lpr_diag") %>%
    select(RECNUM, C_DIAG, C_TILDIAG, year) %>%
    filter(year == y) %>%
    collect() %>%
    setDT()

  lpr_adm <- open_dataset("data/rawdata/lpr_adm") %>%
    select(PNR, RECNUM, D_INDDTO, year) %>%
    filter(year == y) %>%
    collect() %>%
    setDT()

  colnames(lpr_y) <- tolower(colnames(lpr_y))
  colnames(lpr_adm) <- tolower(colnames(lpr_adm))

  lpr_y <- merge(lpr_adm, lpr_y,
    by = c("recnum", "year"),
    all = TRUE, allow.cartesian = TRUE
  )
  # Reshape diagnosis columns into long format for analysis
  lpr_y <- melt(lpr_y,
    measure.vars = c("c_diag", "c_tildiag"), value.name = "dx"
  )
  # Remove empty diagnosis entries
  lpr_y <- lpr_y[dx != ""]
  # Truncate diagnosis codes to first 5 characters
  lpr_y[, dx := str_sub(dx, 1, 5)]

  # Clean up memory
  rm(lpr_adm)
  gc()

  return(lpr_y)
}) %>%
  rbindlist() %>%
  unique()

# Ensure personal ID is treated as a factor
lpr[, pnr := as.factor(pnr)]

# Create sequence of dates for Charlson index evaluation (Jan 1st of each year)
cdates <- paste0(2000:2015, "-01-01")
# For each evaluation date, calculate the Charlson index using the previous 5 years of data
dx <- lapply(cdates, function(c_morb_date) {
  # Extract current year from the Charlson evaluation date
  cyear <- year(c_morb_date)
  # Subset LPR data to relevant years for comorbidity calculation
  lpr_year <- copy(lpr[year <= cyear & year > cyear - 6])
  # Add Charlson index evaluation date to data
  lpr_year[, charlson_date := as.Date(c_morb_date, format = "%Y-%m-%d")]
  # Run Charlson index function from heaven package
  cci_year <- charlsonIndex(lpr_year,
    ptid = "pnr", vars = "dx", data.date = "d_inddto",
    charlson.date = "charlson_date"
  )
  # Merge index scores with comorbidity diagnosis info
  cci_year <- merge(cci_year[1], cci_year[2])
  setDT(cci_year)
  # Add year column and clean up temp variables
  cci_year[, year := cyear][, charlson_date := NULL]
  return(cci_year)
}) %>% rbindlist()

# Write final Charlson comorbidity data to disk
write_dataset(dx, "data/analysis/cci")
