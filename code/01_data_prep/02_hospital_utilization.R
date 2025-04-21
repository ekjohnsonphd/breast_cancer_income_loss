# Purpose: Count hospitaliz utilization per person per year 
# to create a utilization index

# Load environment settings and common packages
source("code/environment_setup.R")
# Load heaven package for clinical data tools (e.g., Charlson index)
library(heaven)

# Loop through years 1995–2018 and count unique hospital admissions 
# per person per year
lpr <- lapply(1995:2018, function(y) {
  # Load hospital admission records and filter by year
  lpr_adm <- open_dataset("data/rawdata/lpr_adm") %>%
    select(PNR, RECNUM, D_INDDTO, year) %>%
    filter(year == y) %>%
    collect() %>%
    setDT()

  # Count number of unique admissions per person in the given year
  lpr_adm <- lpr_adm[, .(util = n_distinct(RECNUM)), by = c("year", "PNR")]
  return(lpr_adm)
}) %>% rbindlist()

# Create hospital utilization index based on 2 years prior to each 
# index year (2000–2015)
index_years <- 2000:2015
util_by_index <- lapply(index_years, function(y) {
  # Subset data to 2 years prior to the index year
  util <- copy(lpr)[year == y - 1 | year == y - 2]
  # Sum admissions across the 2-year window for each individual
  util <- util[, .(util = sum(util), year = y), by = "PNR"]
  return(util)
}) %>% rbindlist()

# Save final utilization index to disk
write_dataset(util_by_index, "data/analysis/inp_util")
