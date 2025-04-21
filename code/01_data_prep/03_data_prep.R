# Purpose: combine register data files and do variable processing for analysis.
# First, breast cancer data is read, then other data is read and merged 
# in a loop over data years for computational efficiency

source("code/environment_setup.R")

# Create a named vector to map education codes to text descriptions
edu_coding <- edu$code5txt
names(edu_coding) <- edu$hfaudd

# Load region code data and convert municipality codes to character
load("data/supplemental_inputs/regions.RData")
regs$kom <- as.character(regs$kom)

# Map sex codes to labels
sex_coding <- c("male", "female")
names(sex_coding) <- c(1, 2)

# Process breast cancer diagnosis data (DBCG)
dbcg <- open_dataset("data/rawdata/dbcg/") %>%
  select(PNR = CPR, year = Aar) %>%
  mutate(dbcg_dx_yr = year) %>%
  collect() %>%
  setDT()
dbcg_years <- expand(dbcg, PNR, year)
dbcg <- merge(dbcg[, .(PNR, dbcg_dx_yr)], dbcg_years, all = TRUE)
dbcg[, dbcg := 1]
dbcg$year <- as.integer(dbcg$year)

# Process death data (DOD) and extract year and age of death
dod <- open_dataset("data/rawdata/dod") %>%
  select(PNR, DODDATO, ALDER_HAEND) %>%
  collect() %>%
  setDT()
dod <- dod[, .(PNR, yod = year(DODDATO), aod = ALDER_HAEND)]

# Set up parallel processing for looping through years
yearlist <- 1995:2018
cl <- makeCluster(length(yearlist))
clusterExport(cl, c("dbcg", "dod", "edu_coding", "sex_coding", "regs"))
clusterEvalQ(cl, {
  source("code/environment_setup.R")
})

parLapplyLB(cl, yearlist, function(yr) {
  # Load household and demographic info (BEF)
  bef <- open_dataset("data/rawdata/bef/") %>%
    filter(year == yr) %>%
    select(year, PNR, KOM, ALDER, KOEN, ANTPERSF, 
           FAMILIE_TYPE, FAMILIE_ID) %>%
    collect()

  # Load family income and social group (FAIK)
  faik <- open_dataset("data/rawdata/faik/") %>%
    filter(year == yr) %>%
    select(year, FAMILIE_ID, FAMAEKVIVADISP_13, FAMSOCIOGRUP_13) %>%
    collect() %>%
    unique()

  # Load pre-income and socio status (IND)
  ind <- open_dataset("data/rawdata/ind_open") %>%
    filter(year == yr) %>%
    select(year, PNR, PRE_SOCIO, PERINDKIALT_13) %>%
    collect()

  # Load education codes (UDDA)
  udda <- open_dataset("data/rawdata/udda") %>%
    filter(year == yr) %>%
    select(year, PNR, HFAUDD) %>%
    collect()

  # Load employment status (IDAN)
  idan <- open_dataset("data/rawdata/idan") %>%
    filter(year == yr) %>%
    select(year, PNR, STILL) %>%
    collect()

  # Load Charlson comorbidity index
  cci <- open_dataset("data/analysis/cci") %>%
    filter(year == yr) %>%
    collect()
  setnames(cci, "pnr", "PNR")

  # Load healthcare utilization variables
  util <- open_dataset("data/analysis/util") %>%
    filter(year == yr) %>%
    collect()

  # Merge all datasets for the given year
  df <- left_join(bef, faik, by = c("FAMILIE_ID", "year")) %>%
    left_join(ind, by = c("PNR", "year")) %>%
    left_join(udda, by = c("PNR", "year")) %>%
    left_join(dod, by = c("PNR")) %>%
    left_join(cci, by = c("PNR", "year")) %>%
    left_join(util, by = c("PNR", "year")) %>%
    left_join(dbcg, by = c("PNR", "year")) %>%
    rename_with(tolower)

  setDT(df)

  # Recode categorical variables for analysis
  # Create age group variable from exact age
  df[, age_group := cut(alder,
    breaks = c(18, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 120),
    labels = c(
      "18to29", "30to34", "35to39", "40to45", "45to49", "50to54", "55to59",
      "60to64", "65to69", "70to74", "75to79", "80to84", "85to89", "90to120"
    ),
    include.lowest = TRUE, right = FALSE
  )]
  df[, age_group := as.character(age_group)]
  df[, alder := as.integer(alder)]
  # Recode and label pre-socioeconomic status
  df[pre_socio %in% c(110, 111, 112, 113, 114), pre_socio := 110]
  df$pre_socio <- factor(df$pre_socio,
    exclude = NULL,
    levels = c(
      "110", "120", "130", "210", "220", "310", "321", "322", "323",
      "330", "410", "420", NA
    ),
    labels = c(
      "self_employed", "working_spouse", "wage_earner", "unemployed_6mo",
      "leave_recipient", "in_school", "disab_recipient", "retired",
      "early_retirement", "unemp_recipient", "other", "children", "unknown"
    )
  )
  df[, pre_socio := as.character(pre_socio)]
  # Recode and label family social group
  df[famsociogrup_13 %in% c(110, 111, 112, 113, 114), famsociogrup_13 := 110]
  df[
    famsociogrup_13 %in% c(131, 132, 133, 134, 135, 139),
    famsociogrup_13 := 130
  ]
  df$famsociogrup_13 <- factor(df$famsociogrup_13,
    exclude = NULL,
    levels = c(
      "110", "120", "130", "210", "220", "310", "321", "322", "323",
      "330", "410", "420", NA
    ),
    labels = c(
      "self_employed", "working_spouse", "wage_earner", "unemployed_6mo",
      "leave_recipient", "in_school", "disab_recipient", "retired",
      "early_retirement", "unemp_recipient", "other", "children", "unknown"
    )
  )
  df[, famsociogrup_13 := as.character(famsociogrup_13)]
  # Recode sex
  df[, sex := sex_coding[koen]]
  df[, koen := as.integer(koen)]
  # Collapse family type categories and label them
  df[familie_type %in% c(5, 9, 10), familie_type := 5]
  df[familie_type %in% c(1, 2, 7, 8), familie_type := 1]
  df[familie_type %in% c(3, 4), familie_type := 3] # or 3 = unmarried partnered
  df$familie_type <- factor(df$familie_type,
    levels = c("1", "3", "5"),
    labels = c("married", "partnered_unmarried", "single")
  )
  df[, familie_type := as.character(familie_type)]
  # Map education code to text
  df[, edu := edu_coding[as.character(hfaudd)]]
  df[is.na(edu), edu := "-1"]
  # Categorize family size
  df[, fam_size := as.character(antpersf)]
  df[antpersf >= 3, fam_size := "3+"]
  df[, antpersf := as.integer(antpersf)]
  # Merge with region data by municipality code
  df <- merge(df, regs, by = "kom")
  # Final formatting and write to Parquet files for downstream analysis
  df <- df %>%
    rename_with(tolower) %>%
    group_by(year, age_group) %>%
    write_dataset("data/analysis/01_matching/00_matching_inputs/",
      format = "parquet", existing_data_behavior = "overwrite"
    )
})

stopCluster(cl)
