source("code/environment_setup.R")

edu_coding <- edu$code5txt
names(edu_coding) <- edu$hfaudd

load("data/supplemental_inputs/regions.RData")
regs$kom <- as.character(regs$kom)

sex_coding <- c("male", "female")
names(sex_coding) <- c(1, 2)

# Disease files coding -----------------------------------------------------------------------------------
dbcg <- open_dataset("data/rawdata/dbcg/") %>%
  select(PNR = CPR, year = Aar) %>%
  mutate(dbcg_dx_yr = year) %>%
  collect() %>%
  setDT()
dbcg_years <- expand(dbcg, PNR, year)
dbcg <- merge(dbcg[, .(PNR, dbcg_dx_yr)], dbcg_years, all = TRUE)
dbcg[, dbcg := 1]
dbcg$year <- as.integer(dbcg$year)

ddd <- open_dataset("data/rawdata/ddd/") %>%
  select(PNR = CPR, year = Indl_Dato_Start) %>%
  mutate(ddd_dx_yr = year) %>%
  collect() %>%
  setDT()
ddd[, year := year(year)][, ddd_dx_yr := year(ddd_dx_yr)]

# for now - get incidence only
ddd <- ddd[, .(year = min(year), ddd_dx_yr = min(ddd_dx_yr)), by = "PNR"]

ddd_years <- expand(ddd, PNR, year)
ddd <- merge(ddd[, .(PNR, ddd_dx_yr)], ddd_years, all = TRUE, allow.cartesian = TRUE)
ddd <- ddd[year >= ddd_dx_yr - 1]
ddd[, ddd := 1]
ddd$year <- as.integer(ddd$year)

dap <- open_dataset("data/rawdata/strokedap/") %>%
  select(PNR = CPR, year = akutdato) %>%
  mutate(dap_dx_yr = year) %>%
  collect() %>%
  setDT()
dap[, year := year(year)][, dap_dx_yr := year(dap_dx_yr)]

# for now - get incidence only
dap <- dap[, .(year = min(year), dap_dx_yr = min(dap_dx_yr)), by = "PNR"]

dap_years <- expand(dap, PNR, year)
dap <- merge(dap[, .(PNR, dap_dx_yr)], dap_years, all = TRUE, allow.cartesian = TRUE)
dap <- dap[year >= dap_dx_yr - 1]
dap[, dap := 1]
dap$year <- as.integer(dap$year)

nab <- open_dataset("data/rawdata/t_nab_grundoplysninger/") %>%
  select(PNR = TXCPR, year = D_INDDATO) %>%
  mutate(nab_dx_yr = year) %>%
  collect() %>%
  setDT()
nab[, year := year(year)][, nab_dx_yr := year(nab_dx_yr)]

# for now - get incidence only
nab <- nab[, .(year = min(year), nab_dx_yr = min(nab_dx_yr)), by = "PNR"]

nab_years <- expand(nab, PNR, year)
nab <- merge(nab[, .(PNR, nab_dx_yr)], nab_years, all = TRUE, allow.cartesian = TRUE)
nab <- nab[year >= nab_dx_yr - 1]
nab[, nab := 1]
nab$year <- as.integer(nab$year)

dod <- open_dataset("data/rawdata/dod") %>%
  select(PNR, DODDATO, ALDER_HAEND) %>%
  collect() %>%
  setDT()
dod <- dod[, .(PNR, yod = year(DODDATO), aod = ALDER_HAEND)]

# Loop over year to combine data
yearlist <- 1995:2018
cl <- makeCluster(length(yearlist))
clusterExport(cl, c("dbcg", "ddd", "dap", "nab", "dod", "edu_coding", "sex_coding", "regs"))
clusterEvalQ(cl, {
  source("code/environment_setup.R")
})

parLapplyLB(cl, yearlist, function(yr) {
  print(yr)
  bef <- open_dataset("data/rawdata/bef/") %>%
    filter(year == yr) %>%
    select(year, PNR, KOM, ALDER, KOEN, ANTPERSF, FAMILIE_TYPE, IE_TYPE, FAMILIE_ID) %>%
    collect()

  faik <- open_dataset("data/rawdata/faik/") %>%
    filter(year == yr) %>%
    select(year, FAMILIE_ID, FAMAEKVIVADISP_13, FAMSOCIOGRUP_13) %>%
    collect() %>%
    unique()

  ind_open <- open_dataset("data/rawdata/ind_open") %>%
    filter(year == yr) %>%
    select(year, PNR, PRE_SOCIO, PERINDKIALT_13) %>%
    collect()

  # ind <- open_dataset("data/rawdata/ind") %>%
  #   select(-dset)

  udda <- open_dataset("data/rawdata/udda") %>%
    filter(year == yr) %>%
    select(year, PNR, HFAUDD) %>%
    collect()

  idan <- open_dataset("data/rawdata/idan") %>%
    filter(year == yr) %>%
    select(year, PNR, STILL) %>%
    collect()

  cci <- open_dataset("data/analysis/cci") %>%
    filter(year == yr) %>%
    collect()
  setnames(cci, "pnr", "PNR")

  util <- open_dataset("data/analysis/util") %>%
    filter(year == yr) %>%
    collect()

  # Combining data -----------------------------------------------------------------------------------
  df <- left_join(bef, faik, by = c("FAMILIE_ID", "year")) %>%
    left_join(ind_open, by = c("PNR", "year")) %>%
    left_join(udda, by = c("PNR", "year")) %>%
    left_join(dod, by = c("PNR")) %>%
    left_join(cci, by = c("PNR", "year")) %>%
    left_join(util, by = c("PNR", "year")) %>%
    left_join(dbcg, by = c("PNR", "year")) %>%
    left_join(ddd, by = c("PNR", "year")) %>%
    left_join(dap, by = c("PNR", "year"), relationship = "many-to-many") %>%
    left_join(nab, by = c("PNR", "year"), relationship = "many-to-many") %>%
    rename_with(tolower)

  setDT(df)

  # Factor coding -----------------------------------------------------------------------------------
  df[, age_group := cut(alder,
    breaks = c(18, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 120),
    labels = c(
      "18to29", "30to34", "35to39", "40to45", "45to49", "50to54", "55to59", "60to64",
      "65to69", "70to74", "75to79", "80to84", "85to89", "90to120"
    ),
    include.lowest = TRUE, right = FALSE
  )]
  df[, age_group := as.character(age_group)]
  df[, alder := as.integer(alder)]
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
  df[famsociogrup_13 %in% c(110, 111, 112, 113, 114), famsociogrup_13 := 110]
  df[famsociogrup_13 %in% c(131, 132, 133, 134, 135, 139), famsociogrup_13 := 130]
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
  df[, sex := sex_coding[koen]]
  df[, koen := as.integer(koen)]
  df[familie_type %in% c(5, 9, 10), familie_type := 5]
  df[familie_type %in% c(1, 2, 7, 8), familie_type := 1]
  df[familie_type %in% c(3, 4), familie_type := 3] # or 3 = unmarried partnered
  df$familie_type <- factor(df$familie_type,
    levels = c("1", "3", "5"),
    labels = c("married", "partnered_unmarried", "single")
  )
  df[, familie_type := as.character(familie_type)]
  df$ie_type <- factor(df$ie_type,
    levels = c(1, 2, 3), labels = c("danish", "immigrant", "descendants")
  )
  df[, ie_type := as.character(ie_type)]
  df[, edu := edu_coding[as.character(hfaudd)]]
  df[is.na(edu), edu := "-1"]
  df[, fam_size := as.character(antpersf)]
  df[antpersf >= 3, fam_size := "3+"]
  df[, antpersf := as.integer(antpersf)]

  df <- merge(df, regs, by = "kom")

  df <- df %>%
    rename_with(tolower) %>%
    group_by(year, age_group) %>%
    write_dataset("data/analysis/01_matching/00_matching_inputs/",
      format = "parquet", existing_data_behavior = "overwrite"
    )
})

stopCluster(cl)
