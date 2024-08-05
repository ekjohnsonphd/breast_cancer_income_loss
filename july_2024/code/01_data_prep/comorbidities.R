source("code/environment_setup.R")
library(heaven)


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

  lpr_y <- merge(lpr_adm, lpr_y, by = c("recnum", "year"), all = TRUE, allow.cartesian = TRUE)
  lpr_y <- melt(lpr_y, measure.vars = c("c_diag", "c_tildiag"), value.name = "dx")
  lpr_y <- lpr_y[dx != ""]
  lpr_y[, recnum := NULL]
  lpr_y[, dx := str_sub(dx, 1, 5)]

  rm(lpr_adm)
  gc()

  return(lpr_y)
})

lpr <- rbindlist(lpr)
lpr <- unique(lpr)
lpr[, pnr := as.factor(pnr)]

cdates <- paste0(2000:2015, "-01-01")
dx <- lapply(cdates, function(c_morb_date) {
  print(c_morb_date)
  cyear <- year(c_morb_date)
  lpr_year <- copy(lpr[year <= cyear & year > cyear - 6])
  lpr_year[, charlson_date := as.Date(c_morb_date, format = "%Y-%m-%d")]
  cci_year <- charlsonIndex(lpr_year,
    ptid = "pnr", vars = "dx", data.date = "d_inddto",
    charlson.date = "charlson_date"
  )
  cci_year <- merge(cci_year[1], cci_year[2])
  setDT(cci_year)
  cci_year[, year := cyear][, charlson_date := NULL]
  return(cci_year)
})


dx <- rbindlist(dx)
write_dataset(dx, "data/analysis/cci")

# # lpr_data <- copy(lpr[,.(year, pnr, d_inddto, variable, dx)])
#
# lpr[, charlson_dt := as.Date('2013-01-01', format = '%Y-%m-%d')]
#
# lpr[, pnr := as.numeric(factor(pnr))]
#
# charlsonIndex(lpr, ptid = 'pnr', vars = 'dx', data.date = 'd_inddto', charlson.date = 'charlson_dt')
