source('code/environment_setup.R')
library(heaven)

lpr <- lapply(1995:2018, function(y){
  
  lpr_adm <- open_dataset("data/rawdata/lpr_adm") %>%
    select(PNR, RECNUM, D_INDDTO, year) %>%
    filter(year == y) %>%
    collect() %>% setDT()
  
  lpr_adm <- lpr_adm[,.(util = n_distinct(RECNUM)), by = c('year','PNR')]
  return(lpr_adm)

}) %>% rbindlist()


# now create utilization indices - based on 2 years prior to index year
index_years <- 2000:2015
util_by_index <- lapply(index_years, function(y){
  util <- copy(lpr)[year == y-1 | year == y-2]
  util <- util[,.(util = sum(util), year = y), by = 'PNR']
  return(util)
}) %>% rbindlist()

write_dataset(util_by_index, 'data/analysis/inp_util')
