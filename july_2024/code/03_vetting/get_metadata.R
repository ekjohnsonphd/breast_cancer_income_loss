data_dir <- Sys.glob("data/rawdata/*")


metadata1 <- lapply(data_dir, function(dd) {
  yrs <- list.dirs(dd)
  yrs <- str_subset(yrs, "year=") %>% str_sub(-4, -1)
  # cols <- open_dataset(dd)$schema$names
  return(data.table(register = basename(dd), years = yrs))
}) %>% rbindlist()

metadata2 <- lapply(data_dir, function(dd) {
  cols <- open_dataset(dd)$schema$names
  return(data.table(register = basename(dd), variables = cols))
}) %>% rbindlist()

writexl::write_xlsx(
  list(Register_years = metadata1, Register_vars = metadata2),
  "E:/ProjektDB/OPEN/Workdata/xxx/hjemsendelse/project_registers.xlsx"
)
