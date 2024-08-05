library(tidyverse)
library(data.table)
library(arrow)
library(haven)
library(tools)

raw_data <- Sys.glob(c(
  "PATH/Grunddata/*.sas7bdat",
  "PATH/Eksterne data/*.sas7bdat"
))
raw_parquet_dir <- "PATH/data/rawdata/"

lapply(raw_data, function(file) {
  fname <- file_path_sans_ext(basename(file))
  print(fname)
  dset <- str_remove_all(fname, "\\d+")
  dset <- str_remove_all(dset, "_+$")
  yr <- str_remove_all(fname, "\\D+")

  if (!dir.exists(paste0(raw_parquet_dir, dset))) {
    print("Writing")

    dt <- read_sas(file)
    if (yr == "") {
      yr <- 2018
    }
    dt$year <- as.numeric(yr)

    dt$dset <- dset

    dt <- dt %>%
      group_by(year) %>%
      write_dataset(path = paste0(raw_parquet_dir, dset), format = "parquet")
  }
})

# unlink(paste0("data/rawdata/studiepopulation"), recursive = TRUE)

# unlink(str_subset(Sys.glob(paste0(raw_parquet_dir,"*")), "nab"), recursive = TRUE)
