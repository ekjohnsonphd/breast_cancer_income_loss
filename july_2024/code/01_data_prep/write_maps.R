source("code/environment_setup.R")

# edu <- fread('PATH/data/Formater/SAS formater i Danmarks Statistik/TXT_filer/Uddannelser/C_AUDD2015_L1L5_KT.TXT')
# colnames(edu) <- c('hfaudd','edu')
# edu[, edu_desc := str_sub(edu, 4L, -1L)]
# edu[, edu := str_sub(edu, 1L, 2L)]
library(heaven)
edu <- edu_code

save(edu, file = "data/supplemental_inputs/edu.RData")

# inflation adjustment
cpi_dk <- get_data(
  table_id = "PRIS8",
  variables = list(list(code = "TYPE", values = NA), list(code = "Tid", values = NA))
)
setDT(cpi_dk)
cpi_dk <- cpi_dk[, .(year = TID, inf_rate = cpi_dk[TID == 2023]$INDHOLD / INDHOLD)]
cpi_dk[, inf_rate := round(inf_rate, 3)]
save(cpi_dk, file = "data/supplemental_inputs/cpi_dk.RData")

# currency conversion
eur_dkk <- get_data(
  table_id = "DNVALA",
  variables = list(
    list(code = "VALUTA", values = "EUR"), list(code = "KURTYP", values = "KBH"),
    list(code = "OPGOER", values = "E"), list(code = "Tid", values = NA)
  )
)
setDT(eur_dkk)
eur_dkk <- eur_dkk[TID >= 1999, .(year = TID, eur_rate = as.numeric(INDHOLD) / 100)]
eur_dkk <- rbind(eur_dkk, data.table(year = 1995:1999, eur_rate = eur_dkk[year == 1999]$eur_rate))

save(eur_dkk, file = "data/supplemental_inputs/eur_dkk.RData")

# life expectancy
life_expectancy <- get_data("HISB8",
  variables = list(
    list(code = "ALDER", values = NA), list(code = "KØN", values = NA),
    list(code = "TAVLE", values = 3), list(code = "TID", values = NA)
  )
)
setDT(life_expectancy)
life_expectancy <- life_expectancy[, .(
  alder = as.integer(str_remove(ALDER, " year\\w?")), sex = KØN,
  year = as.integer(str_sub(TID, 6, -1L)), indhold = round(INDHOLD / 100)
)]
life_expectancy[sex == "Men", sex := "male"][sex == "Women", sex := "female"]
life_expectancy[, indhold := indhold + alder]

save(life_expectancy,
  file = "data/supplemental_inputs/life_expectancy.RData"
)


# regions
regs <- fread("PATH/Formater/SAS formater i Danmarks Statistik/TXT_filer/Geokoder/C_KOMREG_V4_T.txt")
colnames(regs) <- c("kom", "reg")
regs[reg == "Sj\xe6lland", reg := "Sjaelland"]
save(regs, file = "data/supplemental_inputs/regions.RData")
