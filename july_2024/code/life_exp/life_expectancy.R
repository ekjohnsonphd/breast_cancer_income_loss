df <- open_dataset("data/analysis/01_matching/01_pre_match") %>%
  filter(!is.na(aod)) %>%
  select(year, alder, sex, familie_type, ie_type, fam_inc_qt, per_inc_qt, kom, cci, edu, fam_size, yod, aod) %>%
  collect() %>%
  setDT()

df <- df[year == yod - 1]

annual_quartiles <- df[, as.list(quantile(aod, probs = seq(0, 1, 0.2))), by = "year"]
colnames(annual_quartiles) <- c("year", "min", "q1", "q2", "q3", "q4", "max")

df <- merge(df, annual_quartiles, by = "year", all.x = TRUE)
df[aod <= q1, qt := "q1"]
df[aod <= q2, qt := "q2"]
df[aod <= q3, qt := "q3"]
df[aod <= q4, qt := "q4"]
df[is.na(qt), qt := "q5"]


df[, `:=`(min = NULL, q1 = NULL, q2 = NULL, q3 = NULL, q4 = NULL, max = NULL)]

q1s <- dfSummary(df[qt == "q1" & year == 2017, .(alder, sex, familie_type, fam_size, ie_type, edu, fam_inc_qt, per_inc_qt, cci)])
q5s <- dfSummary(df[qt == "q5" & year == 2017, .(alder, sex, familie_type, fam_size, ie_type, edu, fam_inc_qt, per_inc_qt, cci)])
