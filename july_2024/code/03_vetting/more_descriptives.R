rm(list = ls())
gc()
source("code/environment_setup.R")
library(yardstick)
library(GGally)
library(vtable)

df <- open_dataset("data/analysis/01_matching/01_pre_match") %>%
  collect() %>%
  setDT()


desc_cols <- c("alder", "sex", "familie_type", "ie_type", "pre_socio", "edu", "fam_size")

# table1 <- function()
# skim(df[year == 2015,.(alder, sex, familie_type, ie_type, pre_socio, per_ind, fam_ind, edu, fam_size)])


sumdf <- copy(df[year == 2015])
sumdf[, grp := "Overall"]
sumdf <- rbind(
  sumdf,
  copy(sumdf[dbcg == 1 & dbcg_dx_yr == 2014])[, grp := "dbcg"],
  copy(sumdf[ddd == 1 & ddd_dx_yr == 2014])[, grp := "ddd"],
  copy(sumdf[nab == 1 & nab_dx_yr == 2014])[, grp := "nab"],
  copy(sumdf[dap == 1 & dap_dx_yr == 2014])[, grp := "dap"]
)

st(sumdf,
  vars = desc_cols, summ = c("mean(x)", "median(x)", "sd(x)", "propNA(x)"),
  group = "grp", out = "csv",
  file = "data/analysis/99_vetting/apr24_parallel_trends_and_descriptives/table1.csv"
)


match_groups <- copy(df)[dbcg == 0 | dbcg_dx_yr == year - 1]
match_groups <- count(
  match_groups, dbcg, year, age_group, sex, familie_type, ie_type,
  pre_socio, edu, fam_size, fam_inc_qt
)
match_groups <- dcast(match_groups, ... ~ dbcg, value.var = "n")
match_groups <- match_groups[year %in% 1999:2014]
setnames(match_groups, c("0", "1"), c("control", "treat"))
match_groups[is.na(control), control := 0]
match_groups <- match_groups[!is.na(treat)]

count(match_groups[control == 0], treat)

pdf("data/analysis/99_vetting/apr24_parallel_trends_and_descriptives/match_groups.pdf", onefile = TRUE)
ggplot(match_groups) +
  geom_hline(aes(yintercept = 0), color = "red") +
  geom_bin2d(aes(x = treat, y = control), bins = 70) +
  theme_bw() +
  labs(
    x = "Number of exposed in match group", y = "Number of controls in match group", title = "Match group size",
    subtitle = "Matched by age, sex, family size, relationship status, immigration, labor attachment, education, income"
  )

ggplot(match_groups) +
  geom_hline(aes(yintercept = 0), color = "red") +
  geom_bin2d(aes(x = treat, y = control), bins = 70) +
  theme_bw() +
  labs(
    x = "Number of exposed in match group", y = "Number of controls in match group", title = "Match group size",
    subtitle = "Matched by age, sex, family size, relationship status, immigration, labor attachment, education, income"
  ) +
  scale_y_log10()

ggplot(match_groups) +
  geom_histogram(aes(x = treat + control)) +
  geom_histogram(data = match_groups[treat + control == 1], aes(x = treat + control), fill = "red") +
  theme_bw() +
  scale_x_log10() +
  labs(x = "Match group size", y = "Count", title = "Match group size")

dev.off()

# Correlation tables ------------
cdf <- copy(df)[year == 2015, .(
  pid, year, familie_type, ie_type, pre_socio, age_group, alder, dbcg,
  ddd, dap, nab, sex, edu, fam_size, fam_ind, per_ind
)]

ggpairs(cdf[dbcg == 1, .(alder, sex, edu, familie_type, fam_size, fam_ind, per_ind)])
