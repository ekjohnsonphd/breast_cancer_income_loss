## Script to produce statistics for paper on income loss after breast cancer
# Clear environment
rm(list = ls())
gc()
source("code/environment_setup.R")
source("code/02_analysis/match_functions.R")

## Data and setup ------------------------------------------
# Format median and IQR for display
med_iqr <- function(x) {
  return(paste0(
    round(median(x)), " (", round(quantile(x, 0.25)),
    " - ", round(quantile(x, 0.75)), ")"
  ))
}

# Format ATT result for display
format_results <- function(x) {
  return(paste0(round(x$att), " (", round(x$lower), " - ", round(x$upper), ")"))
}

# Load baseline dataset
df <- open_dataset("data/analysis/01_matching/01_pre_match") %>%
  collect() %>%
  setDT()

# Load personal income model and matched data
model_info_per <- fread("model_comparisons_10y.csv")
match_df_per <- open_dataset("02_matched/dbcg/per_ind/2024-05-25/m13") %>%
  collect() %>%
  setDT()
cate_df_per <- fread("03_cate/dbcg/per_ind/2024-05-25/m13.csv")
cate_df_per[, retired := 0]
cate_df_per[str_detect(pre_socio, "retire"), retired := 1]
# Create counterfactual time series including years after death
cate_df_per_deaths_cfactual <- get_match_time_series(
  "02_matched/dbcg/per_ind/2024-05-25/m13",
  treatment_var = "dbcg", treatment_year = "dbcg_dx_yr",
  outcome_var = "per_ind",
  follow_up_length = 10, lookback = 1, deaths_as_0 = FALSE,
  agg = TRUE, prematch = FALSE
)
cate_df_per_deaths_cfactual[, retired := 0]
cate_df_per_deaths_cfactual[str_detect(pre_socio, "retire"), retired := 1]
# Load household income model and matched data
model_info_fam <- fread("02_matched/dbcg/fam_ind/2024-05-25/model_comparisons_10y.csv")
match_df_fam <- open_dataset("02_matched/dbcg/fam_ind/2024-05-25/m10") %>%
  collect() %>%
  setDT()
cate_df_fam <- fread("03_cate/dbcg/fam_ind/2024-05-25/m10.csv")
cate_df_fam[, retired := 0]
cate_df_fam[str_detect(pre_socio, "retire"), retired := 1]
# Initialize results list
results <- list()

## Intro ----------------------------------------------------
 # Sample size statistics
# total sample size
results["total sample size"] <- length(unique(df$pid))

## Results - descriptives ----------------------------------------------------
# Descriptive statistics
## 6 total stats

# total dbcg sample size
results["breast cancer sample size"] <- nrow(df[year == dbcg_dx_yr &
                                                  year %in% 1999:2014])

# total person-years of data
pyears <- count(df[year %in% 2000:2018 &
                     (is.na(dbcg_dx_yr) | dbcg_dx_yr < 2014)], pid)
results["total person years"] <- sum(pyears$n)

# person years per individual
results["pyears per person"] <- round(mean(pyears$n), 1)

# age at dbcg dx
dbcg <- df[year == dbcg_dx_yr & year %in% 2000:2014]
results["breast cancer age"] <- med_iqr(dbcg$alder)

# median incomes
results["median personal income"] <- med_iqr(df[year %in% 2000:2018]$per_ind)
results["median hhold income"] <- med_iqr(df[year %in% 2000:2018]$fam_ind)

## Results - match metadata ----------------------------------------------------
 # Matching diagnostics
## 9 total stats

match_df_per[, sbc := .GRP, by = c("subclass", "match_yr")]
group_df_per <- count(match_df_per, sbc)
match_df_fam[, sbc := .GRP, by = c("subclass", "match_yr")]
group_df_fam <- count(match_df_fam, sbc)

results["# match groups"] <- length(unique(match_df_per$sbc))

# results['min match group size'] <- min(group_df_per$n, group_df_fam$n)
results["max match group size"] <- max(group_df_per$n, group_df_fam$n)

results["personal income lost breast cancer pop"] <- nrow(df[year == dbcg_dx_yr & year %in% 1999:2014]) - model_info_per[index == "m13"]$n_treat
results["hhold income lost breast cancer pop"] <- nrow(df[year == dbcg_dx_yr & year %in% 1999:2014]) - model_info_per[index == "m10"]$n_treat

results["personal income small group percentage"] <- 100 * nrow(group_df_per[n < 5]) / nrow(group_df_per)
results["personal income median group size"] <- median(group_df_per$n)

results["hhold income small group percentage"] <- 100 * nrow(group_df_fam[n < 5]) / nrow(group_df_fam)
results["hhold income median group size"] <- median(group_df_fam$n)

## Results - income loss and subgroups ----------------------------------------------------
# ATT estimates and subgroup analyses
## 22 total stats

cp <- calculate_att(cate_df_per)
ch <- calculate_att(cate_df_fam)

results["10 year personal income loss"] <- format_results(cp)
results["10 year hhold income loss"] <- format_results(ch)

cp_nr <- calculate_att(cate_df_per, "retired")[retired == 0]
ch_nr <- calculate_att(cate_df_fam, "retired")[retired == 0]

results["10 year nonretired personal income loss"] <- format_results(cp_nr)
results["10 year nonretired hhold income loss"] <- format_results(ch_nr)

med_per <- median(df[year %in% 2000:2018]$per_ind)
med_fam <- median(df[year %in% 2000:2018]$fam_ind)

results["10 year personal income loss percent of total"] <- paste0(
  round(100 * cp_nr$att / med_per, 2),
  "% (", round(100 * cp_nr$lower / med_per, 2), "% - ", round(100 * cp_nr$upper / med_per, 2), "%)"
)
results["10 year hhold income loss percent of total"] <- paste0(
  round(100 * ch_nr$att / med_fam, 2),
  "% (", round(100 * ch_nr$lower / med_fam, 2), "% - ", round(100 * ch_nr$upper / med_fam, 2), "%)"
)


cp_y10 <- calculate_att(cate_df_per, c("index", "retired"))[index == 9 & retired == 0]
results["personal income loss in year 10"] <- format_results(cp_y10)

deaths_cfactual_y10 <- calculate_att(cate_df_per_deaths_cfactual, c("index", "retired"))[index == 9 & retired == 0]
deaths_cfactual <- calculate_att(cate_df_per_deaths_cfactual, "retired")[retired == 0]

results["personal income loss deaths counterfactual in year 10"] <- format_results(deaths_cfactual_y10)
results["10 year personal income loss deaths counterfactual"] <- format_results(deaths_cfactual)

calculate_att(cate_df_fam, c("index", "retired")) # all insignificant -- need to change text if this changes

cp_pre_socio <- calculate_att(cate_df_per, "pre_socio")

results["personal income loss individuals in school"] <- format_results(cp_pre_socio[pre_socio == "in_school"])
results["personal income loss individuals working"] <- format_results(cp_pre_socio[pre_socio == "wage_earner"])

cp_pre_socio_min <- cp_pre_socio[att == min(cp_pre_socio[!str_detect(pre_socio, "retire")]$att)]
cp_pre_socio_max <- cp_pre_socio[att == max(cp_pre_socio[!str_detect(pre_socio, "retire|school|wage")]$att)]
results["personal income loss pre_socio lower range"] <- format_results(cp_pre_socio_min)
results["personal income loss pre_socio upper range"] <- format_results(cp_pre_socio_max)

results["personal income loss retirees"] <- format_results(cp_pre_socio[pre_socio == "retired"])

ch_pre_socio <- calculate_att(cate_df_fam, "pre_socio")

results["hhold income loss individuals working"] <- format_results(ch_pre_socio[pre_socio == "wage_earner"])
results["hhold income loss individuals in school"] <- format_results(ch_pre_socio[pre_socio == "in_school"])
results["hhold income loss unemployment recipients"] <- format_results(ch_pre_socio[pre_socio == "unemp_recipient"])

cp_edu <- calculate_att(cate_df_per, "edu")

results["personal income loss upper secondary eduation"] <- format_results(cp_edu[edu == "Upper secondary"])

cp_age <- calculate_att(cate_df_per, "match_age")
results["personal income loss age 18-29"] <- format_results(cp_age[match_age == "18to29"])
results["personal income loss age 60-64"] <- format_results(cp_age[match_age == "60to64"])

## write all results
# Export all results to CSV
fwrite(data.table(result = names(results), value = results), "data/paper1/paper1_results.csv")
