rm(list = ls())
gc()
source("code/environment_setup.R")

source('code/02_analysis/match_functions.R')

library(fixest)
library(broom)
theme_set(theme_bw())

# Project inputs --------------------------------------------------------------------------
# treatment to run matching on - dbcg, ddd, dap, nab
treatment_var <- 'dbcg'
treatment_year <- paste0(treatment_var, '_dx_yr')

# outcome to run matching on
outcome_var <- 'per_ind'

follow_up_yrs <- 10

model_dir <- paste0('data/analysis/01_matching/02_matched/',treatment_var,'/',outcome_var,'/2024-05-25/')

print(model_dir)

mdf <- get_match_time_series(match_dir = paste0(model_dir,'m10'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                             lookback = 1, deaths_as_0 = TRUE, agg = FALSE, prematch = TRUE)
mdf[!is.na(treat_yr), treat := 1]
mdf[, sbc := .GRP, by = c('subclass','match_yr')]
did_df <- mdf[,.(outcome = mean(outcome), n = .N), 
              by = c('match_yr','sbc','match_age','sex','treat','per_inc_qt','fam_inc_qt','familie_type',
                     'fam_size','edu','pre_socio','retired','year','index')]

did_es <- feols(outcome ~ i(index, treat, ref = -1) + treat*(1 + match_age + edu + fam_size + familie_type) | year, 
                data = did_df, weights = did_df$n)

smry <- summary(did_es)

smry_coefs <- tidy(smry) %>% setDT()
smry_coefs[, estimate := round(estimate, 2)][, std.error := round(std.error, 2)]
smry_coefs[, statistic := round(statistic, 3)][, p.value := round(p.value, 3)]

smry_perf <- glance(smry) %>% setDT() %>% round(3)

fwrite(smry_coefs,'data/paper1/did_coefficients.csv')
fwrite(smry_perf,'data/paper1/did_perf.csv')

coefplot(did_es)

ggplot(did_df) + 
  stat_summary(aes(x = index, y = outcome, color = exposure)) +
  facet_wrap(.~match_yr)

