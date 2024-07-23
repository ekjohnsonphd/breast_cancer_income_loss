rm(list = ls())
gc()
source("code/environment_setup.R")
source('code/02_analysis/match_functions.R')
library(yardstick)
library(GGally)
library(vtable)

df <- open_dataset('data/analysis/01_matching/01_pre_match') %>%
  collect() %>% setDT()

df$edu <- factor(df$edu, levels = c('Basic','Upper secondary','Vocational training','Bachelor','Higher education'), ordered = TRUE)
df$pre_socio <- factor(df$pre_socio, levels = c('wage_earner','retired','in_school','disab_recipient','unemp_recipient',
                                                'early_retirement','self_employed','leave_recipient','unemployed_6mo',
                                                'working_spouse','other','unknown'), ordered = TRUE)

desc_cols <- c('alder','per_ind','fam_ind','familie_type','fam_size','pre_socio','edu')

sumdf <- copy(df[year == 2015 & sex == 'female'])
sumdf[, grp := 'Overall']
sumdf <- rbind(sumdf,
               copy(sumdf[dbcg == 1 & dbcg_dx_yr == 2016])[,grp := 'dbcg'])

st(sumdf, vars = desc_cols, summ = c('mean(x)','median(x)','sd(x)'), 
   group = 'grp', out = 'csv', 
   file = 'data/paper1/table1.csv')
