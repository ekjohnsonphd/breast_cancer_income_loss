rm(list = ls())
gc()
source("code/environment_setup.R")
source("code/02_analysis/match_functions.R")
library(MatchIt)

treatment_var <- 'dbcg'
treatment_year <- 'dbcg_dx_yr'
outcome <- 'per'
outcome_var <- 'per_ind'
dx_y <- 2008

model_dir <- paste0('data/analysis/01_matching/02_matched/',treatment_var,'/',outcome_var,'/2024-05-25/')

df <- open_dataset('data/analysis/01_matching/01_pre_match') %>%
  collect() %>% setDT()

parallel_trends_data <- function(treatment, outcome, dx_y, df, deaths = FALSE){
  setnames(df, c(treatment, paste0(treatment,'_dx_yr'), paste0(outcome,'_ind'), paste0(outcome,'_inc_qt')), 
           c('treat','treat_yr','outcome','outcome_qt'))
  
  pid_prior <- unique(df[year == dx_y - 1]$pid)
  df <- df[pid %in% pid_prior & (is.na(treat_yr) | treat_yr == dx_y)]
  if(treatment == 'dbcg') df <- df[sex == 'female']
  
  m1 <- matchit(treat ~ age_group + sex + year, method = 'exact', data = df[year == dx_y - 1])
  m1 <- data.table(match.data(m1))
  m1 <- merge(df, m1[,.(pid, weights)], by = 'pid')
  
  m2 <- matchit(treat ~ age_group + sex + year + pre_socio + familie_type + fam_size + edu, 
                method = 'exact', data = df[year == dx_y - 1])
  m2 <- data.table(match.data(m2))
  m2 <- merge(df, m2[,.(pid, weights)], by = 'pid')

  m3 <- matchit(treat ~ age_group + sex + year + pre_socio + familie_type + fam_size + edu + outcome_qt, 
                method = 'exact', data = df[year == dx_y - 1])
  m3 <- data.table(match.data(m3))
  m3 <- merge(df, m3[,.(pid, weights)], by = 'pid')
  
  if(deaths){
    m1 <- deaths_fcn(m1, 10)
    m2 <- deaths_fcn(m2, 10)
    m3 <- deaths_fcn(m3, 10)
  }
  
  sdf <- rbind(df[,.(outcome = mean(outcome), mod = 'Overall'), by = c('year','treat')],
               m1[treat == 0,.(outcome = weighted.mean(outcome, weights), mod = 'Age + sex'), 
                  by = c('year','treat')],
               m2[treat == 0,.(outcome = weighted.mean(outcome, weights), mod = 'Age + sex + covs'), 
                  by = c('year','treat')],
               m3[treat == 0,.(outcome = weighted.mean(outcome, weights), mod = 'Age + sex + covs + inc'), 
                  by = c('year','treat')])
  sdf[treat == 1, mod := 'Exposed']
  sdf$mod <- factor(sdf$mod, levels = c('Exposed','Overall','Age + sex','Age + sex + covs',
                                        'Age + sex + covs + inc'))
  sdf[, index_yr := dx_y]
  return(sdf)
}

sdf <- lapply(2000:2015, function(x){
  dt <- parallel_trends_data(treatment = 'dbcg', outcome = 'per', dx_y = x, df = copy(df), deaths = TRUE)
  return(dt)
}) %>% rbindlist()


m1 <- get_match_time_series(match_dir = paste0(model_dir,'m10'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                             lookback = 1, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)
m2 <- get_match_time_series(match_dir = paste0(model_dir,'m11'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                            lookback = 3, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)
m3 <- get_match_time_series(match_dir = paste0(model_dir,'m12'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                            lookback = 5, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)
m4 <- get_match_time_series(match_dir = paste0(model_dir,'m13'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                            lookback = 1, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)
m5 <- get_match_time_series(match_dir = paste0(model_dir,'m14'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                            lookback = 3, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)
m6 <- get_match_time_series(match_dir = paste0(model_dir,'m15'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                            lookback = 5, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)
m7 <- get_match_time_series(match_dir = paste0(model_dir,'m1'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                            lookback = 1, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)
m8 <- get_match_time_series(match_dir = paste0(model_dir,'m2'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                            lookback = 3, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)
m9 <- get_match_time_series(match_dir = paste0(model_dir,'m3'), treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                            lookback = 5, deaths_as_0 = TRUE, agg = TRUE, prematch = TRUE)


dt <- rbind(m1[, model := 'm1'], m2[, model := 'm2'], m3[, model := 'm3'], m4[, model := 'm4'], m5[, model := 'm5'], 
            m6[, model := 'm6'], m7[, model := 'm7'], m8[, model := 'm8'], m9[, model := 'm9'], fill = TRUE)

dtc <- calculate_att(dt,c('model','match_yr','year','index'))

ggplot(dtc[match_yr == 2005]) + 
  geom_vline(aes(xintercept = 2005)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(x = year, y = att, color = model))


ggplot(sdf) +
  geom_vline(aes(xintercept = index_yr)) +
  geom_line(aes(x = year, y = outcome, color = mod), linewidth = 0.8) +
  facet_wrap(.~index_yr) +
  scale_color_manual(values = c('#ff7f00','#a6cee3','#1f78b4','#08519c','#08306b')) +
  theme_bw() +
  labs(x = 'Year', y = 'Income (2023 dkk)', color = NULL)

ggplot(sdf[index_yr == 2004]) +
  geom_vline(aes(xintercept = index_yr)) +
  geom_line(aes(x = year, y = outcome, color = mod), linewidth = 0.8) +
  scale_color_manual(values = c('#ff7f00','#a6cee3','#1f78b4','#08519c','#08306b')) +
  theme_bw() +
  labs(x = 'Year', y = 'Income (2023 dkk)', color = NULL)

pdf('data/analysis/99_vetting/apr24_parallel_trends_and_descriptives/parallel_trends_per_mort.pdf', onefile = TRUE)
ggplot(sdf) +
  geom_vline(aes(xintercept = index_yr)) +
  geom_line(aes(x = year, y = outcome, color = mod), linewidth = 0.8) +
  facet_wrap(.~index_yr) +
  scale_color_manual(values = c('#ff7f00','#a6cee3','#1f78b4','#08519c','#08306b')) +
  theme_bw() +
  labs(x = 'Year', y = 'Income (2023 dkk)', color = NULL)

lapply(unique(sdf$index_yr), function(y){
  ggplot(sdf[index_yr == y]) +
    geom_vline(aes(xintercept = index_yr)) +
    geom_line(aes(x = year, y = outcome, color = mod), linewidth = 0.8) +
    scale_color_manual(values = c('#ff7f00','#a6cee3','#1f78b4','#08519c','#08306b')) +
    theme_bw() +
    labs(x = 'Year', y = 'Income (2023 dkk)', color = NULL, title = y)
})

dev.off()
