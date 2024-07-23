rm(list = ls())
gc()
source("code/environment_setup.R")
library(ggalluvial)
library(ggsci)
theme_set(theme_ej())
# pull matching data ---------------------------------------------
treatment_var <- 'dbcg'
outcome_var <- 'per_ind'
model <- 'm25'

plot_year <- 2005

match_data <- open_dataset(paste0('data/analysis/01_matching/02_matched/',treatment_var,'/',outcome_var,'/2024-06-20/',model,'/')) %>%
  filter(match_yr == plot_year) %>%
  select(pid, subclass, match_yr, match_age, treat, pre_socio) %>%
  collect() %>% setDT()
match_data[, sbc := .GRP, by = c('match_yr','subclass')]
match_data[, pre_socio := NULL][, subclass := NULL]

df <- open_dataset('data/analysis/01_matching/01_pre_match') %>%
  filter(year >= plot_year) %>%
  collect() %>% setDT()

df <- merge(match_data, df, by = 'pid', all.x = TRUE, allow.cartesian = TRUE)
df <- df[year %in% c(match_yr, match_yr + 3, match_yr + 6, match_yr + 11)]
df[, years_since_dx := year - match_yr - 1]
dft <- expand(df, nesting(pid, yod, match_yr), years_since_dx)
df <- merge(df, dft, by = c('pid', 'years_since_dx','yod','match_yr'), all = TRUE)
df[, cci := as.factor(cci)]

skey <- function(dt,varb){
  dt <- dcast(dt, pid + yod + match_yr ~ years_since_dx, value.var = varb)
  setnames(dt,c('-1','2','5','10'),c('y-1','y2','y5','y10'))
  dt[yod <= match_yr + 3, y2 := 'died']
  dt[yod <= match_yr + 6, y5 := 'died']
  dt[yod <= match_yr + 11, y10 := 'died']
  
  dt <- dt[, .(count = .N), by = c('y-1','y2','y5','y10')]
  dt[, index := .I]
  dt <- melt(dt, id.vars = c('count','index'), measure.vars = patterns('^y'), variable.name = 'year')
  dt[, year := factor(str_sub(year, 2, -1), levels =  c('-1','2','5','10'))]
  
  g <- ggplot(dt, aes(x = year, y = count, stratum = value, 
                      fill = value, alluvium = index)) + 
    geom_stratum(alpha = 0.5) +
    geom_flow() + 
    scale_fill_d3(palette = 'category20') +
    labs(y = 'Count', x = 'Years since index')
  
  return(g)
}

skey(df[treat == 1],'pre_socio')
skey(df[treat == 1 & match_age == '35to39'],'fam_size')
skey(df[treat == 0 & match_age == '35to39'],'fam_size')
skey(df[treat == 1 & match_age %in% c('65to69','70to74','75to79','80to84','85to89','90to120')],'familie_type')
skey(df[treat == 0 & match_age %in% c('65to69','70to74','75to79','80to84','85to89','90to120')],'familie_type')

skey(df[treat == 1],'cci')
skey(df[treat == 1],'per_inc_qt')


