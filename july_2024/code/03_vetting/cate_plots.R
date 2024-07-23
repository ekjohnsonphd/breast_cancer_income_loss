rm(list = ls())
gc()
source("code/environment_setup.R")
library(rpart)
library(rpart.plot)

source('code/02_analysis/match_functions.R')
theme_set(theme_bw())

# Project inputs --------------------------------------------------------------------------
# treatment to run matching on - dbcg, ddd, dap, nab
treatment_var <- 'dbcg'

# outcome to run matching on
outcome_var <- 'per_ind'

follow_up_yrs <- 10

model_dir <- paste0('data/analysis/01_matching/02_matched/',treatment_var,'/',outcome_var,'/2024-06-20/')
cate_dir <- paste0('data/analysis/01_matching/03_cate/',treatment_var,'/',outcome_var,'/2024-06-20/')

print(model_dir)

load(paste0(model_dir,'params.RData'))

mdf <- fread(paste0(cate_dir,'m25.csv'))
mdf$match_age <- factor(mdf$match_age, levels = c("18to29","30to34","35to39","40to45","45to49","50to54","55to59","60to64","65to69",
                                                  "70to74","75to79","80to84","85to89","90to120"), ordered = TRUE)
mdf$edu <- factor(mdf$edu, levels = c('Basic','Upper secondary','Vocational training','Bachelor','Higher education'), ordered = TRUE)
mdf$per_inc_qt <- factor(mdf$per_inc_qt, levels = c('q1','q2','q3','q4'), ordered = TRUE)
mdf$fam_inc_qt <- factor(mdf$fam_inc_qt, levels = c('q1','q2','q3','q4'), ordered = TRUE)


mdf[, retired := FALSE][, in_workforce := FALSE][, leave := FALSE]
mdf[str_detect(pre_socio,'retire'), retired := TRUE]
mdf[pre_socio %in% c('wage_earner','self_employed','unemployed_6mo','working_spouse'), in_workforce := TRUE]
mdf[pre_socio %in% c('leave_recipient','unemp_recipient','disab_recipient','early_retirement','retired'), leave := TRUE]

dx_cols <- str_subset(colnames(mdf), '^dx_')
mdf[, (dx_cols) := lapply(.SD, as.factor), .SDcols = dx_cols]


mdf_agg <- calculate_att(mdf, groupings = c('match_yr','match_age','fam_inc_qt','familie_type','fam_size','pre_socio','edu','sbc',
                                            'retired','in_workforce','leave',dx_cols,'reg'))


cate_plot <- function(mdf, var, yearly = FALSE, relative = FALSE){
  if(yearly){
    mdf_stratified <- calculate_att(mdf,c(var,'index'))
    mdf_stratified$strat <- mdf_stratified$index
    mdf_stratified$fct <- mdf_stratified[[var]]
  }else{
    mdf_stratified <- calculate_att(mdf,var)
    mdf_stratified$strat <- mdf_stratified[[var]]
  }
  
  if(relative){
   mdf_stratified[, att := pct][, lower := lower/outcome_control][, upper := upper/outcome_control]
  }

  g <- ggplot(mdf_stratified) + 
    geom_vline(aes(xintercept = 0)) +
    geom_pointrange(aes(x = att, xmin = lower, xmax = upper, y = strat, 
                        color = ifelse(p_val < 0.05, TRUE, FALSE))) +
    scale_color_manual(values = c('FALSE' = 'black','TRUE' = 'red'), drop = FALSE) +
    labs(x = 'ATT', y = var, color = 'Significant')
  
  if(relative){
    g <- g + scale_x_continuous(labels = scales::label_percent())
  }else{
    g <- g + scale_x_continuous(labels = scales::label_currency(suffix = NULL, prefix = '€', 
                                                                big.mark = ".",decimal.mark = ","))
  }
  
  
  if(yearly) g <- g + facet_wrap(.~fct) + coord_flip() + labs(subtitle = var, y = 'Index')
  return(g)
}


cate_plot(mdf,'pre_socio') + labs(y = 'Labor market attachment')
cate_plot(mdf,'pre_socio', relative = TRUE) + labs(y = 'Labor market attachment')

cate_plot(mdf[!str_detect(pre_socio,'retire') & !is.na(edu)],'edu')
cate_plot(mdf[!str_detect(pre_socio,'retire') & !is.na(edu)],'edu')
cate_plot(mdf[!str_detect(pre_socio,'retire')],'per_inc_qt')
cate_plot(mdf[!str_detect(pre_socio,'retire')],'per_inc_qt', relative = TRUE)
cate_plot(mdf[!str_detect(pre_socio,'retire')],'familie_type')
cate_plot(mdf[!str_detect(pre_socio,'retire')],'fam_size')
cate_plot(mdf[!str_detect(pre_socio,'retire')],'reg')
cate_plot(mdf[!str_detect(pre_socio,'retire')],'reg', relative = TRUE)
cate_plot(mdf[!str_detect(pre_socio,'retire')],'dx_cvasc')
cate_plot(mdf,'match_age')
cate_plot(mdf,'match_age', relative = TRUE)
cate_plot(mdf,'year',yearly = TRUE) + coord_flip()
cate_plot(mdf,'pre_socio', yearly = TRUE)
cate_plot(mdf,'pre_socio', yearly = TRUE, relative = TRUE)







dtree <- rpart(att ~ match_yr + match_age + edu + fam_inc_qt + fam_size + familie_type + pre_socio + dx_cvasc + dx_copd + dx_demen +
                 dx_diabc + dx_diabuc + dx_hfail + dx_hemi + dx_mliver + dx_mi + dx_pud + dx_renal + dx_rheum + dx_sliver + reg, 
               data = mdf_agg, weights = n_treat, control = rpart.control(cp = 0.001),
               method = 'anova')
rpart.plot(dtree, tweak = 0.8)

mdf_idx <- calculate_att(mdf,c('pre_socio','index'))

g6 <- ggplot(mdf_idx) + 
  geom_line(aes(y = n_dead_treat/n_treat, x = index, color = 'Exposed')) + 
  geom_line(aes(y = n_dead_control/n_control, x = index, color = 'Unexposed')) +
  geom_point(aes(y = n_dead_treat/n_treat, x = index, color = 'Exposed')) + 
  geom_point(aes(y = n_dead_control/n_control, x = index, color = 'Unexposed')) +
  scale_color_manual(values = c('red','black'), drop = FALSE) + 
  facet_wrap(.~pre_socio) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = 'Years since index', y = 'Mortality rate (% of total)', color = 'Population',
       title = 'Mortality rates in matched groups')


pdf('data/analysis/99_vetting/may17_cate/att_plots_faminc.pdf', onefile = TRUE)
g1
g2
g3
g4
g5
# g6
dev.off()
