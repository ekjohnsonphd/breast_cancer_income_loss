rm(list = ls())
gc()
source("code/environment_setup.R")
source('code/02_analysis/match_functions.R')

# Project inputs --------------------------------------------------------------------------
# treatment to run matching on - dbcg, ddd, dap, nab
treatment_var <- 'dbcg'

# outcome to run matching on
outcome_var <- 'fam_ind'

model_dir <- paste0('data/analysis/01_matching/02_matched/',treatment_var,'/',
                    outcome_var,'/',Sys.Date(),'/')
# model_dir <- paste0('data/analysis/01_matching/02_matched/',treatment_var,'/',outcome_var,'/2024-05-25/')

cate_dir <- paste0('data/analysis/01_matching/03_cate/',treatment_var,'/',
                   outcome_var,'/',Sys.Date(),'/')

# cate_dir <- paste0('data/analysis/01_matching/03_cate/',treatment_var,'/',
#                    outcome_var,'/2024-05-25/')
dir.create(cate_dir, recursive = TRUE, showWarnings = FALSE)

print(model_dir)

load(paste0(model_dir,'params.RData'))

follow_up_yrs <- 10
if(outcome_var == 'per_ind'){
  deaths <- TRUE
}else{
  deaths <- FALSE
}

# Calculate time series ATT --------------------------------------------------------------------------

cl <- makeCluster(10)
clusterExport(cl, c('model_matrix','model_dir','cate_dir','treatment_var','treatment_year','outcome_var','deaths','follow_up_yrs'))
clusterEvalQ(cl, {
  require(MatchIt)
  source('code/environment_setup.R')
  source('code/02_analysis/match_functions.R')
})

model_comparisons <- parLapplyLB(cl, 1:nrow(model_matrix), function(i){
  m <- model_matrix[i]
  
  mdf <- get_match_time_series(paste0(model_dir,m$index), treatment_var, treatment_year, outcome_var, follow_up_length = follow_up_yrs, 
                               lookback = m$lookbacks, deaths_as_0 = deaths, agg = TRUE, prematch = FALSE)
  fwrite(mdf,paste0(cate_dir,m$index,'.csv'))
  att <- calculate_att(mdf)
  gc()
  return(cbind(m,att))
}) %>% rbindlist()

stopCluster(cl)

model_comparisons[method %in% c('cem','exact'), dist := NA]
fwrite(model_comparisons, paste0(model_dir,'/model_comparisons_',follow_up_yrs,'y.csv'))
