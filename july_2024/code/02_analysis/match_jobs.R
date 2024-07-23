rm(list = ls())
gc()
source("E:/ProjektDB/OPEN/Workdata/xxx/Emily/code/environment_setup.R")
library(MatchIt)
# library(marginaleffects)

source('code/02_analysis/match_functions.R')

# Project inputs --------------------------------------------------------------------------
# treatment to run matching on - dbcg, ddd, dap, nab
treatment_var <- 'dbcg'
treatment_year <- paste0(treatment_var, '_dx_yr')
treatment_match_var <- paste0(treatment_var,'_match_df')

# outcome to run matching on
outcome_var <- 'fam_ind'

model_dir <- paste0('data/analysis/01_matching/02_matched/',treatment_var,'/',outcome_var,'/',Sys.Date(),'/')
# model_dir <- paste0('data/analysis/01_matching/02_matched/',treatment_var,'/',outcome_var,'/2024-05-16/')

year_set <- data.table(cause = c('dbcg','ddd','dap','nab'),
                       st_yr = c(2000,2013,2008,2011),
                       ed_yr = 2014)

# Create model grid --------------------------------------------------------------------------
lookbacks <- c(1,3,5)
dx_vars <- c('dx_cvasc','dx_copd','dx_demen','dx_diabc','dx_diabuc','dx_hfail','dx_hemi',
            'dx_mliver','dx_mi','dx_pud','dx_pvd','dx_renal','dx_rheum','dx_sliver')

eq_set <- c(paste0('treat ~ sex + age_group + ',outcome_var,' + familie_type + pre_socio + fam_size + edu + reg + ',paste0(dx_vars, collapse = ' + ')),
            paste0('treat ~ sex + age_group + ',outcome_var,' + familie_type + pre_socio + fam_size + edu + ',paste0(dx_vars, collapse = ' + ')),
            paste0('treat ~ sex + age_group + ',outcome_var,' + familie_type + pre_socio + fam_size + edu + reg + cci'),
            paste0('treat ~ sex + age_group + ',outcome_var,' + familie_type + pre_socio + fam_size + edu + cci'),
            paste0('treat ~ sex + age_group + ',outcome_var,' + familie_type + pre_socio + fam_size + edu + reg'),
            paste0('treat ~ sex + age_group + ',outcome_var,' + familie_type + pre_socio + fam_size + edu'),
            'treat ~ sex + age_group + familie_type + pre_socio + fam_size + edu + reg + cci',
            'treat ~ sex + age_group + edu + as.factor(retired)',
            'treat ~ sex + age_group + as.factor(retired)')

model_matrix <- data.frame(method = c('exact'),
                           dist = c(NA))

model_matrix <- expand(model_matrix, nesting(method, dist), eq_set, lookbacks)
setDT(model_matrix)

model_matrix[method %in% c('exact','cem'), eq_set := str_replace(eq_set, 'fam_ind', 'fam_inc_qt')]
model_matrix[method %in% c('exact','cem'), eq_set := str_replace(eq_set, 'per_ind', 'per_inc_qt')]
model_matrix[method %in% c('exact','cem'), dist := NA]

model_matrix <- unique(model_matrix)
model_matrix[, index := paste0('m',.I)]
model_matrix <- model_matrix[method != 'optimal']

# model_matrix <- model_matrix[1:10]
dir.create(model_dir, showWarnings = FALSE)

save(treatment_var, treatment_year, treatment_match_var, outcome_var, model_matrix,
     file = paste0(model_dir,'params.RData'))

years <- year_set[cause == treatment_var]$st_yr:year_set[cause == treatment_var]$ed_yr
cl <- makeCluster(length(years))
clusterExport(cl, c('model_matrix','model_dir','treatment_var','treatment_year','treatment_match_var',
                    'outcome_var'))
clusterEvalQ(cl, {
  require(MatchIt)
  source('code/environment_setup.R')
  source('code/02_analysis/match_functions.R')
})

clusterApply(cl, years, function(y){
  year_df <- open_dataset('data/analysis/01_matching/01_pre_match') %>%
    filter(year == y) %>%
    collect() %>%
    setDT()

  year_df$treat <- year_df[[treatment_var]]
  year_df$treat_yr <- year_df[[treatment_year]]
  year_df$outcome <- year_df[[outcome_var]]
  
  year_df[is.na(treat), treat := 0]

  lapply(1:nrow(model_matrix), function(i){
    m <- model_matrix[i]

    if(y + m$lookbacks <= 2015){
      print(m)
      ydf <- copy(year_df)[year_df[[treatment_var]] == 0 | 
                             year_df[[treatment_year]] >= year + m$lookback]
      ydf$treat <- ydf[[treatment_var]]
      ydf[treat_yr > year + m$lookback, treat := 0]
      
      run_matches(match_type = m$method, distance_type = m$dist, eqn = m$eq_set,
                  data = ydf, name = m$index, model_dir = model_dir, c_ratio = 3)
    }

    
    gc()
  })
})
                 
stopCluster(cl)

