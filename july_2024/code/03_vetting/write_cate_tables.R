rm(list = ls())
gc()
source("code/environment_setup.R")
source("code/02_analysis/match_functions.R")

# cate_dir <- paste0('data/analysis/01_matching/03_cate/',treatment_var,'/',
#                    outcome_var,'/',Sys.Date(),'/')
cate_dir <- paste0(
  "data/analysis/01_matching/03_cate/", treatment_var, "/",
  outcome_var, "/2024-06-20/"
)

get_cate <- function(file, var_list, model, income) {
  cate_df <- fread(file)

  if (!("retired" %in% colnames(cate_df)) & "pre_socio" %in% colnames(cate_df)) {
    cate_df[, retired := 0]
    cate_df[str_detect(pre_socio, "retire"), retired := 1]
  }

  cate <- lapply(var_list, function(st) {
    ct1 <- calculate_att(cate_df, unique(c(st, "retired")))
    ct2 <- calculate_att(cate_df, st)
    ct <- rbind(ct1, ct2, fill = TRUE)
    ct[, stratifier := st]
    setnames(ct, st, "stratification")
    return(ct)
  }) %>% rbindlist()

  cate <- rbind(calculate_att(cate_df), cate, fill = TRUE)
  cate <- rbind(calculate_att(cate_df, "retired"), cate, fill = TRUE)
  cate[, model := model][, income := income]
  return(cate)
}

c1 <- get_cate(
  paste0(cate_dir, "/m22.csv"),
  c("index", "pre_socio", "edu", "fam_size", "familie_type", "per_inc_qt", "match_age", "year", "reg", "cci"),
  "final", "personal"
)
c2 <- get_cate(
  paste0(cate_dir, "m10.csv"),
  c("index", "pre_socio", "edu", "fam_size", "familie_type", "fam_inc_qt", "match_age", "year"),
  "final", "household"
)
c3 <- get_cate(
  paste0(cate_dir, "m1.csv"),
  c("index", "year", "match_age"), "age_sex", "personal"
)
c4 <- get_cate(
  paste0(cate_dir, "m8.csv"),
  c("index", "year", "match_age", "edu"), "age_sex_edu", "personal"
)
c5 <- get_cate(
  paste0(cate_dir, "m14.csv"),
  c("index", "year", "match_age"), "3y_lookback", "personal"
)
c6 <- get_cate(
  paste0(cate_dir, "m15.csv"),
  c("index", "year", "match_age"), "5y_lookback", "personal"
)

# cfact_data <- get_match_time_series('data/analysis/01_matching/02_matched/dbcg/per_ind/2024-05-25/m13',
#                                     'dbcg','dbcg_dx_yr','per_ind',10,1,deaths_as_0 = FALSE,agg = TRUE,prematch = FALSE)
# fwrite(cfact_data,'data/analysis/01_matching/03_cate/dbcg/per_ind/2024-05-25/m00.csv')

c7 <- get_cate(
  paste0(cate_dir, "m00.csv"),
  c("index", "pre_socio", "edu", "fam_size", "familie_type", "per_inc_qt", "match_age", "year"),
  "deaths_cfactual", "personal"
)

# combine
cate_df <- rbind(c1, c2, c3, c4, c5, c6, c7)
cate_df <- cate_df %>% mutate(across(where(is.numeric), \(x) round(x, 3)))
setcolorder(cate_df, c("income", "model", "retired", "stratifier", "stratification", "att", "lower", "upper"))
cate_df[, `:=`(
  n_treat = as.character(n_treat), n_control = as.character(n_control),
  n_dead_treat = as.character(n_dead_treat), n_dead_control = as.character(n_dead_control)
)]
cate_df[n_treat %in% c("1", "2", "3", "4", "5"), n_treat := "<5"]
cate_df[n_control %in% c("1", "2", "3", "4", "5"), n_control := "<5"]
cate_df[n_dead_treat %in% c("1", "2", "3", "4", "5"), n_dead_treat := "<5"]
cate_df[n_dead_control %in% c("1", "2", "3", "4", "5"), n_dead_control := "<5"]

fwrite(cate_df, "data/paper1/heterogeneous_effects.csv")
