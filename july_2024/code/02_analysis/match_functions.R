# Functions used to apply matching methods to data

# Match functions --------------------------------------------------------------------------

# flexible matching model which runs matching along age groups and year
run_matches <- function(match_type, distance_type = NA, eqn = NA, data, c_ratio = 5, name = Sys.time(), 
                        model_dir){
  stopifnot(length(unique(data$year)) == 1)
  form <- as.formula(eqn)
  
  mt <- matchit(form, method = match_type, distance = distance_type, ratio = c_ratio, data = data,
                exact = c('age_group','year','sex'), estimand = 'ATT')
  
  dt <- data.table(match.data(mt))
  dt[, match_yr := unique(data$year)]
  
  # write model data
  dt %>% 
    select(pid, subclass, year, treat, match_yr, match_age = age_group, sex,
           per_inc_qt, fam_inc_qt, familie_type, fam_size, edu, pre_socio, reg, cci,
           starts_with('dx_')) %>%
    distinct() %>%
    group_by(year) %>% 
    write_dataset(paste0(model_dir,'/',name))
}

deaths_fcn <- function(df, follow_up_length, lookback){
  # load('data/supplemental_inputs/life_expectancy.RData')
  
  # get deaths - pull people below LE and get the last year they're in the data
  # deaths <- df[year == yod-1,.(pid, treat, match_yr, alder, match_age, 
  #                                      sex, subclass, sbc, yod, aod, treat_yr, year)]
  deaths <- df[year == yod - 1]
  # deaths <- merge(deaths, life_expectancy, by.x = c('sex','alder','match_yr'), 
  #                 by.y = c('sex','alder','year'), all.x = TRUE)
  deaths[, indhold := 65]
  deaths <- deaths[aod < indhold]
  deaths[, alder := NULL][, year := NULL][, dead := NULL][, outcome := NULL]
  
  # create table of possible years to observe depending on year of death
  lost_years <- data.table(yod = unlist(lapply(2000:2018, function(x) rep(x,18))), 
                           year = 2000:2018, outcome = 0, dead = 1)
  lost_years <- lost_years[yod <= year & year - yod < follow_up_length]
  
  deaths <- merge(deaths, lost_years, by='yod', allow.cartesian = TRUE)
  
  # apply LE to hypothetical years - assume they only live to set LE (2015 LE)
  deaths <- deaths[year <= (indhold - aod) + yod]
  
  
  # fix index of death years
  deaths[, index := year - (match_yr + lookback)]
  deaths <- deaths[index < follow_up_length]
  
  # add counterfactual posthumous years to data
  df <- rbind(df, deaths, fill = TRUE)
  return(df)
}

get_match_time_series <- function(match_dir, treatment_var, treatment_year, outcome_var, follow_up_length = 10,
                                  lookback, deaths_as_0 = FALSE, agg = TRUE, prematch = FALSE){

  df <- open_dataset('data/analysis/01_matching/01_pre_match') %>%
    select(pid, year, outcome = all_of(outcome_var), treat_yr = all_of(treatment_year),
           alder, yod, aod) %>% collect() %>% setDT()
  
  model_num <- str_extract(match_dir,"m\\d+$")
  load(paste0(dirname(match_dir),'/params.RData'))
  match_vars <- all.vars(as.formula(model_matrix[index == model_num]$eq_set)) 
  match_vars <- setdiff(match_vars,c('age_group','sex','treat'))
  
  match_data_all <- lapply(Sys.glob(paste0(match_dir,'/*')), function(y_match_dir){
    print(y_match_dir)
    match_data <- open_dataset(y_match_dir) %>%
      select(pid, match_yr, subclass, match_age, sex, treat,
             per_inc_qt, fam_inc_qt, familie_type, fam_size, edu, pre_socio, reg, cci, starts_with('dx_')) %>%
      collect() %>% setDT()
    
    match_data[, sbc := .GRP, by = c('match_yr','subclass')]
    match_data[, retired := 0]
    match_data[str_detect(pre_socio,'retire'), retired := 1]
    
    # print('merging')
    match_data <- merge(match_data, df, by=c('pid'), all.x = TRUE, allow.cartesian = TRUE)
    
    # creating index - starts at 0 for dx year (year of matching + # years back matching happened before index)
    match_data[, index := year - (match_yr + lookback)]
    
    # restrict follow up length
    match_data <- match_data[index < follow_up_length]
    
    # restrict lookback
    match_data <- match_data[index >= -5]
    if(prematch == FALSE) match_data <- match_data[index >= 0]
    
    match_data[, dead := 0]
    
    if(deaths_as_0) match_data <- deaths_fcn(match_data, follow_up_length, lookback)
    
    # some people are matched as controls then get diagnosed later. Censoring years of observation
    # after diagnosis
    match_data <- match_data[treat == 1 | treat == 0 & is.na(treat_yr) | treat == 0 & year < treat_yr]
    
    match_data <- match_data[!is.na(outcome)]
    
    if(agg){
      match_data <- match_data[,.(outcome = mean(outcome, na.rm = TRUE), 
                                  s_var = var(outcome, na.rm = TRUE), 
                                  n = .N, n_dead = sum(dead)), 
                               by = c('treat','match_yr','match_age','sex','sbc','year','index',match_vars)]
      match_data[is.na(s_var) & n == 1, s_var := 0]
      
      match_data[, treat := as.character(treat)]
      match_data[treat == '0', treat := 'control']
      match_data[treat == '1', treat := 'treat']
      match_data <- dcast(match_data, ... ~ treat, 
                          value.var = c('outcome','s_var','n','n_dead'))
      match_data <- match_data[!is.na(outcome_treat)]
      
      match_data[, cate := outcome_control - outcome_treat]
      match_data[, cate_var := s_var_control/n_control + s_var_treat/n_treat]
    }
    return(match_data)
  }) %>% rbindlist()
  
  match_data_all[, sbc := .GRP, by = c('match_yr','sbc')]
  return(match_data_all)
}

# ATT function
calculate_att <- function(df, groupings = NULL){
  df <- df[!is.na(outcome_control)]
  df[, dummy := 1]
  df <- df[,.(att = weighted.mean(cate, w = n_treat), 
             se = sqrt(sum((n_treat/sum(n_treat))^2*cate_var)),
             n_treat = sum(n_treat), n_control = sum(n_control),
             n_dead_treat = sum(n_dead_treat), n_dead_control = sum(n_dead_control),
             outcome_control = weighted.mean(outcome_control, w = n_treat),
             outcome_treat = weighted.mean(outcome_treat, w = n_treat)),
           by = eval(unique(c(groupings, 'index','dummy')))]
  
  df <- df[,.(att = sum(att, na.rm = TRUE), se = sqrt(sum(se^2)),
              n_treat = max(n_treat), n_control = max(n_control),
              n_dead_treat = max(n_dead_treat), n_dead_control = max(n_dead_control),
              outcome_control = sum(outcome_control), outcome_treat = sum(outcome_treat)), 
           by = c(groupings,'dummy')]
  
  df[, lower := att - 1.96*se][, upper := att + 1.96*se]
  df[, p_val := 2*pt(-abs(att/se), df = n_control + n_treat - 1)]
  df[, pct := att/outcome_control]
  df[, dummy := NULL]
  df <- unique(df)
  return(df)
}
