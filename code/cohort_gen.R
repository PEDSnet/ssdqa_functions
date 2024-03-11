
#' Reusable FOT function
#'
#' @param cohort a cohort tbl with a column for `site`, `person_id`, `start_date` and `end_date`
#' @param check_func the base function for the check that needs to be executed across time; this argument
#'                   should be structured as the following, where dat is the input data for the function:
#'                   
#'                   function(dat){check_function(param1 = dat, param2 = param2_input, ..., 
#'                   paramX = paramX_input)}
#'                   
#'                   all parameters for the base check function should be included if any defaults are not being
#'                   used
#' @param reduce_id if the check provided in @check_func returns a list of tables, this is
#'                  the ID that should be used to reduce the tables into one dataframe
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#' @param time_span when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as c(start date, end date) in yyyy-mm-dd date format
#' @param site_list A list of sites for which you would like to examine clinical facts. Can be one site 
#'                  (single-site) or multiple (multi-site) 
#'
#' @return one dataframe where the output of @check_func has been executed for each @time_period in 
#'         the provided @time_span for each of the sites included in @site_list
#' 
compute_fot <- function(cohort,
                        check_func,
                        site_col,
                        reduce_id = 'visit_type',
                        time_period='year',
                        time_span= c('2012-01-01','2022-12-31'),
                        site_list=list('stanford',
                                       'colorado',
                                       'chop')
) {
  
  site_list_v <- unlist(site_list)
  
  final_results <- list()
  
  t1 <- seq(from=ymd(time_span[[1]]),to=ymd(time_span[[2]]),by=time_period)
  t2 <- ceiling_date(t1, time_period) - 1
  
  
  # narrows the visit time to cohort_entry and end date
  for(k in 1:length(t1)) {
    
    message(paste0('Starting ',t1[[k]]))
    
    target <- ymd(t1[[k]])
    
    baseline_start_date <- target
    baseline_end_date <- ceiling_date(target, time_period) - 1
    
    cohort_narrowed <- cohort %>% 
      mutate(start_date = as_date(baseline_start_date),
             end_date = as_date(baseline_end_date))
    
    
    cohort_narrow_prepped <- cohort_narrowed %>%
      filter(!! sym(site_col) %in% site_list_v) %>% 
      mutate(time_start=start_date,
             time_increment=time_period)
    
    output <- check_func(dat = cohort_narrow_prepped)
    
    if(is.list(output)&!any(class(output)=='tbl_sql')){
      output_reduced <- dplyr::bind_rows(output, .id= reduce_id)
    }else{output_reduced <- output}
    
    final_results[[k]] <- output_reduced
    
  }
  
  rslt = reduce(.x=final_results,
                .f=dplyr::union)
  
  return(rslt)
  
}
  
#' compute age at cohort entry
#' 
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param person_tbl the CDM person table
#' @param age_groups a csv file (template found in specs folder) where the user defines the minimum and maximum
#'                   age allowed for a group and provides a string name for the group
#' 
#' @return `cohort_tbl` with the age at cohort entry and age group for each patient
#' 
#' 

compute_age_groups <- function(cohort_tbl,
                               person_tbl,
                               age_groups) {
  
  cohorts <- cohort_tbl %>% 
    inner_join(select(person_tbl,
                      person_id,
                      birth_date)) %>% 
    mutate(age_ce = floor((start_date - birth_date)/365.25)) %>%
    collect_new()
  
  cohorts_grpd <- cohorts %>%
    cross_join(age_groups) %>%
    mutate(age_grp = case_when(age_ce >= min_age & age_ce <= max_age ~ group,
                               TRUE ~ as.character(NA))) %>%
    filter(!is.na(age_grp)) %>%
    right_join(cohorts) %>%
    select(-c(birth_date, min_age, max_age, group)) %>%
    mutate(age_grp = case_when(is.na(age_grp) ~ 'No Group',
                               TRUE ~ age_grp))
  
  copy_to_new(df = cohorts_grpd)
  
}

#' intake codeset to customize patient labels
#'
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param codeset_meta a CSV file with metadata relating to a codeset with customized group labels
#'                     
#'                     this file should have `table`, `column`, and `file_name` columns

cohort_codeset_label <- function(cohort_tbl,
                                 codeset_meta){
  
  codeset <- load_codeset(codeset_meta$file_name)
  
  filter_tbl <- select(cdm_tbl('visit_occurrence'), person_id, visit_occurrence_id, provider_id, care_site_id) %>%
    left_join(cdm_tbl(codeset_meta$table)) %>%
    rename('concept_id' = codeset_meta$column) %>%
    inner_join(codeset, by = 'concept_id') %>%
    select(person_id, flag) %>%
    distinct() %>%
    right_join(cohort_tbl, by = 'person_id') %>%
    mutate(flag = case_when(is.na(flag) ~ 'None',
                            TRUE ~ flag))
  
  
}  
  
  
#' Prepare cohort for check execution
#' requirement: fields must have columns: 
#' `person_id`, `start_date`, `end_date`
#' 
#' @param cohort_tbl table with required fields for each member of the cohort
#' @param age_groups option to read in a CSV with age group designations to allow for stratification
#'                   by age group in output. defaults to `NULL`. 
#'                   sample CSV can be found in `specs/age_group_definitions.csv`
#' @param codeset option to read in a CSV with codeset metadata to allow for labelling of 
#'                cohort members based on a user-provided codeset. the codeset itself should be
#'                a CSV file with at least a `concept_id` column and a `flag` column with user-provided
#'                labels.
#'                a sample metadata CSV, where the user can provide the correct table and column information,
#'                can be found in `specs/codeset_metadata.csv`
#' 
#' 
#' @return a tbl with person_id and the following:
#'          `start_date` the cohort entry date
#'          `end_date` the last visit
#'          `fu`: length of follow up
#'          `site` : patient site
#'        if age_groups is not NULL: 
#'          `age_ce`: patient age at cohort entry
#'          `age_grp`: user-provided age grouping
#'        if codeset is not NULL:
#'          `flag`: flag that indiciates patient is a member of a user-specified group in the codeset
#' 

prepare_cohort <- function(cohort_tbl,
                           age_groups = NULL,
                           codeset = NULL) {
  
  ct <- cohort_tbl
  
  stnd <- 
    ct %>% 
    mutate(fu = round((end_date - start_date + 1)/365.25,3)) #%>% 
    #select(site, person_id, start_date, end_date, fu) #%>% 
    #add_site()
  
  if(!is.data.frame(age_groups)){
    final_age <- stnd
  }else{
    final_age <- compute_age_groups(cohort_tbl = stnd,
                                    person_tbl = cdm_tbl('person'),
                                    age_groups = age_groups)}
  
  if(!is.data.frame(codeset)){
    final_cdst <- stnd
  }else{
    final_cdst <- cohort_codeset_label(cohort_tbl = stnd,
                                       codeset_meta = codeset) %>%
      add_site()}
  
  final <- stnd %>%
    left_join(final_age) %>%
    left_join(final_cdst)
  
  return(final)
  
}
  
  
  
## Function to check correct inputs for multi/single site argument
  
#' Check site type (single vs multi) against number of sites provided in list
#'
#' @param cohort a cohort tbl with a column for `site`, `person_id`, `start_date` and `end_date`
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#'                               - @single - run the function for a single site
#'                               - @multi - run the function for multiple sites
#' @param site_list A list of sites for which you would like to examine clinical facts. Can be one site 
#'                  (single-site) or multiple (multi-site) 
#'
#' @return if @multi_or_single_site = single but multiple sites are provided, the cohort table
#'         is returned with a summary site column equaling `combined` so all sites will be treated
#'         as one
#'         
#'         otherwise, the existing site column is left alone. if an illogical parameter combination
#'         is supplied, the function will provide an error with recommendations on how to remedy the
#'         issue.
#' 
check_site_type <- function(cohort,
                            #site_list,
                            multi_or_single_site){
  
  if('site' %in% colnames(cohort)){
    
    # count number of sites in site list that also exist in the cohort
    n_site <- cohort %>% select(site) %>% 
      summarise(n_distinct(site)) %>% pull()
      #filter(site %in% site_list) %>% 
      #summarise(n_distinct(site)) %>% pull()
    
    if(multi_or_single_site == 'single' && n_site > 1){
    # create new "summary" site column / name, add that to grouped list
    # instead of site, and create new site list to account for new site name
      cohort_final <- cohort %>%
        #filter(site %in% site_list) %>%
        mutate(site_summ = 'combined')
      
      grouped_list <- c('site_summ')
      site_list_adj <- 'combined'
      
    }else if(multi_or_single_site == 'multi' && n_site == 1){
      
      stop('Please include data from multiple sites in your cohort to 
           conduct a multi-site analysis.')
      
    }else if((multi_or_single_site == 'single' && n_site == 1) ||
             (multi_or_single_site == 'multi' && n_site > 1)){
      
      cohort_final <- cohort #%>%
        #filter(site %in% site_list)
      
      site_list <- cohort %>% 
        select(site) %>% distinct() %>% pull()
      
      grouped_list <- c('site')
      site_list_adj <- site_list
      
    }else{stop('Invalid argument for multi_or_single_site. Please select either `single` or `multi`')}
  }else{stop('Please include a `site` column in your cohort.')}
  
  final <- list('cohort' = cohort_final, 
                'grouped_list' = grouped_list, 
                'site_list_adj' = site_list_adj)
  
  return(final)
}

#' check for `site_summ` column and switch to `site`
#' 
#' @param tbl the tbl with to use for 
#' replacement of `site_summ`
#' 
#' 
#' @return tbl with `site` replacing `site_summ`
#' 

replace_site_col <- function(tbl) {
  
  site_summ_exist <- 'site_summ' %in% colnames(tbl)
  site_exist <- 'site' %in% colnames(tbl)
  if(site_summ_exist & ! site_exist) 
    {final_tbl_site <- 
        tbl %>% ungroup() %>% rename(site = site_summ)} 
  else if(site_summ_exist & site_exist)
    {final_tbl_site <- 
        tbl %>% ungroup() %>% select(-site_summ)} 
  else {final_tbl_site <- tbl}
  
}

#' Join to vocabulary table
#'
#' @param tbl data table to which the vocabulary table should be joined
#' @param vocab_tbl location of the vocabulary table
#' @param col the column that should be used in the `by` statement to join
#'            to the concept_id column in the vocabulary table
#'
#' @return the dataframe provided in @tbl with the addition of the concept name
#'         column
#' 
join_to_vocabulary <- function(tbl,
                               vocab_tbl,
                               col){
  if(!is.null(vocab_tbl)){
  
  final <- select(vocab_tbl, concept_id, concept_name) %>%
    rename('join_col' = concept_id) %>%
    right_join(tbl %>% rename('join_col' = col), by = c('join_col'),
               copy = TRUE) %>%
    rename_with(~col, join_col) %>%
    collect()
  }else{
    final <- tbl %>% mutate(concept_name = 'No vocabulary table input')
  }
}


#' Generate parameter summary and recommended string to input into output function
#'
#' @param check_string abbreviation to represent check type, should be the same as what
#'                     is prefixed to the names of the output functions
#' @param ... all of the parameters input into the core function. any argument that is not
#'            able to be vectorized (i.e. a CDM tbl, codeset, etc) will not appear in the final
#'            summary
#'
#' @return paramater_summary.csv to the results directory
#' @return output_type string to be piped into a descriptive message at the end of the core function
#'         to inform users what should be used as the `output_function` argument in the output
#'         function
#' 
param_csv_summ2 <- function(check_string, ...){
  
  argg <- c(...)
  
  
  df <- stack(argg) %>%
    rename('param' = ind,
           'value' = values)
  
  site_type <- df %>% filter(param == 'multi_or_single_site') %>% 
    mutate(v = ifelse(value == 'single', 'ss', 'ms')) %>% distinct(v) %>% pull()
  exp_anom <- df %>% filter(param == 'anomaly_or_exploratory') %>% 
    mutate(v = ifelse(value == 'anomaly', 'anom', 'exp')) %>% distinct(v) %>% pull()
  time <- df %>% filter(param == 'time') %>% 
    mutate(v = ifelse(value == TRUE, 'at', 'nt')) %>% distinct(v) %>% pull()
  
  output_type <- paste0(check_string, '_', site_type, '_', exp_anom, '_', time)
  
  df_final <- df %>%
    add_row(param = 'output_function',
            value = output_type)
  
  output_tbl(df_final, 'parameter_summary', file = TRUE)
  
  return(output_type)
  
}


#' Generate concept reference table to accompany output
#' 
#' @param tbl intermediate table generated in the output function that contains the concepts
#'            of interest to be displayed in the reference table
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' @param col the name of the column with the concept that needs to be summarised in the 
#'            refrence table
#' @param denom the denominator count associated with @col to be displayed in the 
#'              reference table
#' @param time logical to define whether @tbl has over time output or not
#' 
#' @return a reference table with summary information about the codes in the output that 
#'         could not be displayed in the associated graph

generate_ref_table <- function(tbl,
                               col,
                               denom,
                               time = FALSE){
  if(!time){
      
      t <- tbl %>%
        rename('denom_col' = denom) %>%
        distinct(site, !!sym(col), concept_name, denom_col) %>%
        gt::gt() %>%
        fmt_number(denom_col, decimals = 0) %>%
        data_color(palette = "Dark2", columns = c(site)) %>%
        cols_label(denom_col = 'Total Count') %>%
        tab_header('Concept Reference Table') %>%
        opt_interactive(use_search = TRUE)
  }else{
    
    time_inc <- tbl %>% ungroup() %>% distinct(time_increment) %>% pull()
      
      t <- tbl %>%
        rename('denom_col' = denom) %>%
        distinct(site, !!sym(col), concept_name, denom_col) %>%
        group_by(site, !!sym(col)) %>%
        mutate(denom_col = sum(denom_col)) %>%
        ungroup() %>%
        distinct() %>%
        gt::gt() %>%
        fmt_number(denom_col, decimals = 0) %>%
        data_color(palette = "Dark2", columns = c(site)) %>%
        cols_label(denom_col = 'Total Count (All Time Points)') %>%
        tab_header('Concept Reference Table') %>%
        opt_interactive(use_search = TRUE)
      
  }
  
  return(t)
  
}

#' *Computing Distance From Mean*
#' Should be able to use this for other checks,
#' but naming this way to differentiate from
#' the existing `compute_dist_mean` function
#' @param tbl table with at least the vars specified in `grp_vars` and `var_col`
#' @param grp_vars variables to group by when computing summary statistics
#' @param var_col column to compute summary statistics on
#' @param num_sd (integer) number of standard deviations away from the mean
#'               from which to compute the sd_lower and sd_upper columns
#' @return a table with the `grp_vars` ** | mean | sd | sd_lower | sd_upper | **
#'                                     ** anomaly_yn: indicator of whether data point is +/- num_sd from mean **
#'                                     ** abs_diff_mean: absolute value of difference between mean for group and observation **
compute_dist_mean_median <- function(tbl,
                                      grp_vars,
                                      var_col,
                                      num_sd,
                                      num_mad){
  
  site_rows <-
    tbl %>% ungroup() %>% select(site) %>% distinct()
  grpd_vars_tbl <- tbl %>% ungroup() %>% select(!!!syms(grp_vars)) %>% distinct()
  
  tbl_new <- 
    cross_join(site_rows,
               grpd_vars_tbl) %>% 
    left_join(tbl) %>% 
    mutate(across(where(is.numeric), ~replace_na(.x,0)))
  
  stats <- tbl_new %>%
    group_by(!!!syms(grp_vars))%>%
    summarise(mean=mean(!!!syms(var_col)),
              median=median(!!!syms(var_col)),
              sd=sd(!!!syms(var_col), na.rm=TRUE),
              mad=mad(!!!syms(var_col),center=median),
              `90th_percentile`=quantile(!!!syms(var_col), 0.95)) %>%
    ungroup() %>%
    mutate(sd_lower=mean-num_sd*sd,
           sd_upper=mean+num_sd*sd,
           mad_lower=median-num_mad*mad,
           mad_upper=median+num_mad*mad)
  
  tbl_new %>%
    inner_join(stats)%>%
    mutate(anomaly_yn=case_when(!!sym(var_col)<sd_lower|!!sym(var_col)>sd_upper|!!sym(var_col)>`90th_percentile`~TRUE,
                                TRUE~FALSE),
           abs_diff_mean=abs(!!sym(var_col)-mean),
           abs_diff_median=abs(!!sym(var_col)-median),
           n_mad=abs_diff_median/mad)
  
}


#' *Compute AUC for checks across time*
#' computes AUC for comparisons, iterating through a group of variables
#' for comparative AUC's
#' 
#' @param tbl_name the tbl name to pass through
#' @param iterate_var the variable to iterate and compute an AUC for; 
#'                    defaults to `site`
#' @param time_var  the variable that contains the time; 
#'                   defaults to `time_start`
#' @param outcome_var the outcome variable
#' @param gold_standard_var the variable that contains the gold standard (e.g., all site mean)
#' 
#' @return the variables given as input, with the AUC computed for a given time period, 
#' for all sites provided
#' 

compute_auc_at <- function(tbl_name,
                           iterate_var = 'site',
                           time_var = 'time_start',
                           outcome_var = 'prop_concept',
                           gold_standard_var = 'mean') {
  
  
  
  tbl_name_vars_ready <- 
    tbl_name %>% 
    rename(xaxis_time=!!sym(time_var),
           yaxis=!!sym(outcome_var),
           gold_standard=!!sym(gold_standard_var),
           iterate=!!sym(iterate_var)) %>% 
    select(iterate,
           xaxis_time,
           yaxis,
           gold_standard,
           iterate)
  
  iterate_vector <- 
    tbl_name_vars_ready %>% select(iterate) %>% 
    distinct() %>% pull()
  
  final <- list()
  
  for(i in 1:length(iterate_vector)) {
    
    tbl_norms <- 
      tbl_name_vars_ready %>% 
      filter(iterate==iterate_vector[i]) %>% 
      mutate(xaxis=row_number())
    
    auc_val <- DescTools::AUC(tbl_norms$xaxis,tbl_norms$yaxis,method='spline')
    
    
    tbl_norms_auc <- tbl_norms %>% mutate(auc_value=round(auc_val,2))
    
    final[[i]] <- tbl_norms_auc
  }
  
  final_reduce <- reduce(.x=final,
                         .f=dplyr::union)
  overall <- final_reduce %>% select(xaxis_time,
                                     gold_standard) %>% distinct() %>% 
    mutate(xaxis=row_number()) 
  auc_avg_val <- DescTools::AUC(overall$xaxis,overall$gold_standard,method='spline')
  
  output <- 
    final_reduce %>% 
    mutate(auc_gold_standard=round(auc_avg_val,4)) %>% 
    rename(!!sym(time_var):=xaxis_time,
           !!sym(outcome_var):=yaxis,
           !!sym(gold_standard_var):=gold_standard,
           !!sym(iterate_var):=iterate) 
  
  return(output)
}


