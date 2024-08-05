
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
                        site_list
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
      filter(start_date <= baseline_end_date,
             baseline_start_date <= end_date)
    # mutate(start_date = as_date(baseline_start_date),
    #        end_date = as_date(baseline_end_date))
    
    
    cohort_narrow_prepped <- cohort_narrowed %>%
      filter(!! sym(site_col) %in% site_list_v) %>% 
      mutate(time_start=baseline_start_date,
             time_end=baseline_end_date,
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
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
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
      
      cli::cli_abort('Please include data from multiple sites in your cohort to 
                      conduct a multi-site analysis.')
      
    }else if((multi_or_single_site == 'single' && n_site == 1) ||
             (multi_or_single_site == 'multi' && n_site > 1)){
      
      cohort_final <- cohort #%>%
        #filter(site %in% site_list)
      
      site_list <- cohort %>% 
        select(site) %>% distinct() %>% pull()
      
      grouped_list <- c('site')
      site_list_adj <- site_list
      
    }else{cli::cli_abort('Invalid argument for multi_or_single_site. Please select either {.code single} or {.code multi}')}
  }else{cli::cli_abort('Please include a {.code site} column in your cohort.')}
  
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
                               col,
                               vocab_col = 'concept_id'){
  if(!is.null(vocab_tbl)){
    
    final <- select(vocab_tbl, vocab_col, concept_name, vocabulary_id) %>%
      rename('join_col' = vocab_col) %>%
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
                               id_col,
                               name_col,
                               denom,
                               time = FALSE){
  if(!time){
      
      t <- tbl %>%
        rename('denom_col' = denom) %>%
        distinct(site, !!sym(id_col), !!sym(name_col), denom_col) %>%
        gt::gt() %>%
        fmt_number(denom_col, decimals = 0) %>%
        data_color(palette = ssdqa_colors_standard, columns = c(site)) %>%
        cols_label(denom_col = 'Total Count') %>%
        tab_header('Concept Reference Table') %>%
        opt_interactive(use_search = TRUE)
  }else{
    
    time_inc <- tbl %>% ungroup() %>% distinct(time_increment) %>% pull()
      
      t <- tbl %>%
        rename('denom_col' = denom) %>%
        distinct(site, !!sym(id_col), !!sym(name_col), denom_col) %>%
        group_by(site, !!sym(id_col)) %>%
        mutate(denom_col = sum(denom_col)) %>%
        ungroup() %>%
        distinct() %>%
        gt::gt() %>%
        fmt_number(denom_col, decimals = 0) %>%
        data_color(palette = ssdqa_colors_standard, columns = c(site)) %>%
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
  
  final <- tbl_new %>%
    inner_join(stats)%>%
    mutate(anomaly_yn=case_when(!!sym(var_col)<sd_lower|!!sym(var_col)>sd_upper|!!sym(var_col)>`90th_percentile`~TRUE,
                                TRUE~FALSE),
           abs_diff_mean=abs(!!sym(var_col)-mean),
           abs_diff_median=abs(!!sym(var_col)-median),
           n_mad=abs_diff_median/mad)
  
  return(final)
}


#' #' *Compute AUC for checks across time*
#' #' computes AUC for comparisons, iterating through a group of variables
#' #' for comparative AUC's
#' #' 
#' #' @param tbl_name the tbl name to pass through
#' #' @param iterate_var the variable to iterate and compute an AUC for; 
#' #'                    defaults to `site`
#' #' @param time_var  the variable that contains the time; 
#' #'                   defaults to `time_start`
#' #' @param outcome_var the outcome variable
#' #' @param gold_standard_var the variable that contains the gold standard (e.g., all site mean)
#' #' 
#' #' @return the variables given as input, with the AUC computed for a given time period, 
#' #' for all sites provided
#' #' 
#' 
#' compute_auc_at <- function(tbl_name,
#'                            iterate_var = 'site',
#'                            time_var = 'time_start',
#'                            outcome_var = 'prop_concept',
#'                            gold_standard_var = 'mean') {
#'   
#'   
#'   
#'   tbl_name_vars_ready <- 
#'     tbl_name %>% 
#'     rename(xaxis_time=!!sym(time_var),
#'            yaxis=!!sym(outcome_var),
#'            gold_standard=!!sym(gold_standard_var),
#'            iterate=!!sym(iterate_var)) %>% 
#'     select(iterate,
#'            xaxis_time,
#'            yaxis,
#'            gold_standard)
#'   
#'   iterate_vector <- 
#'     tbl_name_vars_ready %>% 
#'     select(iterate) %>% 
#'     distinct() %>% pull()
#'   
#'   final <- list()
#'   
#'   for(i in 1:length(iterate_vector)) {
#'     
#'     tbl_norms <- 
#'       tbl_name_vars_ready %>% 
#'       filter(iterate==iterate_vector[i]) %>% 
#'       mutate(xaxis=row_number())
#'     
#'     auc_val <- DescTools::AUC(tbl_norms$xaxis,tbl_norms$yaxis,method='spline')
#'     
#'     
#'     tbl_norms_auc <- tbl_norms %>% mutate(auc_value=round(auc_val,2))
#'     
#'     final[[i]] <- tbl_norms_auc
#'   }
#'   
#'   final_reduce <- reduce(.x=final,
#'                          .f=dplyr::union)
#'   overall <- final_reduce %>% select(xaxis_time,
#'                                      gold_standard) %>% distinct() %>% 
#'     mutate(xaxis=row_number()) 
#'   auc_avg_val <- DescTools::AUC(overall$xaxis,overall$gold_standard,method='spline')
#'   
#'   output <- 
#'     final_reduce %>% 
#'     mutate(auc_gold_standard=round(auc_avg_val,4)) %>% 
#'     rename(!!sym(time_var):=xaxis_time,
#'            !!sym(outcome_var):=yaxis,
#'            !!sym(gold_standard_var):=gold_standard,
#'            !!sym(iterate_var):=iterate) 
#'   
#'   return(output)
#' }

#' loops through visit types and sites to compute patient facts
#' 
#' @param cohort_tbl the tbl that comes from `prepare_pf`
#' @param check_func the base function for the check that needs to be executed across time; this argument
#'                   should be structured as the following, where cht is the cohort and t is the input data 
#'                   for the function:
#'                   
#'                   function(cht, t){check_function(param1 = cht, param2 = t, param3 = param3_input, ..., 
#'                   paramX = paramX_input)}
#'                   
#'                   all parameters for the base check function should be included if any defaults are not being
#'                   used
#' @param site_col the column in the data where the site variable can be found
#' 
#' @param visit_type_tbl The visit_concept_ids of interest for the analysis. `all` may be used in this field
#'                      to select every visit type; defaults to `pf_visit_types` in specs folder
#' @param visit_tbl the cdm visit_occurrence tbl; defaults to `cdm_tbl('visit_occurrence')`
#' @param site_list the sites to iterate through
#' @param visit_list the list of visit types to iterate through
#' @param grouped_list the input for which to group variables
#' @param domain_tbl defaults to `pf_domains` in the specs folder; 
#'      @domain: the domain name; output will have this domain
#'      @default_tbl: the table to pull from 
#'      @field_name the field name to filter by; leave null if no filter
#'      @field_filter: the filtered codes
#' 
#' @return a returned list stratified by visit type
#' 
loop_through_visits <- function(cohort_tbl,
                                check_func,
                                site_col,
                                #time=FALSE,
                                visit_type_tbl=read_codeset('pf_visit_types','ic'),
                                visit_tbl=cdm_tbl('visit_occurrence'),
                                site_list,
                                visit_list=c('inpatient','outpatient'),
                                grouped_list=c('person_id','start_date','end_date',
                                                'fu','site'),
                                domain_tbl=read_codeset('pf_domains_short','cccc')) {
  
  # iterates through visits
  visit_output <- list()
  for(j in 1:length(visit_list)) {
    
    # iterates through sites
    site_output <- list()
    for(k in 1:length(site_list)) {
      
      site_list_thisrnd <- site_list[[k]]
      
      # filters by site
      cohort_site <- cohort_tbl %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))
      
      # pulls the visit_concept_id's that correspond to the visit_list
      visit_types <- 
        visit_type_tbl %>% 
        filter(visit_type %in% c(visit_list[[j]])) %>% 
        select(visit_concept_id) %>% pull()
      
      # narrows the visit time to cohort_entry and end date
      visits <- 
        cohort_site %>% 
        inner_join(
          select(visit_tbl,
                 person_id,
                 visit_occurrence_id,
                 visit_concept_id,
                 visit_start_date)
        ) %>% 
        filter(visit_concept_id %in% c(visit_types)) %>% 
        filter(visit_start_date >= start_date,
               visit_start_date <= end_date) %>% 
        compute_new(temporary=TRUE,
                    indexes=list('person_id'))
      
      # execute function
      domain_compute <- check_func(cht = cohort_site,
                                   t = visits)
      
      site_output[[k]] <- domain_compute
      
    }
    
    
    all_site <- reduce(.x=site_output,
                       .f=dplyr::union)
    
    #visit_output[[paste0('pf_',config('cohort'),'_',(visit_list[j]))]] <- all_site
    visit_output[[visit_list[j]]] <- all_site
    
  }
  
  visit_output
  
}

#' Create a cross-joined master table for variable reference
#'
#' @param cj_tbl multi-site, over time output from check_code_dist_csd function
#' @param cj_var_names a vector with the names of variables that should be used as the "anchor"
#'                     of the cross join where all combinations of the variables should be
#'                     present in the final table
#' @param join_type the type of join that should be performed at the end of the function
#'                  left is used for multi-site anomaly (euclidean distance) while full
#'                  is used for single site anomaly (timetk package)
#'
#' @return one data frame with all combinations of the variables from cj_var_names with their
#'         associated facts from the original cj_tbl input
#' 
compute_at_cross_join <- function(cj_tbl,
                                  cj_var_names = c('site','concept_id'),
                                  join_type = 'left') {
  
  
  cj_tbl <- ungroup(cj_tbl)
  blah <- list()
  
  date_first <- cj_tbl %>% distinct(time_start) %>% arrange(time_start) %>% first() %>% pull()
  date_last <- cj_tbl %>% distinct(time_start) %>% arrange(time_start) %>% last() %>% pull()
  time_increment_var <- cj_tbl %>% distinct(time_increment) %>% pull()
  
  all_months <- seq.Date(from=date_first,
                         to=date_last,
                         by=time_increment_var)
  all_months_tbl <- as_tibble(all_months) %>% rename(time_start=value)
  
  for(i in 1:length(cj_var_names)) {
    
    cj_var_name_i <- (cj_var_names[[i]])
    
    cj_tbl_narrowed <- cj_tbl %>% distinct(!! sym(cj_var_name_i))
    
    blah[[i]] <- cj_tbl_narrowed
    
  }
  
  cj_tbl_cjd <- reduce(.x=blah,
                       .f=cross_join)
  
  cj_tbl_cjd_time <- 
    all_months_tbl %>% cross_join(cj_tbl_cjd)
  
  if(join_type == 'left'){
    cj_tbl_full <- 
      cj_tbl_cjd_time %>% 
      left_join(cj_tbl) %>% 
      mutate(across(where(is.numeric), ~ replace_na(.x,0)))
  }else{
    cj_tbl_full <- 
      cj_tbl_cjd_time %>% 
      full_join(cj_tbl) %>% 
      mutate(across(where(is.numeric), ~ replace_na(.x,0)))
  }
  
  
}

#' Compute Euclidean Distance
#'
#' @param ms_tbl output from compute_dist_mean_median where the cross-joined table from
#'               compute_at_cross_join is used as input
#' @param output_var the output variable that should be used to compute the Euclidean distance
#'                   i.e. a count or proportion
#'
#' @return one dataframe with all variables from ms_tbl with the addition of columns with a site Loess
#'         value and a site Euclidean distance value
#' 
compute_euclidean <- function(ms_tbl,
                              output_var,
                              grp_vars = c('site', 'concept_id')) {
  
  grp_tbls <- group_split(ms_tbl %>% unite(facet_col, !!!syms(grp_vars), sep = '_', remove = FALSE) %>%
                            group_by(facet_col))
  
  euclidean_dist <- function(x, y) sqrt(sum((x - y)^2)) 
  
  overall <- list()
  
  for(i in 1:length(grp_tbls)) {
    
    site_datenumeric <- 
      grp_tbls[[i]] %>%  
      mutate(date_numeric = as.numeric(time_start),
             output_var = !!sym(output_var))
    site_loess <- loess(output_var ~ date_numeric, data=site_datenumeric)
    site_loess_df <- as_tibble(predict(site_loess)) %>% rename(site_loess=1) 
    euclidean_site_loess <- euclidean_dist(predict(site_loess), site_datenumeric$mean_allsiteprop)
    ms_witheuclidean <- 
      cbind(site_datenumeric,site_loess_df) %>% 
      mutate(dist_eucl_mean=euclidean_site_loess) #%>% 
    # mutate(loess_predicted=predict(site_loess)) 
    
    overall[[i]] <- ms_witheuclidean
    
  }
  
  overall_reduce <- reduce(.x=overall,
                           .f=dplyr::union) %>% as_tibble() %>% 
    mutate(dist_eucl_mean=round(dist_eucl_mean,2),
           site_loess=round(site_loess,2)) %>%
    select(-facet_col)
  
}

#' Euclidean Distance for *_ms_anom_at output
#'
#' @param fot_input_tbl table output by compute_fot where the check of interest
#'                      is used as the check_func
#' @param grp_vars the variables that should be preserved in the cross join
#' @param var_col the column with the numerical statistic of interest for the euclidean
#'                distance computation
#' @param time_period a string denoting the period of time that separates each date value
#'                    (i.e. month, year, etc)
#'
#' @return data frame with mean and median values for the user provided variable column
#'         and the euclidean distance value from the all site mean
#' 
ms_anom_euclidean <- function(fot_input_tbl,
                              grp_vars,
                              var_col) {
  
  
  ms_at_cj <- compute_at_cross_join(cj_tbl=fot_input_tbl,
                                    cj_var_names = grp_vars)
  
  allsite_grps <- grp_vars %>% append('time_start')
  allsite_grps <- allsite_grps[! allsite_grps %in% c('site')]
  
  ms_at_cj_avg <- compute_dist_mean_median(tbl=ms_at_cj,
                                           grp_vars=allsite_grps,
                                           var_col=var_col,
                                           num_sd = 2,
                                           num_mad = 2)  %>% 
    rename(mean_allsiteprop=mean) 
  
  euclidiean_tbl <- compute_euclidean(ms_tbl=ms_at_cj_avg,
                                      output_var=var_col,
                                      grp_vars = grp_vars)
  
  final <- 
    euclidiean_tbl %>% 
    select(site,time_start, grp_vars, var_col,
           mean_allsiteprop, median, date_numeric,
           site_loess,dist_eucl_mean
    )
  
  return(final)
  
}


#' Run Anomilization for *_ss_anom_at
#'
#' @param fot_input_tbl table output by compute_fot where the check of interest
#'                      is used as the check_func
#' @param grp_vars the variables that should be preserved in the cross join
#' @param var_col the column with the numerical statistic of interest for the euclidean
#'                distance computation
#' @param time_var the column with time information 
#'
#' @return one dataframe with all columns from the original input table
#'         plus the columns needed for timetk output generated by the
#'         `anomalize` function
#'
anomalize_ss_anom_at <- function(fot_input_tbl,
                                 grp_vars,
                                 time_var,
                                 var_col){
  
  time_inc <- fot_input_tbl %>% ungroup() %>% filter(!is.na(time_increment)) %>%
    distinct(time_increment) %>% pull()
  
  if(time_inc == 'year'){
    
    final_tbl <- fot_input_tbl
    
  }else{
    
  plt_tbl <- compute_at_cross_join(cj_tbl = fot_input_tbl,
                                   cj_var_names = grp_vars,
                                   join_type = 'full')
  
  anomalize_tbl <- anomalize(plt_tbl %>% group_by(!!!syms(grp_vars)),
                             .date_var=!!sym(time_var), 
                             .value=!!sym(var_col))
  
  final_tbl <- plt_tbl %>%
    left_join(anomalize_tbl) %>%
    ungroup()
  
  }
  
  return(final_tbl)
  
}

#' @param df_tbl output from the computation of a particular function for anomaly detection
#' @param grp_vars the columns to group by to compute the summary statistics for
#' @param prop_concept column to perform summary statistics for, to detect an anomaly 
#' 
#' @return the `df_tbl` with the following computed: 
#'  `mean_val`, `median_val`, `sd_val`, `mad_val`, `cov_val`, `max_val`, 
#'  `min_val`, `range_val`, `total_ct`, `analysis_eligible`
#'  the `analysis_eligible` will indicate whether the group for which the user
#'  wishes to detect an anomaly for is eligible for analysis.
#'  
#'  The following conditions will disqualify a group from the anomaly detection analysis:
#'  (1) Sample size < 5 in group
#'  (2) Mean < 0.02 or Median < 0.01
#'  (3) Mean value < 0.05 and range <0.01
#'  (4) Coefficient of variance <0.01 and sample size <11
#'  


compute_dist_anomalies <- function(df_tbl,
                                   grp_vars,
                                   var_col,
                                   denom_cols){
  
  site_rows <-
    df_tbl %>% ungroup() %>% select(site) %>% distinct()
  grpd_vars_tbl <- df_tbl %>% ungroup() %>% select(!!!syms(grp_vars)) %>% distinct()
  denom_tbl <- df_tbl %>% ungroup() %>% select(site, !!!syms(denom_cols)) %>% distinct()
  
  tbl_new <- 
    cross_join(site_rows,
               grpd_vars_tbl) %>% 
    left_join(denom_tbl) %>%
    left_join(df_tbl) %>% 
    mutate(across(where(is.numeric), ~replace_na(.x,0)))
  
  
  stats <- tbl_new %>%
    group_by(!!!syms(grp_vars))%>%
    summarise(mean_val=mean(!!!syms(var_col)),
              median_val=median(!!!syms(var_col)),
              sd_val=sd(!!!syms(var_col), na.rm=TRUE),
              mad_val=mad(!!!syms(var_col)),
              cov_val=sd(!!!syms(var_col),na.rm=TRUE)/mean(!!!syms(var_col)),
              max_val=max(!!!syms(var_col)),
              min_val=min(!!!syms(var_col)),
              range_val=max_val-min_val,
              total_ct=n()) %>% ungroup() %>% 
    ungroup() %>% mutate(analysis_eligible = 
                           case_when(mean_val < 0.02 | median_val < 0.01 | 
                                       (mean_val < 0.05 & range_val < 0.1) | 
                                       (cov_val < 0.1 & total_ct < 11) ~ 'no',
                                     TRUE ~ 'yes'))
  final <- tbl_new %>% left_join(stats,
                        by=c(grp_vars))
  
  return(final)
  
  
}



#' Computes anomaly detection for a group (e.g., multi-site analysis)
#' Assumes: (1) No time component; (2) Table has a column indicating 
#' whether a particular group or row is eligible for analysis; (3) column 
#' variable exists for which to compute the anomaly
#' 
#' @param df_tbl tbl for analysis; usually output from `compute_dist_anomalies`
#' @param tail_input whether to detect anomaly on right, left, or both sides; defaults to `both`
#' @param p_input the threshold for anomaly; defaults to 0.9
#' @param column_analysis a string, which the name of the column for which to compute anomaly detection;
#' @param column_variable a string, which is the name of the variable to compute summary statistics for;
#' @param column_eligible a string, which is the name of the column that indicates eligibility for analysis
#' 

detect_outliers <- function(df_tbl,
                            tail_input = 'both', 
                            p_input = 0.9,
                            column_analysis = 'prop_concept',
                            column_eligible = 'analysis_eligible',
                            column_variable = 'concept_id') {
  
  final <- list()
  
  eligible_outliers <- 
    df_tbl %>% filter(!! sym(column_eligible) == 'yes')
  
  if(nrow(eligible_outliers) == 0){
    
    output_final_all <- df_tbl %>% mutate(anomaly_yn = 'no outlier in group')
    
    cli::cli_warn('No variables were eligible for anomaly detection analysis')
  
  }else{
  
  groups_analysis <- group_split(eligible_outliers %>% unite(facet_col, !!!syms(column_variable), sep = '_', remove = FALSE) %>%
                                   group_by(facet_col))
  
  for(i in 1:length(groups_analysis)) {
    
    # filtered <- 
    #   eligible_outliers %>% filter(!!! syms(column_variable) == i) 
    
    vector_outliers <- 
      groups_analysis[[i]] %>% select(!! sym(column_analysis)) %>% pull()
    
    outliers_test <- 
      hotspots::outliers(x=vector_outliers, p=p_input, tail= tail_input)
    
    output <- groups_analysis[[i]] %>% mutate(
      lower_tail = outliers_test[[10]],
      upper_tail = outliers_test[[9]]
    ) %>% mutate(anomaly_yn = case_when(!! sym(column_analysis) < lower_tail |
                                          !! sym(column_analysis) > upper_tail ~ 'outlier',
                                        TRUE ~ 'not outlier'))
    
    final[[i]] <- output
    
    
  }
  
  final
  
  output_final_anomaly <- purrr::reduce(.x=final,
                                        .f=dplyr::union)
  
  output_final_all <- df_tbl %>% left_join(output_final_anomaly) %>% 
    mutate(anomaly_yn=case_when(
      is.na(anomaly_yn) ~ 'no outlier in group',
      TRUE ~ anomaly_yn
    )) %>% select(-facet_col)
  }
  
  return(output_final_all)
}


#' Compute Jaccard Index
#' 
#' @param jaccard_input_tbl tbl that will undergo jaccard index computation;
#'                          the requirement is that it contains at least two columns: `person_id` and `concept_id`
#'                          where each row represents an instance where a specific `concept_id` is used for a given patient (`person_id`)
#'                          Alternatively, it can be a list of all unique `person_id` and `concept_id` combinations
#' @param var_col the column within `jaccard_input_table` that contains all the concepts that should be compared to each other
#'                          
#' @return a table with both concepts, labeled `concept1` and `concept2`, the co-occurrence (`cocount`), individual
#'         concept counts (`concept1_ct`, `concept2_ct`), total unique patient counts where either code is used (`concept_count_union`), 
#'         the `jaccard_index`, as well as proportion of patients where the concept appears (`concept1_prop`, `concepet2_prop`) 


compute_jaccard <- function(jaccard_input_tbl,
                            var_col) {
  
  match_class <- function(x, type = var_class) {class(x) <- type; x}
  
  persons_concepts <- 
    jaccard_input_tbl %>% ungroup %>% #distinct() %>% collect()
    select(person_id,
           var_col) %>% distinct() %>% collect()
  
  var_class <- class(persons_concepts[[var_col]])
  
  persons_concepts_cts <- 
    persons_concepts %>% 
    group_by(!!sym(var_col)) %>% 
    summarise(var_person_ct=n_distinct(person_id))
  
  concord <- 
    persons_concepts %>% table() %>% crossprod()
  diag(concord) <- -1
  
  best <- as_tibble(concord, rownames='concept1') %>% 
    pivot_longer(!concept1, names_to = 'concept2', values_to='cocount') %>% 
    filter(cocount != -1L) %>% mutate(across(.cols = c(concept1, concept2), .fns=match_class)) %>%
    mutate(cocount = as.integer(cocount)) %>%
    left_join(persons_concepts_cts, by = c('concept1'=var_col))%>%
    rename(concept1_ct=var_person_ct)%>%
    left_join(persons_concepts_cts, by = c('concept2'=var_col))%>%
    rename(concept2_ct=var_person_ct) %>%
    mutate(concept_count_union=concept1_ct+concept2_ct-cocount,
           jaccard_index=cocount/concept_count_union) %>% 
    mutate(concept1_prop=round(cocount/concept1_ct,2),
           concept2_prop=round(cocount/concept2_ct,2)) %>% 
    filter(concept1_ct > 0 & concept2_ct > 0 & cocount > 0) %>% 
    filter(concept1 > concept2) 
  
  best
  
}
