
#' Clinical Facts per Patient
#' 
#' This is a completeness check that will compute the number of facts per years of follow-up for each patient in 
#' a cohort. The user will provide the domains of interest in a provided CSV file. Results can optionally be stratified
#' by site, age group, visit type, codeset utilization, and/or time.
#'
#' @param cohort - A dataframe with the cohort of patients for your study. Should include the columns:
#'                       - @person_id
#'                       - @start_date
#'                       - @end_date
#'                       - @site
#' @param site_list - A list of sites for which you would like to examine clinical facts. Can be one site (single-site) or multiple
#'                    (multi-site)
#' @param study_name - A custom string label with the name of your study 
#' @param intermediate_tbl - optional parameter to return an intermediate results table in addition to the standard summary results
#'                           table. the options for the method of return of this table are:
#'                           - @r_dataframe: will return a dataframe within your local R environment
#'                           - @database_table: will write the intermediate table to a database based on the provided connection
#'                                              information
#'                           - @csv: will write the table to a CSV file and save it to the `results` folder provided in the package
#'                           - @NULL: the function will not save an intermediate results table
#' @param visit_types - A list of visit types by which the output should be stratified. Options for visit types can be found or adjusted
#'                      in the provided `pf_visit_types.csv` file. If you would not like to stratify your results by visit type, please
#'                      set the visit_types argument equal to `all`
#' @param multi_or_single_site - Option to run the function on a single vs multiple sites
#'                               - @single - run the function for a single site
#'                               - @multi - run the function for multiple sites
#' @param p_value the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param time - a logical that tells the function whether you would like to look at the output over time
#' @param time_span - when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                    formatted as c(start date, end date) in yyyy-mm-dd date format.
#' @param time_period - when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                      to `year`, but other time periods such as `month` or `week` are also acceptable
#' @param age_groups - If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - @min_age: the minimum age for the group (i.e. 10)
#'                     - @max_age: the maximum age for the group (i.e. 20)
#'                     - @group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'                     
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'                     
#'                     If you would not like to stratify by age group, leave the argument as NULL
#'                     
#' @param codeset - If you would like to stratify the results by codeset utilization, first supply your codeset and add a column called
#'                  `flag` to the codeset. This column will be used to identify groups for each of the codes. You can either use one flag
#'                  label for the whole codeset, or identify subgroups within the codeset with different flag labels. Once your codeset
#'                  as been annotated, fill out the provided `codeset_metadata.csv` file with the following information:
#'                  - @table: the CDM table where the codes can be found (i.e. condition_occurrence)
#'                  - @field: the field within the CDM table where the codes can be found (i.e. condition_concept_id)
#'                  - @file_name: the name of the codeset file with the added flag column you provided
#'                  
#'                  
#'                  Then supply this csv file as the codeset argument (i.e. read.csv('path/to/codeset_metadata.csv'))
#'                     
#'                  If you would not like to stratify by codeset, leave the argument as NULL
#'                  
#' @param anomaly_or_exploratory - Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                                 level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                                 analyses are specialized to identify outliers within the cohort.
#' @param domain_tbl - a csv file that defines the domains where facts should be identified. defaults to the provided
#'                     `pf_domains.csv` file, which contains the following fields:
#'                     - @domain: a string label for the domain being examined (i.e. prescription drugs)
#'                     - @default_tbl: the CDM table where information for this domain can be found (i.e. drug_exposure)
#'                     - @field_name: if filtering is required, which field should be used to filter the default_tbl (i.e. drug_type_concept_id)
#'                     - @field_filter: if filtering is required, a string with codes that will filter the default_tbl to the
#'                                      domain of interest (i.e. "38000177")
#'                                      
#'                    This CSV can be altered to fit the users needs, or another csv with the same columns and formatting can be supplied.
#' @param visit_type_table - a csv file that defines available visit types that are called in @visit_types. defaults to the provided
#'                           `pf_visit_types.csv` file, which contains the following fields:
#'                           - @visit_concept_id: the visit_concept_id that represents the visit type of interest (i.e. 9201)
#'                           - @visit_type: the string label to describe the visit type; this label can be used multiple times
#'                                          within the file if multiple visit_concept_ids represent the visit type
#'                          
#'                           This CSV can be altered to fit the users needs, or another csv with the same columns and formatting can be supplied.
#'
#' @return if intermediate_tbl is not NULL, an intermediate results table is returned in the user-defined format
#' @return a dataframe with summary results (i.e. medians) that can be used as the input for `pf_output_gen` to generate graphical output
#' @return a CSV file with a summary of the parameters used to configure the function and recommendations for how to configure the
#'         `pf_output_gen` function parameters; written to the provided results folder
#'         
 
pf_process <- function(cohort = cohort,
                       #site_list = c('seattle','cchmc'),
                       study_name = 'glom',
                       intermediate_tbl = 'r_dataframe',
                       visit_types = c('outpatient','inpatient'),
                       multi_or_single_site = 'multi',
                       time = FALSE,
                       time_span = c('2014-01-01', '2023-01-01'),
                       time_period = 'year',
                       p_value = 0.9,
                       age_groups = NULL,
                       codeset = NULL,
                       anomaly_or_exploratory='anomaly',
                       domain_tbl=read_codeset('pf_domains_short','cccc'),
                       visit_type_table=read_codeset('pf_visit_types','ic')){
  
  ## Step 0: Set cohort name for table output
  config('cohort', study_name)
  
  ## parameter summary output
  output_type <- suppressWarnings(param_csv_summ2(check_string = 'pf',
                                                  as.list(environment())))
  
  ## Step 1: Check Sites
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  
  ## Step 2: Prep cohort
  
  cohort_prep <- prepare_cohort(cohort = cohort_filter, age_groups = age_groups, codeset = codeset)
  
  ## Step 3: Run Function
  
  if(!time){grouped_list <- grouped_list %>% append(c('person_id','start_date','end_date','fu'))}else{
    grouped_list <- grouped_list %>% append(c('person_id','time_start','time_increment','fu'))}
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  if(is.data.frame(codeset)){grouped_list <- grouped_list %>% append('flag')}
  
  if(time){
    
    grouped_list <- grouped_list[! grouped_list %in% 'fu']
    
    pf_int <- compute_fot(cohort = cohort_prep,
                            site_col = site_col,
                            reduce_id = 'visit_type',
                            time_period = time_period,
                            time_span = time_span,
                            site_list = site_list_adj,
                            check_func = function(dat){
                              loop_through_visits(cohort_tbl = dat,
                                                  check_func = function(cht, t){
                                                    compute_pf_for_fot(cohort = cht,
                                                                       pf_input_tbl = t,
                                                                       grouped_list = grouped_list,
                                                                       domain_tbl = domain_tbl)},
                                                  site_col = site_col,
                                                  #time = TRUE,
                                                  visit_type_tbl=visit_type_table,
                                                  site_list=site_list_adj,
                                                  visit_list=visit_types,
                                                  grouped_list=grouped_list,
                                                  domain_tbl=domain_tbl)
                            })
    
  } else {
    pf_tbl <- loop_through_visits(
      cohort_tbl=cohort_prep,
      check_func = function(cht, t){
        compute_pf(cohort = cht,
                   pf_input_tbl = t,
                   grouped_list = grouped_list,
                   domain_tbl = domain_tbl)},
      site_col = site_col, 
      #time = FALSE,
      site_list=site_list_adj,
      visit_list=visit_types,
      visit_type_tbl=visit_type_table,
      grouped_list=grouped_list,
      domain_tbl = domain_tbl
    )
    
    ### NEED TO MAKE SURE THAT CREATING LONG TABLE IS A GOOD DECISION FOR REPRODUCIBILITY 
    pf_int <- combine_study_facts(pf_tbl=pf_tbl,
                                  domain_list = domain_tbl,
                                  study_abbr = study_name, 
                                  time = time, 
                                  visit_type_list = visit_types) %>% collect()
    }
  
  # Output intermediate results if requested
  if(!is.null(intermediate_tbl)){
    if(intermediate_tbl == 'r_dataframe'){
      pf_intermediate_results <<- pf_int
    }else if(intermediate_tbl == 'database_table'){
        output_tbl(pf_int, 'pf_intermediate_results')
    }else if(intermediate_tbl == 'csv'){
      output_tbl(pf_int, 'pf_intermediate_results', file = TRUE)}
  }else{pf_intermediate_results <- NULL}
  
  ## Step 4: Summarise (Medians, SD)
  if(!time) {
    if(anomaly_or_exploratory=='anomaly' && multi_or_single_site=='single') {
      pf_final <- compute_dist_mean_pf(pf_int,
                                       n_sd = 2,
                                       site_col = site_col,
                                       agegrp= age_groups,
                                       codeset = codeset)
    } else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'multi'){
      
      pf_int_summ <- pf_int %>% group_by(site, visit_type, domain) %>% 
        summarise(tot_pt = n_distinct(person_id), n_pt_fact = sum(var_ever)) %>% 
        mutate(prop_pt_fact = n_pt_fact / tot_pt)
      
      pf_anom_int <- compute_dist_anomalies(df_tbl = pf_int_summ %>% replace_site_col(),
                                            grp_vars = c('domain', 'visit_type'), 
                                            var_col = 'prop_pt_fact',
                                            denom_cols = c('domain', 'visit_type', 'tot_pt')) 
      
      pf_final <- detect_outliers(df_tbl = pf_anom_int,
                                  tail_input = 'both',
                                  p_input = p_value,
                                  column_analysis = 'prop_pt_fact',
                                  column_variable = c('domain', 'visit_type'))
      
    } else {pf_final <- compute_pf_medians(data_input=pf_int,
                                            site_col = site_col,
                                            agegrp = age_groups,
                                            codeset=codeset)}
    
  }else{
    
    if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'multi'){
      
      pf_final <- ms_anom_euclidean(fot_input_tbl = pf_int %>% mutate(prop_pts_fact = fact_ct_denom / site_visit_ct),
                                    grp_vars = c('site', 'visit_type', 'domain'),
                                    var_col = 'prop_pts_fact')
      
    }else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'single'){
      
      pf_final <- anomalize_ss_anom_at(fot_input_tbl = pf_int %>% mutate(prop_pts_fact = fact_ct_denom / site_visit_ct),
                                       time_var = 'time_start',
                                       grp_vars = c('domain', 'visit_type'),
                                       var_col = 'prop_pts_fact')
      
      }else{pf_final <- pf_int}
    
    }
  
  message(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                       output function in pf_output_gen: ', output_type, '. This is also included
                       in the parameter_summary.csv file output to the results directory.')))
  
  pf_final %>%
    replace_site_col() %>%
    output_tbl('pf_process_results', file = TRUE)
  
  return(pf_final %>% replace_site_col())
  
}


#' Clinical Facet per Patient -- Output Generation
#'
#' @param pf_process - the summary dataframe output by the `pf_process` function. 
#' 
#'                     Note any intermediate table generated is not intended to be used with this function.
#' @param output_function - the name of the output function that should be used provided in the `parameter_summary` csv 
#'                          file that is output to the provided results folder after running the `pf_process` function 
#' @param output - which output variable you would like to use in the graphs. available options based on check 
#'                 configuration are provided in the `parameter_summary` csv file
#' @param facet - the variables by which you would like to facet the graph. available and/or recommended options for
#'                faceting variables are provided in the `parameter_summary` csv file
#' @param time_span - the length of time that should be displayed for relevant over-time graphs. this time period should
#'                    either be the same as or a subset of the time period used in the `pf_process` function
#' @param date_breaks_str - only for single-site, exploratory, over time; a string that informs the program how the
#'                          time period should be divided (i.e. '1 year', '3 months', etc). Defaults to 1 year.
#' @param domain_filter for `pf_ms_anom_at` only, the single domain to which the graph should be filtered for displaying
#'                      AUC values
#' @param visit_filter for `pf_ms_anom_at` only, the single visit type to which the graph should be filtered for displaying
#'                     AUC values
#'
#' @return "raw" output that can be called within the R environment to generate the graph in the Viewer window
#' 

pf_output <- function(process_output,
                      output_function,
                      output,
                      facet = NULL,
                      time_span = c('2012-01-01', '2023-01-01'),
                      date_breaks_str = '1 year',
                      domain_filter = 'conditions_all',
                      visit_filter = 'outpatient'){
  
  ## Run output functions
  if(output_function == 'pf_ms_anom_nt'){
    pf_output <- pf_ms_anom_nt(data_tbl = process_output,
                               #output = output,
                               facet = facet,
                               #kmeans_clusters = kmeans_clusters
                               visit_filter = visit_filter)
  }else if(output_function == 'pf_ss_anom_nt'){
    pf_output <- pf_ss_anom_nt(data_tbl = process_output,
                               output = output,
                               facet = facet)
  }else if(output_function == 'pf_ms_exp_nt'){
    pf_output <- pf_ms_exp_nt(data_tbl = process_output,
                              output = output,
                              facet = facet)
  }else if(output_function == 'pf_ss_exp_nt'){
    pf_output <- pf_ss_exp_nt(data_tbl = process_output,
                              output = output,
                              facet = facet)
  }else if(output_function == 'pf_ms_anom_at'){
    pf_output <- pf_ms_anom_at(process_output = process_output,
                               domain_filter = domain_filter,
                               visit_filter = visit_filter)
  }else if(output_function == 'pf_ss_anom_at'){
    pf_output <- pf_ss_anom_at(data_tbl = process_output,
                               #output = output,
                               facet = facet,
                               visit_filter = visit_filter,
                               domain_filter = domain_filter)
  }else if(output_function == 'pf_ms_exp_at'){
    pf_output <- pf_ms_exp_at(data_tbl = process_output,
                              output = output,
                              facet = facet,
                              time_span = time_span)
  }else if(output_function == 'pf_ss_exp_at'){
    pf_output <- pf_ss_exp_at(data_tbl = process_output,
                              output = output,
                              facet = facet,
                              date_breaks_str = date_breaks_str)
  }else(cli::cli_abort('Please enter a valid output_function for this check type.'))
  
  return(pf_output)
}
