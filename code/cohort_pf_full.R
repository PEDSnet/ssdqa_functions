

pf_process <- function(cohort,
                       site_list,
                       study_name = 'study',
                       visit_types = c('all'),
                       multi_site = FALSE,
                       time = FALSE,
                       time_span = c('2009-01-01', Sys.Date()),
                       age_groups = FALSE,
                       codeset = FALSE,
                       anomaly_detection = FALSE,
                       exploratory = TRUE,
                       lof_domains = list('first' = list('conditions_all', '#a5879e'),
                                          'second' = list('anthropometrics','#e8ce4d'),
                                          'third' = list('labs', '#6e9f65'),
                                          'fourth' = list('procedures_all', '#7ba8b0'))){
  
  ## Step 0: Set cohort name for table output
  config('cohort', study_name)
  
  ## Step 1: Prepare cohort
  if(age_groups){
    age_arg <- read.csv(file.path(base_dir, 'specs', 'age_group_definitions.csv'))
    }else(age_arg = NULL)
  
  if(codeset){
   cs_arg <- read.csv(file.path(base_dir, 'specs', 'codeset_metadata.csv'))
  }else(cs_arg = NULL)
  
  cohort_prep <- prepare_pf(cohort = cohort, age_groups = age_arg, codeset = cs_arg)
  
  ## Step 2: Run Function
  grouped_list <- c('site', 'person_id', 'start_date', 'end_date', 'fu')
  
  if(age_groups){grouped_list <- grouped_list %>% append('age_grp')}
  if(codeset){grouped_list <- grouped_list %>% append('flag')}
  
  if(!multi_site){pf_tbl <- loop_through_visits(cohort_tbl = cohort_prep,
                                                   site_list = site_list,
                                                   combine_sites = TRUE,
                                                   visit_list = visit_types,
                                                   grouped_list = grouped_list)}
  else if(multi_site){pf_tbl <- loop_through_visits(cohort_tbl = cohort_prep,
                                                       site_list = site_list,
                                                       combine_sites = FALSE,
                                                       visit_list = visit_types,
                                                       grouped_list = grouped_list)}
  
  output_list_to_db(pf_tbl)
  
  pf_final <- combine_study_facts(study_abbr = study_name, visit_type_list = visit_types)
  
  ## Step 3: Summarise (Medians)
  if(exploratory){
    pf_output <- compute_pf_medians(data_input = pf_final,
                                        agegrp = age_groups,
                                        codeset = codeset)}
  if(anomaly_detection){
    if(!multi_site){
      lof_input <- create_lof_input(data_tbl = pf_final)
      lof_output <- create_sepsite_output_lof(input_tbls = lof_input,
                                              var_list_arg = lof_domains)
      pf_output <- sepsite_lof_reduce(lof_output)}
    else{
    "euclidian distance"
    }
  }
  
  return(pf_output)
  
}




pf_output_gen <- function(pf_output,
                          site_list,
                          study_name = 'study',
                          visit_types = c('all'),
                          multi_site = FALSE,
                          time = FALSE,
                          time_span = c('2009-01-01', Sys.Date()),
                          age_groups = FALSE,
                          codeset = FALSE,
                          anomaly_detection = FALSE,
                          exploratory = FALSE){
  
  ## Create empty list to store graphical output
  graph_output <- list()
  
  ## Generate appropriate output based on selections
  if(!multi_site){
    
    if(exploratory){
    ss_med_list <- create_list_input_sepsites(data_tbl = pf_output,
                                              outcome_var = 'median_site_without0s')
    graph_output$ss_med_bar <- create_sepsite_output(ss_med_list)}
    
    if(anomaly_detection){
      create_sepsite_output_lof(pf_output)
    }
    
  }else{
    
    if(exploratory){
    graph_output$ms_exp <- create_multisite_output(pf_output)}
    
    if(anomaly_detection){
      "euclidian distance output"
    }
  }
  
}