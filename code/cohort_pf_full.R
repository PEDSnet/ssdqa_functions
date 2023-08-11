

pf_process <- function(cohort,
                       site_list,
                       study_name = 'glomerular',
                       visit_types = c('all', 'outpatient'),
                       multi_or_single_site = 'single',
                       collapse_sites = FALSE,
                       #multi_site = FALSE,
                       time = FALSE,
                       time_span = c('2009-01-01', Sys.Date()),
                       age_groups = NULL,
                       codeset = NULL,
                       anomaly_or_exploratory='exploratory',
                       domain_tbl=read_codeset('pf_domains_short','cccc'),
                       visit_type_table=read_codeset('pf_visit_types','ic'),
                       #anomaly_detection = FALSE,
                       #exploratory = TRUE,
                       lof_domains = list('first' = list('conditions_all', '#a5879e'),
                                          'second' = list('anthropometrics','#e8ce4d'),
                                          'third' = list('labs', '#6e9f65'))){
  
  ## Step 0: Set cohort name for table output
  config('cohort', study_name)
  
  ## Step 1: Prepare cohort
  
  cohort_prep <- prepare_pf(cohort = cohort, age_groups = age_groups, codeset = codeset)
  
  ## Step 2: Run Function
  grouped_list <- c('site', 'person_id', 'start_date', 'end_date', 'fu')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  if(is.data.frame(codeset)){grouped_list <- grouped_list %>% append('flag')}
  
    pf_tbl <- loop_through_visits(
      cohort_tbl=cohort_prep,
      time = time,
      collapse_sites=collapse_sites,
      site_list=site_list,
      visit_list=visit_types,
      visit_type_tbl=visit_type_table,
      grouped_list=grouped_list,
      domain_tbl = domain_tbl
     )
  
  
  output_list_to_db(pf_tbl)
  
  pf_final <- combine_study_facts(study_abbr = study_name, time = time, visit_type_list = visit_types)
  
  ## Step 3: Summarise (Medians, LOF)
  if(!time){
    if(anomaly_or_exploratory=='anomaly') {
      if(multi_or_single_site=='single') {
        # lof_input <- create_lof_input(data_tbl=pf_final)
        # lof_output <- create_sepsite_output_lof(input_tbls=lof_input,
        #                                         var_list_arg=lof_domains)
        # pf_output <- sepsite_lof_reduce(lof_output)
        pf_output <- compute_dist_mean(pf_final,
                                       agegrp = age_groups,
                                       codeset = codeset)
      } else {
        medians_prep <- compute_pf_medians(data_input = pf_final,
                           agegrp = age_groups,
                           codeset = codeset)
          
        pf_output <- prep_kmeans(dat=medians_prep, age_group = age_groups, codeset = codeset)}
      } else {
        pf_output <- compute_pf_medians(data_input = pf_final,
                                        agegrp = age_groups,
                                        codeset = codeset)
      }
    } else {pf_output <- pf_final}
  
  
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


# if(time){
#   if(single_or_multi == 'single'){
#     if(anomaly_or_exploratory == 'anomaly'){
#       output <- ss_anom_at
#     } else {
#       output <- ss_exp_at
#     }
#   } else {
#     if(anomaly_or_exploratory == 'anomaly'){
#       output <- ms_anom_at
#     } else {
#       output <- ms_exp_at
#     }
#   }
# } else {
#   if(single_or_multi == 'single'){
#     if(anomaly_or_exploratory == 'anomaly'){
#       output <- ss_anom_nt
#     } else {
#       if('age_grp' %in% colnames()){Output <- ss_facet_age}else{output <- ss_exp_nt}
#       if('flag' %in% colnames()){Output <- ss_facet_code}else{output <- ss_exp_nt}
#     }
#   } else {
#     if(anomaly_or_exploratory == 'anomaly'){
#       output <- ms_anom_nt
#     } else{
#       if('age_grp' %in% colnames()){Output <- ms_facet_age} else {output <- ms_exp_nt}
#       if('flag' %in% colnames()){Output <- ms_facet_code} else {output <- ms_exp_nt}
#     }
#   }
# }
