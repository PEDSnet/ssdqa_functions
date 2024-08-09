
prc_process <- function(cohort,
                        prc_event_file = read_codeset('pes_events_1', 'cccc'),
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = NULL,
                        intermediate_tbl = FALSE,
                        p_value = 0.9,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'){
  
  ## parameter summary output
  output_type <- suppressWarnings(param_csv_summ2(check_string = 'pes',
                                                  as.list(environment())))
  
  
  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  
  # Set up grouped list
  
  #grouped_list <- grouped_list %>% append('domain')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  
  #site_output <- list()
  
  # Prep cohort
  
  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, 
                                codeset = NULL) %>%
    group_by(!!! syms(grouped_list))
  
  if(!time){
    
    if(multi_or_single_site == 'single' & anomaly_or_exploratory == 'anomaly'){
      
      prc_tbl <- compute_prc_ntanom(cohort = cohort_prep,
                                    grouped_list = grouped_list,
                                    event_csv = prc_event_file,
                                    target_col = 'bin_col')
      
    }else if(multi_or_single_site == 'multi' & anomaly_or_exploratory == 'anomaly'){
      
      prc_tbl_jacc <- compute_prc_ntanom(cohort = cohort_prep,
                                         grouped_list = grouped_list,
                                         event_csv = prc_event_file,
                                         target_col = 'bin_col') %>%
        mutate(bin_pair = paste0(concept2, ' & ', concept1))
      
      prc_tbl_int <- compute_dist_anomalies(df_tbl = prc_tbl_jacc %>% rename('site' = grp),
                                            grp_vars = c('bin_pair'), 
                                            var_col = 'jaccard_index',
                                            denom_cols = c('bin_pair')) 
      
      prc_tbl <- detect_outliers(df_tbl = prc_tbl_int,
                                 tail_input = 'both',
                                 p_input = p_value,
                                 column_analysis = 'jaccard_index',
                                 column_variable = 'bin_pair')
      
    }else{
      
      prc_tbl <- compute_event_counts(cohort = cohort_prep,
                                      grouped_list = grouped_list,
                                      site_col = site_col,
                                      intermediate_tbl = intermediate_tbl,
                                      time = time,
                                      event_csv = prc_event_file)
      }
    
  }else{
    
    prc_tbl <- compute_fot(cohort = cohort_prep,
                           site_col = site_col,
                           reduce_id = NULL,
                           time_period = time_period,
                           time_span = time_span,
                           site_list = site_list_adj,
                           check_func = function(dat){
                             compute_event_counts(cohort = dat,
                                                  grouped_list = grouped_list,
                                                  site_col = site_col,
                                                  intermediate_tbl = intermediate_tbl,
                                                  time = time,
                                                  event_csv = prc_event_file)
                           })
    
    if(multi_or_single_site == 'single' & anomaly_or_exploratory == 'anomaly'){
      
      prc_expanded <- prc_tbl %>%
        uncount(pt_ct) %>%
        mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                     event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                     event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                     event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
        group_by(!!sym(site_col), time_start, time_increment, event_a_name, event_b_name, total_pts, stat_type) %>%
        summarise(stat_ct = n(),
                  prop_event = stat_ct / total_pts) %>% ungroup()
      
      prc_tbl <- anomalize_ss_anom_at(fot_input_tbl = prc_expanded %>% distinct(),
                                      grp_vars = 'stat_type',
                                      time_var = 'time_start',
                                      var_col = 'prop_event')
      
    }else if(multi_or_single_site == 'multi' & anomaly_or_exploratory == 'anomaly'){
      
      prc_expanded <- prc_tbl %>%
        uncount(pt_ct) %>%
        mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                     event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                     event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                     event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
        group_by(!!sym(site_col), time_start, time_increment, event_a_name, event_b_name, total_pts, stat_type) %>%
        summarise(stat_ct = n(),
                  prop_event = stat_ct / total_pts) %>% ungroup() 
      
      prc_tbl <- ms_anom_euclidean(fot_input_tbl = prc_expanded %>% distinct(),
                                   grp_vars = c('site', 'stat_type'),
                                   var_col = 'prop_event')
      
    }
    
  }
  
  return(prc_tbl %>% replace_site_col())
  
}