
###### USE TIDYR::UNCOUNT TO EXPAND!!!!

pes_process <- function(cohort,
                        user_cutoff = 30,
                        n_event_a = 1,
                        n_event_b = 1,
                        pes_event_file = read_codeset('pes_events', 'cccc'),
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = NULL,
                        intermediate_tbl = FALSE,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'){
  
  ## parameter summary output
  output_type <- suppressWarnings(param_csv_summ2(check_string = 'pes',
                                                  as.list(environment())))
  
  
  # Add site check
  site_filter <- check_site_type_pcnt(cohort = cohort,
                                      multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  
  # Set up grouped list
  
  #grouped_list <- grouped_list %>% append('domain')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  
  site_output <- list()
  
  # Prep cohort
  
  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, 
                                codeset = NULL) %>%
    group_by(!!! syms(grouped_list))
  
  for(k in 1:length(site_list_adj)) {
    
    site_list_thisrnd <- site_list_adj[[k]]
    
    # filters by site
    cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))
    
    pes_tbl_site <- compute_event_sequence_pcnt(cohort = cohort_site,
                                           grouped_list = grouped_list,
                                           user_cutoff = user_cutoff,
                                           n_event_a = n_event_a,
                                           n_event_b = n_event_b,
                                           time = time,
                                           time_period = time_period,
                                           time_span = time_span,
                                           intermediate_tbl = intermediate_tbl,
                                           event_csv = pes_event_file)
    
    site_output[[k]] <- pes_tbl_site
    
  }
  
  pes_tbl_final <- purrr::reduce(.x = site_output,
                                 .f = dplyr::union) %>%
    replace_site_col_pcnt()
  
  if(time){
    file_name <- paste0(output_type, '_', time_period, '_', config('qry_site'))
  }else{
    file_name <- paste0(output_type, '_', config('qry_site'))
  }
  
  pes_tbl_final %>%
    replace_site_col_pcnt() %>%
    output_tbl(file_name, file = TRUE)
  
  return(pes_tbl_final %>% replace_site_col_pcnt())
  
  message(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                       output function in pes_output: ', output_type, '. This is also included
                       in the parameter_summary.csv file output to the results directory.')))
  
}