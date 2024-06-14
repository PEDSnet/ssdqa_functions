
#' Patient Event Sequencing 
#'
#' @param cohort cohort for SSDQA testing; required fields: 
#'               `site` | `person_id` | `start_date` | `end_date` where start and end date 
#' @param user_cutoff user selected number of days between events to be used
#'                    as a threshold cutoff for analyses
#' @param n_event_a the number of times event A should occur before establishing
#'                  the index date; defaults to 1
#' @param n_event_b the number of times event B should occur before establishing
#'                  the occurrence date; defaults to 1
#' @param pes_event_file CSV file with definitions of each of the events
#' @param multi_or_single_site direction to determine what kind of check to run
#'                             string that is either `multi` or `single`
#' @param anomaly_or_exploratory direction to determine what kind of check to run; a string 
#'                               that is either `anomaly` or `exploratory`
#' @param age_groups If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - @min_age: the minimum age for the group (i.e. 10)
#'                     - @max_age: the maximum age for the group (i.e. 20)
#'                     - @group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'                     
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'                     
#'                     If you would not like to stratify by age group, leave the argument as NULL
#' @param intermediate_tbl logical to define whether an intermediate table with
#'                         patient level output should be returned
#' @param time logical to determine whether to output the check across time
#' @param time_span when `time = TRUE`, a vector of two dates for the observation period of the study
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return dataframe with the number of days between events A and B as an integer,
#'         and the number of patients who had the events occur that far apart;
#'         
#'         over time analyses will return the same output, grouped by each time 
#'         period in the time span provided
#' 
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
  site_filter <- check_site_type(cohort = cohort,
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
    
    pes_tbl_site <- compute_event_sequence(cohort = cohort_site,
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
    replace_site_col()
  
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