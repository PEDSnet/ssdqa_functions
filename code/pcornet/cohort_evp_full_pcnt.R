
#' EVP Process function 
#' 
#'
#' @param cohort A dataframe with the cohort of patients for your study. Should include the columns:
#'                       - @person_id
#'                       - @start_date
#'                       - @end_date
#'                       - @site
#' @param site_list A list of sites for which you would like to examine clinical facts. Can be one site 
#'                  (single-site) or multiple (multi-site) 
#' @param evp_concept_file CSV file with information about each of the concept sets that should be
#'                         examined in the function. contains the following columns:
#'                         - @concept_group a label for the group captured by the associated codeset
#'                         - @default_tbl CDM table where data related to the codeset is found
#'                         - @field_name concept_id field with codes from the associated codeset
#'                         - @date_field a date field in the `default_tbl` that should be used for
#'                                       over time analyses
#'                         - @codeset_name the name of the codeset file; DO NOT include the file extension
#'                         
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#'                               - @single - run the function for a single site
#'                               - @multi - run the function for multiple sites
#' @param anomaly_or_exploratory Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                               level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                               analyses are specialized to identify outliers within the cohort.
#' @param output_level the level of output to use for an AUC computation, exclusive to `ms_anom_at`; either `patient` or `row` -- 
#'                     defaults to `row`
#' @param age_groups If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - @min_age: the minimum age for the group (i.e. 10)
#'                     - @max_age: the maximum age for the group (i.e. 20)
#'                     - @group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'                     
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'                     
#'                     If you would not like to stratify by age group, leave the argument as NULL
#' @param time a logical that tells the function whether you would like to look at the output over time
#' @param time_span when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as c(start date, end date) in yyyy-mm-dd date format
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return a dataframe with patient/row counts & proportions for each concept set listed in `evp_concept_file`. 
#'         this output should then be used in the `evp_output` function to generate an appropriate 
#'         visualization
#' 
evp_process_pcnt <- function(cohort,
                            #site_list,
                            evp_concept_file = read_codeset('evp_concepts', 'cccc'),
                            multi_or_single_site = 'single',
                            anomaly_or_exploratory='exploratory',
                            output_level = 'row',
                            age_groups = NULL,
                            time = FALSE,
                            time_span = c('2012-01-01', '2020-01-01'),
                            time_period = 'year'
){
  
  ## parameter summary output
  output_type <- suppressWarnings(param_csv_summ2(check_string = 'evp',
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
  
  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, codeset = NULL) %>%
    group_by(!!! syms(grouped_list))
  
  # Execute function
  for(k in 1:length(site_list_adj)) {
    
    site_list_thisrnd <- site_list_adj[[k]]
    
    # filters by site
    cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))
    
    if(! time) {
      
      if(multi_or_single_site == 'single' && anomaly_or_exploratory == 'anomaly'){
        
        concept_check <- compute_evp_ssanom_pcnt(cohort = cohort_site,
                                                 grouped_list = grouped_list,
                                                 evp_concept_file = evp_concept_file)
        
      }else{
        
        concept_check <- compute_evp_pcnt(cohort = cohort_site,
                                          grouped_list = grouped_list,
                                          evp_concept_file = evp_concept_file,
                                          time = time)
      }
      
      site_output[[k]] <- concept_check
      
    } else if(time){
        
        concept_check <- compute_fot(cohort = cohort_site,
                                     site_col = site_col,
                                     site_list = site_list_adj,
                                     time_span = time_span,
                                     time_period = time_period,
                                     reduce_id = NULL,
                                     check_func = function(dat){
                                       compute_evp_pcnt(cohort = dat,
                                                        grouped_list = grouped_list,
                                                        time = TRUE,
                                                        evp_concept_file = evp_concept_file)
                                     })
    
    site_output[[k]] <- concept_check
    
    }
    
    evp_tbl <- reduce(.x=site_output,
                      .f=dplyr::union)
    
  }
  
  if(time == TRUE && multi_or_single_site == 'multi' && anomaly_or_exploratory == 'anomaly'){
    
    evp_tbl_final <- compute_evp_auc(process_output = evp_tbl,
                                     grp_vars = c('time_start',
                                                  'time_increment',
                                                  'concept_group'),
                                     output_level = output_level)
    
  }else(evp_tbl_final <- evp_tbl)
  
  evp_tbl_final %>%
    replace_site_col() %>%
    output_tbl('evp_process_results', file = TRUE)
  
  return(evp_tbl_final %>% replace_site_col())
  
  message(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                       output function in evp_output: ', output_type, '. This is also included
                       in the parameter_summary.csv file output to the results directory.')))
}

