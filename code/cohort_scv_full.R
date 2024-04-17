
#' SCV Process function 
#' 
#' add csv output of main table and csv output of param summary 
#' (that function needs updating to be more generalizable)
#'
#' @param cohort A dataframe with the cohort of patients for your study. Should include the columns:
#'                       - @person_id
#'                       - @start_date
#'                       - @end_date
#'                       - @site
#' @param site_list A list of sites for which you would like to examine clinical facts. Can be one site 
#'                  (single-site) or multiple (multi-site) 
#' @param concept_set for analyses where time = FALSE, a csv file with the source or cdm codes of interest for the analysis.
#'                    should contain at least a `concept_id` column
#'                    
#'                    for analyses where time = TRUE, a vector with up to 5 source or cdm codes of interest for the analysis.
#' @param domain_tbl a csv file that defines the domains where facts should be identified. defaults to the provided
#'                     `scv_domains.csv` file, which contains the following fields:
#'                     - @domain: the CDM table where information for this domain can be found (i.e. drug_exposure)
#'                     - @source_col: the column in the CDM table where `source` codes can be identified (i.e. drug_source_concept_id)
#'                     - @concept_col: the column in the CDM table where `cdm` codes can be identified (i.e. drug_concept_id)
#'                     - @date_col: the column in the CDM table that should be used as the default date field for
#'                                  over time analyses (i.e. drug_exposure_start_date)
#' @param code_type the type of code that is being used in the analysis, either `source` or `cdm`
#' @param code_domain the domain where the codes in the concept set should be searched for; must match
#'                    a domain defined in @domain_tbl
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#'                               - @single - run the function for a single site
#'                               - @multi - run the function for multiple sites
#' @param anomaly_or_exploratory Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                               level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                               analyses are specialized to identify outliers within the cohort.
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
#' @return a dataframe with counts and proportions for each source -> cdm or cdm -> source mapping
#'         pair for each of the codes provided in @concept_set. this output should then be used in 
#'         the `scv_output` function to generate an appropriate visualization
#' 
scv_process <- function(cohort,
                        #site_list,
                        concept_set,
                        domain_tbl=read_codeset('scv_domains', 'cccc'),
                        code_type = 'source',
                        code_domain = 'condition_occurrence',
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = NULL,
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'
){
  
  ## parameter summary output
  output_type <- suppressWarnings(param_csv_summ2(check_string = 'scv',
                                                  as.list(environment())))
  
  
  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  
  # Set up grouped list
  
  grouped_list <- grouped_list %>% append('domain')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  
  site_output <- list()
  
  # Prep cohort
  
  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, codeset = NULL) %>% 
    mutate(domain = code_domain) %>% 
    group_by(!!! syms(grouped_list))
  
  # Execute function
  if(! time) {
    
    for(k in 1:length(site_list_adj)) {
      
      site_list_thisrnd <- site_list_adj[[k]]
      
      # filters by site
      cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))
      
      domain_compute <- check_code_dist(cohort = cohort_site,
                                        code_type = code_type,
                                        code_domain = code_domain,
                                        concept_set = concept_set,
                                        domain_tbl = domain_tbl) 
      
      site_output[[k]] <- domain_compute
      
    }
    
    scv_tbl_final <- reduce(.x=site_output,
                            .f=dplyr::union)
    
  } else if(time){
    if(!is.vector(concept_set)){stop('For an over time output, please select 1-5 codes from your
                                   concept set and include them as a vector in the concept_set argument.')}
    if(is.vector(concept_set) && length(concept_set) > 5){stop('For an over time output, please select 1-5 
                                                              codes from your concept set and include them as
                                                             a vector in the concept_set argument.')}
    
    concept_set_prep <- as.data.frame(concept_set) %>% rename('concept_id' = concept_set) %>%
      mutate(concept_id = as.integer(concept_id))
    concept_set_prep <- copy_to_new(df = concept_set_prep)
    
    scv_tbl <- compute_fot(cohort = cohort_prep,
                           site_col = site_col,
                           site_list = site_list_adj,
                           time_span = time_span,
                           time_period = time_period,
                           reduce_id = NULL,
                           check_func = function(dat){
                             check_code_dist(cohort = dat,
                                             concept_set = concept_set_prep,
                                             code_type = code_type,
                                             code_domain = code_domain,
                                             domain_tbl = domain_tbl,
                                             time = TRUE)
                           })
    
    if(multi_or_single_site == 'multi' && anomaly_or_exploratory == 'anomaly'){
      
      var_col <- ifelse(code_type == 'cdm', 'concept_prop', 'source_prop')
      
      scv_tbl_final <- ms_anom_euclidean(fot_input_tbl = scv_tbl,
                                         grp_vars = c('site', 'concept_id', 'source_concept_id'),
                                         var_col = var_col)
      
    }else if(multi_or_single_site == 'single' && anomaly_or_exploratory == 'anomaly'){
      
      var_col <- ifelse(code_type == 'cdm', 'concept_id', 'source_concept_id')
      
      n_mappings_time <- scv_tbl %>%
        group_by(!!sym(var_col), time_start, time_increment) %>%
        summarise(n_mappings = n())
      
      scv_tbl_final <- anomalize_ss_anom_at(fot_input_tbl = n_mappings_time,
                                            time_var = 'time_start',
                                            grp_vars = var_col,
                                            var_col = 'n_mappings')
      
    }else{(scv_tbl_final <- scv_tbl)}
    
  }
  
  scv_tbl_final %>%
    replace_site_col() %>%
    output_tbl('scv_process_results', file = TRUE)
  
  message(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                       output function in scv_output: ', output_type, '. This is also included
                       in the parameter_summary.csv file output to the results directory.')))
  
  return(scv_tbl_final %>% replace_site_col())
}


#' SCV Output Generation
#'
#' @param process_output the output of the `scv_process` function
#' @param output_function the name of the output function that should be used provided in the `parameter_summary` csv 
#'                        file that is output to the provided results folder after running the `scv_process` function 
#' @param code_type the type of code that is being used in the analysis, either `source` or `cdm`
#'                  
#'                  should ideally match the code_type that was defined when running `scv_process`
#' @param facet the variables by which you would like to facet the graph. available and/or recommended options for
#'              faceting variables are provided in the `parameter_summary` csv file
#' @param filter_concept for `scv_ms_anom_at` only -- choose ONE concept_id from the concept_set provided in 
#'                       `scv_process` to filter the output
#' @param filter_mapped for `scv_ms_anom_at` only -- choose ONE mapped concept from those associated with the
#'                      concept_id provided in @filter_concept; options can be found in the `mapped_id` column
#'                      of `scv_process`                   
#' @param num_codes the number of top codes of code_type that should be displayed in the analysis
#' 
#'                  used for `ss_exp_nt` and `ms_exp_nt`
#' @param num_mappings the number of top mappings that should be displayed for each code of code_type
#' 
#'                     used for `ss_exp_nt`
#' @param rel_to_median for output types that use a median, an option to select whether values `greater`
#'                      or `less` than the median should be displayed. both options will also display
#'                      values equal to the median
#'                      
#'                      used for `ms_anom_nt` and `ss_anom_nt`
#' @param mad_dev for `ms_anom_at`, the median absolute deviation that should be used as a threshold to
#'                identify anomalous/unstable mappings
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#'
#' @return a graph to visualize the results from `scv_process` based on the parameters provided
#'          
#'         in some cases, an additional reference table with summary information about the codes
#'         included in the graph
#' 
scv_output <- function(process_output,
                       output_function,
                       code_type,
                       facet,
                       filter_concept = NULL,
                       filter_mapped = NULL,
                       num_codes = 10,
                       num_mappings = 25,
                       rel_to_median = 'greater',
                       mad_dev = 2,
                       vocab_tbl = vocabulary_tbl('concept')){
  
  if(output_function != 'scv_ss_anom_at'){
  rslt_cid <- join_to_vocabulary(tbl = process_output,
                                 vocab_tbl = vocab_tbl,
                                 col = 'concept_id')
  
  rslt_scid <- join_to_vocabulary(tbl = process_output,
                                  vocab_tbl = vocab_tbl,
                                  col = 'source_concept_id') %>%
    rename('source_concept_name' = 'concept_name')
  
  process_output <- rslt_cid %>%
    full_join(rslt_scid)
  
  }else{
    process_output <- process_output
  }
  
  ## Run output functions
  if(output_function == 'scv_ms_anom_nt'){
    scv_output <- scv_ms_anom_nt(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 rel_to_median = rel_to_median)
  }else if(output_function == 'scv_ss_anom_nt'){
    scv_output <- scv_ss_anom_nt(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 rel_to_median = rel_to_median)
  }else if(output_function == 'scv_ms_exp_nt'){
    scv_output <- scv_ms_exp_nt(process_output = process_output,
                                code_type = code_type,
                                facet = facet,
                                num_codes = num_codes)
  }else if(output_function == 'scv_ss_exp_nt'){
    scv_output <- scv_ss_exp_nt(process_output = process_output,
                                code_type = code_type,
                                facet = facet,
                                num_codes = num_codes,
                                num_mappings = num_mappings)
  }else if(output_function == 'scv_ms_anom_at'){
    scv_output <- scv_ms_anom_at(process_output = process_output,
                                 code_type = code_type,
                                 filter_concept = filter_concept,
                                 filter_mapped = filter_mapped)
  }else if(output_function == 'scv_ss_anom_at'){
    scv_output <- scv_ss_anom_at(process_output = process_output,
                                 code_type = code_type,
                                 filter_concept = filter_concept,
                                 facet = facet)
  }else if(output_function == 'scv_ms_exp_at'){
    scv_output <- scv_ss_ms_exp_at(process_output = process_output,
                                   code_type = code_type,
                                   facet = facet)
  }else if(output_function == 'scv_ss_exp_at'){
    scv_output <- scv_ss_ms_exp_at(process_output = process_output,
                                   code_type = code_type,
                                   facet = facet)
  }else(stop('Please enter a valid output function for this check type.'))
  
  return(scv_output)
  
}