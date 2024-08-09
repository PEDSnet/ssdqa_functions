

#' CSD Process function 
#' 
#' add csv output of main table and csv output of param summary 
#' (that function needs updating to be more generalizable)
#'
#' @param cohort cohort for SSDQA testing; required fields: 
#'               `site` | `person_id` | `start_date` | `end_date` where start and end date 
#' @param domain_tbl tbl that is similar to the SCV check; 
#'                   four columns: `domain` | `source_col` | `concept_col` | `date_col`;
#'                   the required columns for the csd check are only `domain_tbl`, `concept_col`, `date_col`
#' @param concept_set concept set CSV file with the following columns:
#'                    `concept_id` | `concept_code` | `concept_name` | `vocabulary_id` | `category` | `variable` | `domain`
#'                    The variable field is required to categorize each concept set into a particular variable
#'                    The domain is required so that the function knows which table to join to in order to derive counts
#' @param multi_or_single_site direction to determine what kind of check to run
#'                             string that is either `multi` or `single`
#' @param anomaly_or_exploratory direction to determine what kind of check to run; a string 
#'                               that is either `anomaly` or `exploratory`
#' @param num_concept_combined when `mult_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#'                             this argument is an integer and will ensure that `concept1` and `concept2` meet
#'                             some minimal threshold for including in the jaccard index; if `TRUE`, then 
#'                             *both* conditions for `num_concept_1` and `num_concept_2` should be met;
#'                             if `FALSE` then just one condition needs to be met.
#' @param num_concept_1  when `mult_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#'                             this argument is an integer and requires a minimum number of times that 
#'                             the *first* concept appears in the dataset
#' @param num_concept_2 when `mult_or_single_site` = `single` and `anomaly_or_exploratory` = `anomaly`,
#'                             this argument is an integer and requires a minimum number of times that 
#'                             the *second* concept appears in the dataset
#' @param p_value the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param age_groups If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - @min_age: the minimum age for the group (i.e. 10)
#'                     - @max_age: the maximum age for the group (i.e. 20)
#'                     - @group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'                     
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'                     
#'                     If you would not like to stratify by age group, leave the argument as NULL
#' @param time logical to determine whether to output the check across time
#' @param time_span when `time = TRUE`, a vector of two dates for the observation period of the study
#' @param time_period when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#'
#' @return for `single- and multi-` and `exploratory` analyses, the output is:
#'             `site` | `variable` | `ct_denom` | `concept_id` | `ct_concept` | `prop_concept`
#'         for `single` and `anomaly` analyses, the output is: 
#'             `site` | `concept1`| `concept2`| `cocount`| `concept1_ct` | `concept2_ct` | `concept_count_union` |`jaccard_index` |
#'             `concept1_prop` | `concept2_prop` | `variable` | 
#'             where `concept_count_union` is how often a pair of codes appear in the full dataset
#'             and `concept1_ct` and `concept2_ct` are how often each appear in the dataset
#'         for any that are `across time`, the output is:
#'             `site` | `time_start` | `time_increment` | `variable` | `ct_denom` | `concept_id` | `ct_concept` | `prop_concept`
#' 
#' 
csd_process <- function(cohort = results_tbl('jspa_cohort'),
                        domain_tbl=read_codeset('scv_domains', 'cccc'),
                        concept_set = read_codeset('csd_codesets_jspa','iccccc'), 
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        num_concept_combined = FALSE,
                        num_concept_1 = 30,
                        num_concept_2 = 30,
                        p_value = 0.9,
                        age_groups = FALSE, #read_codeset('age_group_definitions'),
                        time = TRUE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'
){
  
  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
                                 #site_list = site_list)
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
    #mutate(domain = code_domain) %>% 
    group_by(!!! syms(grouped_list))
  
  # Execute function
  if(! time) {
    
    
    for(k in 1:length(site_list_adj)) {
      
      site_list_thisrnd <- site_list_adj[[k]]
      
      # filters by site
      cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))
      
      if(multi_or_single_site=='single' & anomaly_or_exploratory=='anomaly') {
        variable_compute <- check_code_dist_ssanom(cohort_codedist = cohort_site,
                                                   concept_set = concept_set,
                                                   domain_tbl = domain_tbl,
                                                   num_concept_combined = num_concept_combined,
                                                   num_concept_1 = num_concept_1,
                                                   num_concept_2 = num_concept_2)
      } else {
        variable_compute <- check_code_dist_csd(cohort_codedist = cohort_site,
                                                concept_set = concept_set,
                                                time = time,
                                                time_span = time_span,
                                                time_period = time_period,
                                                domain_tbl = domain_tbl) 
      }
      
      
      site_output[[k]] <- variable_compute %>% mutate(site=site_list_thisrnd)
      
    }
    
    csd_tbl <- reduce(.x=site_output,
                      .f=dplyr::union) #%>% mutate(site=site_list_thisrnd)
    
    if(multi_or_single_site == 'multi' & anomaly_or_exploratory == 'anomaly'){
      
      csd_tbl_int <- compute_dist_anomalies(df_tbl = csd_tbl,
                                            grp_vars = c('variable', 'concept_id'), 
                                            var_col = 'prop_concept',
                                            denom_cols = c('variable', 'ct_denom')) 
      
      csd_tbl_final <- detect_outliers(df_tbl = csd_tbl_int,
                                       tail_input = 'both',
                                       p_input = p_value,
                                       column_analysis = 'prop_concept',
                                       column_variable = 'concept_id')
      
    }else{csd_tbl_final <- csd_tbl}
    
  } else {
   
    
    cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, codeset = NULL) %>% 
      #mutate(domain = code_domain) %>% 
      group_by(!!! syms(grouped_list))
    
    csd_tbl <- compute_fot(cohort = cohort_prep,
                           site_list = site_list_adj,
                           site_col = site_col,
                           time_span = time_span,
                           time_period = time_period,
                           reduce_id = NULL,
                           check_func = function(dat){
                             check_code_dist_csd(cohort_codedist = dat,
                                             concept_set = concept_set,
                                             #code_type = code_type,
                                             #code_domain = code_domain,
                                             domain_tbl = domain_tbl,
                                             time = TRUE)
                           })
    
    if(multi_or_single_site == 'multi' & anomaly_or_exploratory=='anomaly') {
      
      lookup <- csd_tbl %>% ungroup() %>% distinct(variable, concept_id)
      
      csd_tbl_ms <- ms_anom_euclidean(fot_input_tbl = csd_tbl,
                                      grp_vars = c('site', 'concept_id'),
                                      var_col = 'prop_concept')
      
      csd_tbl_final <- csd_tbl_ms %>% left_join(lookup)
      
    }else if(multi_or_single_site == 'single' & anomaly_or_exploratory=='anomaly'){
      
      csd_tbl_ss <- anomalize_ss_anom_at(fot_input_tbl = csd_tbl,
                                         grp_vars = 'concept_id',
                                         time_var = 'time_start',
                                         var_col = 'prop_concept')
      
      csd_tbl_final <- csd_tbl_ss
      
    }else{csd_tbl_final <- csd_tbl}
    
  }
  
  return(csd_tbl_final %>% replace_site_col())
  
  
}


#' CSD Output
#'
#' @param process_output the output from `csd_process`
#' @param output_function the name of the output function that should be used provided in the `parameter_summary` csv 
#'                        file that is output to the provided results folder after running the `csd_process` function
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip 
#' @param num_variables an integer to represent the top number of variables to include for the exploratory analyses;
#'                      will pick based on the most commonly appearing variables; 
#' @param num_mappings an integer to represent the top number of mappings for a given variable in the exploratory analyses;
#'                     will pick based on the highest count of the most commonly appearing variables;
#' @param filtered_var for both `single- and multi- site anomaly tests without time measurements` and 
#'                     `single- and multi- site exploratory tests with time measurements`, the variables
#'                     to focus on
#' @param filter_concept for @ss_anom_at, @ms_exp_at, and @ms_anom_at, the specific code that should
#'                       be the focus of the analysis
#' @param facet variables to facet by; defaults to NULL
#' @param text_wrapping_char an integer to limit the length of text on an axis before wrapping is enforced;
#'                           used in @ms_anom_nt
#' @param output_value the column in `process_output` that should be used in the visualization
#'                     relevant for @ss_exp_at, @ms_anom_nt, and @ms_exp_at
#' 
#' @return 
#' 
csd_output <- function(process_output=process_output,
                       output_function,
                       concept_set = read_codeset('csd_codesets_jspa','iccccc'),
                       vocab_tbl = vocabulary_tbl('concept'),
                       num_variables = 10,
                       num_mappings = 10,
                       filtered_var = 'general_jia',
                       filter_concept = 81893,
                       facet=NULL,
                       text_wrapping_char = 80,
                       output_value = 'prop_concept'){
  
  ## check concept col
  concept_col <- ifelse('concept_id' %in% colnames(process_output), 'concept_id', 'concept_code')
  
  if('concept_id' %in% colnames(process_output)){
    process_output <- process_output %>% mutate(concept_id = as.integer(concept_id))
  }else{process_output <- process_output}
  
  ## Get concept names from vocabulary table
  if(output_function != 'csd_ss_anom_nt'){
    process_output <- join_to_vocabulary(tbl = process_output %>% 
                                           inner_join(select(concept_set, concept_id, concept_code,
                                                             vocabulary_id)), 
                                         vocab_tbl = vocab_tbl,
                                         col = concept_col,
                                         vocab_col = concept_col)
  }
  
  ## Run output functions
  if(output_function == 'csd_ss_exp_nt'){
    csd_output <- csd_ss_exp_nt(process_output=process_output,
                                concept_col = concept_col,
                                num_codes = num_variables,
                                num_mappings = num_mappings)
  }else if(output_function == 'csd_ss_anom_nt'){
    csd_output <- csd_ss_anom_nt(process_output,
                                 vocab_tbl = vocab_tbl,
                                 filtered_var = filtered_var)
  }else if(output_function == 'csd_ss_exp_at'){
    csd_output <- csd_ss_exp_at(process_output,
                                concept_col = concept_col,
                                facet=facet,
                                filtered_var = filtered_var,
                                num_mappings = num_mappings,
                                output_value=output_value)
  }else if(output_function == 'csd_ss_anom_at'){
    csd_output <- csd_ss_anom_at(process_output=process_output,
                                 concept_col = concept_col,
                                 filter_concept = filter_concept,
                                 filtered_var=filtered_var,
                                 facet=facet)
  }else if(output_function == 'csd_ms_exp_nt'){
    csd_output <- csd_ms_exp_nt(process_output=process_output,
                                concept_col = concept_col,
                                 facet=facet,
                                 num_codes = num_variables)
  }else if(output_function == 'csd_ms_anom_nt'){
    csd_output <- csd_ms_anom_nt(process_output=process_output,
                                 concept_col = concept_col,
                                 text_wrapping_char=text_wrapping_char,
                                 filtered_var=filtered_var,
                                 comparison_col=output_value)
  }else if(output_function == 'csd_ms_exp_at'){
    csd_output <- csd_ms_exp_at(process_output = process_output,
                                concept_col = concept_col,
                                filtered_var = filtered_var,
                                filtered_concept = filter_concept,
                                output_value = output_value,
                                facet = facet
                                )
  }else if(output_function == 'csd_ms_anom_at'){
    csd_output <- csd_ms_anom_at(process_output=process_output,
                                 concept_col = concept_col,
                                 filter_concept=filter_concept)
  }else(cli::cli_abort('Please enter a valid output_function for this check'))
  
  return(csd_output)
  
}