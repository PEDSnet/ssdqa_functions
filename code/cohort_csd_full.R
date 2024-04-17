

#' SCV Process function 
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
#' @param age_groups N/A for this check?
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
                        concept_set = read_codeset('csd_codesets','iccccc'), #%>% 
                          #filter(variable %in% c('ibd', 'spondyloarthritis', 'systemic_jia', 'uveitis', 'general_jia')),
                        # dplyr::union(load_codeset('jia_codes','iccccc'),
                        # load_codeset('jia_codes_icd','iccccc')) 
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        num_concept_combined = FALSE,
                        num_concept_1 = 30,
                        num_concept_2 = 30,
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
                                              #code_type = 'cdm',
                                              code_domain = code_domain,
                                              concept_set = concept_set,
                                              domain_tbl = domain_tbl) 
      }
      
      
      site_output[[k]] <- variable_compute %>% mutate(site=site_list_thisrnd)
      
    }
    
    csd_tbl <- reduce(.x=site_output,
                      .f=dplyr::union) #%>% mutate(site=site_list_thisrnd)
    
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
      
      csd_tbl_ms <- csd_ms_anom_euclidean(input_tbl = csd_tbl,
                                          time_period = time_period)
      
      csd_tbl <- csd_tbl_ms
      
    }
    
  }
  
  final_csd_tbl <- 
    replace_site_col(csd_tbl)
  
  return(final_csd_tbl)
  
  
}


#' full output function (needs some updating)
#'
#' @param process_output the output from `csd_process`
#' @param num_codes an integer to represent the top number of codes to include in the mappings for the exploratory analyses;
#'                  will pick the codes based on the highest count of the most commonly appearing variables; 
#' @param num_mappings an integer to represent the top number of mappings for a given variable in the exploratory analyses
#' @param filtered_var for both `single- and multi- site anomaly tests without time measurements` and 
#'                     `single- and multi- site exploratory tests with time measurements`, the variables
#'                     to focus on
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' @param save_as_png 
#' @param file_path 
#' @param facet variables to facet by; defaults to NULL
#' @return
#' 
csd_output <- function(process_output=process_output,
                       output_function,
                       vocab_tbl = vocabulary_tbl('concept'),
                       num_codes = 10,
                       num_mappings = 10,
                       filtered_var = 'general_jia',
                       filter_concept = 81893,
                       facet=NULL,
                       text_wrapping_char = 80,
                       comparison_col = 'prop_concept',
                       grouped_vars = c('variable', 'concept_id'),
                       output_value = 'prop_concept',
                       save_as_png = FALSE,
                       file_path = NULL){
  
  ## Get concept names from vocabulary table
  if(output_function != 'csd_ss_anom_nt'){
    process_output <- join_to_vocabulary(tbl = process_output %>% 
                                           mutate(concept_id = as.integer(concept_id)),
                                         vocab_tbl = vocab_tbl,
                                         col = 'concept_id')
  }
  
  ## Run output functions
  if(output_function == 'csd_ss_exp_nt'){
    csd_output <- csd_ss_exp_nt(process_output=process_output,
                                 #vocab_tbl = vocab_tbl,
                                 num_codes = num_codes,
                                 num_mappings = num_mappings)
  }else if(output_function == 'csd_ss_anom_nt'){
    csd_output <- csd_ss_anom_nt(process_output,
                                 vocab_tbl = vocab_tbl,
                                 filtered_var = filtered_var)
  }else if(output_function == 'csd_ss_exp_at'){
    csd_output <- csd_ss_exp_at(process_output,
                                facet=facet,
                                filtered_var = filtered_var,
                                #vocab_tbl = vocab_tbl,
                                output_value=output_value)
  }else if(output_function == 'csd_ss_anom_at'){
    csd_output <- csd_ss_anom_at(process_output=process_output,
                                filter_concept = filter_concept,
                                filtered_var=filtered_var,
                                facet=facet,
                                top_mapping_n = num_mappings
                                )
  }else if(output_function == 'csd_ms_exp_nt'){
    csd_output <- csd_ms_exp_nt(process_output=process_output,
                                 facet=facet,
                                 #vocab_tbl = vocab_tbl,
                                 num_codes = num_codes)
  }else if(output_function == 'csd_ms_anom_nt'){
    csd_output <- csd_ms_anom_nt(process_output=process_output,
                                   #vocab_tbl=vocab_tbl,
                                   text_wrapping_char=text_wrapping_char,
                                   filtered_var=filtered_var,
                                   comparison_col=comparison_col,
                                   grouped_vars=grouped_vars)
  }else if(output_function == 'csd_ms_exp_at'){
    csd_output <- csd_ms_exp_at(process_output = process_output,
                                filtered_var = filtered_var,
                                filtered_concept = filter_concept,
                                output_value = output_value,
                                facet = facet
                                #vocab_tbl = vocab_tbl
                                )
  }else if(output_function == 'csd_ms_anom_at'){
    csd_output <- csd_ms_anom_at(process_output=process_output,
                                 filter_concept=filter_concept)
  }else(stop('Please enter a valid output_function for this check'))
  
  return(csd_output)
  
}