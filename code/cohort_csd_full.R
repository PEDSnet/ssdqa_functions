

#' SCV Process function 
#' 
#' add csv output of main table and csv output of param summary 
#' (that function needs updating to be more generalizable)
#'
#' @param cohort 
#' @param site_list 
#' @param domain_tbl 
#' @param concept_set 
#' @param code_type 
#' @param code_domain 
#' @param multi_or_single_site 
#' @param anomaly_or_exploratory 
#' @param age_groups 
#' @param time 
#' @param time_span 
#' @param time_period 
#'
#' @return
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
                                                   code_domain=code_domain,
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
                                             code_domain = code_domain,
                                             domain_tbl = domain_tbl,
                                             time = TRUE)
                           })
    
  }
  
  final_csd_tbl <- 
    replace_site_col(csd_tbl)
  
  return(final_csd_tbl)
  
  
}


#' full output function (needs some updating)
#'
#' @param process_output 
#' @param output_function 
#' @param code_type 
#' @param facet 
#' @param num_codes 
#' @param num_mappings 
#' @param rel_to_median 
#' @param mad_dev 
#' @param vocab_tbl 
#' @param save_as_png 
#' @param file_path 
#'
#' @return
#' 
csd_output <- function(process_output=process_output,
                       num_concept_combined = FALSE,
                       vocab_tbl = vocabulary_tbl('concept'),
                       num_codes = 10,
                       num_mappings = 10,
                       filtered_var = 'general_jia',
                       save_as_png = FALSE,
                       file_path = NULL){
  
  ## Run output functions
  if(output_function == 'csd_ss_exp_nt'){
    scv_output <- scv_ms_anom_nt(process_output=process_output,
                                 num_concept_combined = num_concept_combined,
                                 vocab_tbl = vocab_tbl,
                                 num_codes = num_codes,
                                 num_mappings = num_mappings,
                                 filtered_var = 'general_jia')
  }else if(output_function == 'csd_ss_'){
    scv_output <- scv_ss_anom_nt(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 rel_to_median = rel_to_median,
                                 vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ms_exp_nt'){
    scv_output <- scv_ms_exp_nt(process_output = process_output,
                                code_type = code_type,
                                facet = facet,
                                num_codes = num_codes,
                                vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ss_exp_nt'){
    scv_output <- scv_ss_exp_nt(process_output = process_output,
                                code_type = code_type,
                                facet = facet,
                                num_codes = num_codes,
                                num_mappings = num_mappings,
                                vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ms_anom_at'){
    scv_output <- scv_ms_anom_at(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 mad_dev = mad_dev,
                                 vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ss_anom_at'){
    scv_output <- scv_ss_anom_at(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet)
  }else if(output_function == 'scv_ms_exp_at'){
    scv_output <- scv_ss_ms_exp_at(process_output = process_output,
                                   code_type = code_type,
                                   facet = facet,
                                   vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ss_exp_at'){
    scv_output <- scv_ss_ms_exp_at(process_output = process_output,
                                   code_type = code_type,
                                   facet = facet,
                                   vocab_tbl = vocab_tbl)
  }
  
  return(scv_output)
  
}