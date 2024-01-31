

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
                        site_list = c('seattle','cchmc'),
                        domain_tbl=read_codeset('scv_domains', 'cccc'),
                        concept_set = read_codeset('csd_codesets','iccccc'),
                        # dplyr::union(load_codeset('jia_codes','iccccc'),
                        # load_codeset('jia_codes_icd','iccccc')) 
                        #code_type = 'source',
                        #code_domain = 'condition_occurrence',
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = FALSE, #read_codeset('age_group_definitions'),
                        time = FALSE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'
){
  
  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site,
                                 site_list = site_list)
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
      
      domain_compute <- check_code_dist_alt(cohort_codedist = cohort_site,
                                           #code_type = 'cdm',
                                            code_domain = code_domain,
                                            concept_set = concept_set,
                                            domain_tbl = domain_tbl) 
      
      site_output[[k]] <- domain_compute
      
    }
    
    scv_tbl <- reduce(.x=site_output,
                      .f=dplyr::union)
    
  } else if(time){
    ## Do we need a loop here? works because it groups by site as a default, not sure if
    ## its necessary (which one is faster/more efficient)
    if(!is.vector(concept_set)){stop('For an over time output, please select 1-5 codes from your
                                   concept set and include them as a vector in the concept_set argument.')}
    if(is.vector(concept_set) && length(concept_set) > 5){stop('For an over time output, please select 1-5 
                                                              codes from your concept set and include them as
                                                             a vector in the concept_set argument.')}
    
    concept_set_prep <- as.data.frame(concept_set) %>% rename('concept_id' = concept_set) %>%
      mutate(concept_id = as.integer(concept_id))
    concept_set_prep <- copy_to_new(df = concept_set_prep)
    
    scv_tbl <- compute_fot(cohort = cohort_prep,
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
    
  }
  
  
  return(scv_tbl)
  
  
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
csd_output <- function(process_output,
                       output_function,
                       code_type,
                       facet,
                       num_codes = 10,
                       num_mappings = 25,
                       rel_to_median = 'greater',
                       mad_dev = 2,
                       vocab_tbl = vocabulary_tbl('concept'),
                       save_as_png = FALSE,
                       file_path = NULL){
  
  ## Run output functions
  if(output_function == 'scv_ms_anom_nt'){
    scv_output <- scv_ms_anom_nt(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 rel_to_median = rel_to_median,
                                 mad_dev = mad_dev,
                                 vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ss_anom_nt'){
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