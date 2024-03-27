





#' function to take in domain and compute patient facts
#' 
#' @param cohort the cohort for which to iterate 
#' @param pf_input_tbl the tbl that should be iterated 
#' so person facts are computed
#' @param grouped_list a vector to group input by. Defaults to `person_id`,
#' `start_date`, `end_date`, `fu`, `site`
#' @param domain_tbl the config CSV file
#' 
#' @return the column `person_id`, for the specified visit type, 
#' the number of facts for the person - each fact corresponds to the 
#' fact in the CSV file; each column is a domain
#' 

compute_pf_for_fot_pcnt <- function(cohort, 
                                    pf_input_tbl,
                                    grouped_list,
                                    domain_tbl) {
  
  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))
  
  
  for (i in 1:length(domain_list)) {
    
    domain_name = domain_list[[i]][[1]]
    message(paste0('Starting domain ', domain_list[[i]][1]))
    
    ## checks to see if the table needs to be filtered in any way; 
    ## allow for one filtering operation
    if(!is.na(domain_list[[i]][[3]]) && !is.na(domain_list[[i]][[4]])) {
      
      filter_var <- domain_list[[i]][[3]] 
      filter_vec <- as.integer(strsplit(domain_list[[i]][[4]],split=',',fixed = TRUE)[[1]])
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
        filter(!! sym(filter_var) %in% c(filter_vec))
      
    } else if(!is.na(domain_list[[i]][[3]] && is.na(domain_list[[i]][[4]]))){
      
      filter_var <- domain_list[[i]][[3]] 
      samp <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% select(!!sym(filter_var)) %>%
        head(1) %>% collect()
      var_class <- unlist(lapply(samp, class))
      
      if(var_class == 'character'){
        domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
          filter(!!sym(filter_var) != 'NI', !!sym(filter_var) != 'OT',
                 !!sym(filter_var) != 'UN', !is.na(!!sym(filter_var)))
      }else{
        domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
          filter(! is.na(!!sym(filter_var)))
      }
      
    }else{domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]]))}
    
    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    pf <- 
      pf_input_tbl %>% 
      inner_join(select(domain_tbl_use,
                        encounterid)) %>% 
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>% 
      mutate(domain=domain_name) %>% ungroup()
    
    new_group <- grouped_list[! grouped_list %in% c('person_id')]
    
    pf_cohort_final <- 
      pf %>% right_join(select(cohort,
                               patid)) %>% 
      distinct(patid) %>% summarise(ct=n()) %>% pull()
    
    site_visit_ct_num <- 
      pf_input_tbl %>% summarise(ct=n_distinct(patid)) %>% 
      pull()
    
    pf_final <- 
      pf %>% group_by(
        !!! syms(new_group)
      ) %>% group_by(domain, .add = TRUE) %>% 
        summarise(fact_ct_denom=n(),
                  sum_fact_ct=sum(total_strat_ct),
                  median_fact_ct=median(total_strat_ct)) %>% 
      #relocate(site) %>% 
      ungroup() 
    
    
    finalized <- 
      pf_final %>% 
      mutate(pt_ct_denom=pf_cohort_final,
             site_visit_ct=site_visit_ct_num) %>% collect()
    
    
    domain_results[[domain_name]] <- finalized
  }
  
  
  reduce(.x=domain_results,
         .f=dplyr::union)
  # domain_results_left_join <- 
  #   reduce(.x=domain_results,
  #          .f=left_join)
}
