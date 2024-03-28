
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

compute_pf_pcnt <- function(cohort, 
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
      filter_vec <- strsplit(domain_list[[i]][[4]],split=',',fixed = TRUE)[[1]]
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
        filter(!! sym(filter_var) %in% c(filter_vec))
      
    } else if(!is.na(domain_list[[i]][[3]]) && is.na(domain_list[[i]][[4]])){
      
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
    ## assumes patid is part of named list
    pf <- 
      pf_input_tbl %>% 
      inner_join(select(domain_tbl_use,
                        encounterid)) %>% 
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>% 
      ungroup() %>% 
      mutate(domain=domain_name) %>% 
      mutate(fact_ct_strat=round(total_strat_ct/fu,2)) %>% 
      select(-total_strat_ct) %>% 
      select(patid,
             domain,
             fact_ct_strat) %>%
      pivot_wider(names_from=domain,
                  values_from=fact_ct_strat) %>% 
      right_join(cohort) %>%
      relocate(patid) %>%
      compute_new(indexes=list('patid'))
    
    domain_results[[domain_name]] <- pf
  }
  
  domain_results_left_join <- 
    reduce(.x=domain_results,
           .f=left_join)
}