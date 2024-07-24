
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

compute_pf <- function(cohort, 
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
    if(! is.na(domain_list[[i]][[3]])) {
      filter_var <- domain_list[[i]][[3]] 
      filter_vec <- as.integer(strsplit(domain_list[[i]][[4]],split=',',fixed = TRUE)[[1]])
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
        filter(!! sym(filter_var) %in% c(filter_vec))
    } else {domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]]))}
    
    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    pf <- 
      pf_input_tbl %>% 
      inner_join(select(domain_tbl_use,
                        visit_occurrence_id)) %>% 
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>% 
      ungroup() %>% 
      mutate(domain=domain_name) %>% 
      mutate(k_mult = case_when(fu < 0.1 ~ 100,
                                fu >= 0.1 & fu < 1 ~ 10,
                                TRUE ~ 1),
             fact_ct_strat=ifelse(fu != 0,round(total_strat_ct/(fu * k_mult),2),0)) %>% 
      #select(-c(total_strat_ct, k_mult)) %>% 
      select(person_id,
             domain,
             fact_ct_strat) %>%
      pivot_wider(names_from=domain,
                  values_from=fact_ct_strat) %>% 
      right_join(cohort) %>%
      relocate(person_id) %>%
      compute_new(indexes=list('person_id'))
    
    domain_results[[domain_name]] <- pf
  }
  
  domain_results_left_join <- 
    reduce(.x=domain_results,
           .f=left_join)
}


#' function to take in domain and compute patient facts
#' 
#' @param cohort the cohort for which to iterate 
#' @param pf_input_tbl the tbl that should be iterated 
#' so person facts are computed
#' @param grouped_list a vector to group input by. Defaults to `person_id`,
#' `start_date`, `end_date`, `fu`, `site`
#' @param domain_tbl a CSV file with configuration information about each
#'                   domain of interest
#' 
#' @return the column `person_id`, for the specified visit type, 
#' the number of facts for the person - each fact corresponds to the 
#' fact in the CSV file; each column is a domain
#' 

compute_pf_for_fot <- function(cohort, pf_input_tbl,
                               grouped_list,
                               domain_tbl) {
  
  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))
  
  
  for (i in 1:length(domain_list)) {
    
    domain_name = domain_list[[i]][[1]]
    message(paste0('Starting domain ', domain_list[[i]][1]))
    
    ## checks to see if the table needs to be filtered in any way; 
    ## allow for one filtering operation
    if(! is.na(domain_list[[i]][[3]])) {
      filter_var <- domain_list[[i]][[3]] 
      filter_vec <- as.integer(strsplit(domain_list[[i]][[4]],split=',',fixed = TRUE)[[1]])
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
        filter(!! sym(filter_var) %in% c(filter_vec))
    } else {domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]]))}
    
    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    pf <- 
      pf_input_tbl %>% 
      inner_join(select(domain_tbl_use,
                        visit_occurrence_id)) %>% 
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>% 
      mutate(domain=domain_name) %>% ungroup()
    
    new_group <- grouped_list[! grouped_list %in% c('person_id')]
    
    pf_cohort_final <- 
      pf %>% right_join(select(cohort,
                               person_id)) %>% 
      distinct(person_id) %>% summarise(ct=n()) %>% pull()
    
    site_visit_ct_num <- 
      pf_input_tbl %>% summarise(ct=n_distinct(person_id)) %>% 
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


#' combine PF output into one table per study
#'
#' @param pf_tbl the table output by `compute_pf` or `compute_pf_for_fot`
#' @param study_abbr string that corresponds with the study/studies of interest
#' @param visit_type_list the types of visits in the PF output; defaults to:
#'                        inpatient, outpatient, other_visit, all
#' @param domain_list a CSV file with configuration information about each
#'                    domain of interest
#' @param time a logical indicating whether the analysis was conducted over time
#'
#' @return a list of dataframes that contain all sites and all domains for each study
#'         listed in `study_abbr`. the resulting dataframes will also have an
#'         age grouping column added.
#'         
#' 
combine_study_facts <- function(pf_tbl,
                                study_abbr,
                                domain_list,
                                time = FALSE,
                                visit_type_list = list('inpatient','outpatient',
                                                       'other_visit','all')) {
  final_list <- list()
  
  pf_visits <- 
    str_remove(names(pf_tbl),'(pf_)')
  
  names(pf_tbl) <- str_remove(names(pf_tbl), '(pf_)')
  
  for(i in 1:length(visit_type_list)) {
    
    possible_cols <-  domain_list %>% 
      select(domain) %>% c()
    
    visit_type_pulled <- 
      paste0(visit_type_list[[i]])
    
    pf_tbl_visittype <- 
      pf_tbl[[visit_type_pulled]]
    
    # visit_type_pulled <- 
    #   get_results(paste0(visit_type_list[[i]]))  %>% 
    #   select(-any_of('fact_ct_strat')) 
    
    tbl_cols <- pf_tbl_visittype %>% colnames()
    
    selected_cols <- 
      intersect(possible_cols[[1]],
                tbl_cols)
    
    if(length(selected_cols) == 0){
      
      mutated_tbl <- NULL
    
      }else{
    
        if(!time){
        mutated_tbl <- 
          pf_tbl_visittype %>% 
          pivot_longer(cols=all_of(selected_cols),
                       names_to='domain',
                       #names_to='var_name',
                       values_to='var_val') %>%
          mutate(var_ever=case_when(!is.na(var_val)~1L,
                                    TRUE~0L)) %>% 
          mutate(var_val=case_when(is.na(var_val) ~ 0,
                                    TRUE ~ var_val)) %>% 
           mutate(study=study_abbr,
                  visit_type=visit_type_pulled)
        } else {mutated_tbl <- pf_tbl_visittype %>% mutate(study=study_abbr,
                                                     visit_type=visit_type_pulled)}
        
      }
    
    
    final_list[[i]] <- mutated_tbl
    
  }
  
  final_list_reduce <- reduce(.x=final_list,
                              .f=dplyr::union)
  final_list_reduce
  
}
