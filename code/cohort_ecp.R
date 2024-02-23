

#' ECP Base Function
#'
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param grouped_list list of columns that should be used to group the table
#' @param time logical to determine whether the function is being run as part of `compute_fot` or not
#' @param ecp_concept_file CSV file with information about each of the concept sets that should be
#'                         examined in the function. contains the following columns:
#'                         
#'                         `concept_group`, `default_tbl`, `field_name`, `date_field`, `codeset_name`
#'
#' @return dataframe with patient/row counts and proportions that are computed per group defined in
#'         grouped_list, and if time = TRUE, for each time period defined in compute_fot
#' 
compute_ecp <- function(cohort,
                        grouped_list,
                        time = FALSE,
                        ecp_concept_file = read_codeset('ecp_concepts', 'cccc')){
  
  ecp_list <- split(ecp_concept_file, seq(nrow(ecp_concept_file)))
  
  result <- list()
  
  for(i in 1:length(ecp_list)){
    
    concept_group <- ecp_list[[i]][[1]]
    
    message(paste0('Starting ', concept_group))
    
    domain_tbl <- cdm_tbl(ecp_list[[i]][[2]]) %>%
      inner_join(cohort) %>%
      group_by(!!!syms(grouped_list))
    
    if(time){
      domain_tbl <- domain_tbl %>% 
        filter(!!sym(ecp_list[[i]][[4]]) >= start_date &
                 !!sym(ecp_list[[i]][[4]]) <= end_date) %>%
        group_by(time_start, time_increment, .add = TRUE)
      }
    
    total_pts <- domain_tbl %>%
      summarise(total_pt_ct = n_distinct(person_id),
                total_row_ct = n()) %>%
      collect()
    
    join_cols <- set_names('concept_id', ecp_list[[i]][[3]])
    
    fact_pts <- domain_tbl %>%
      inner_join(load_codeset(ecp_list[[i]][[5]]), by = join_cols) %>%
      summarise(concept_pt_ct = n_distinct(person_id),
                concept_row_ct = n()) %>% collect()
    
    final_tbl <- total_pts %>%
      left_join(fact_pts) %>%
      mutate(prop_pt_concept = round(as.numeric(concept_pt_ct/total_pt_ct), 3),
             prop_row_concept = round(as.numeric(concept_row_ct/total_row_ct), 3),
             concept_group = concept_group)
    
    final_tbl[is.na(final_tbl)] <- 0
    
    result[[paste0(ecp_list[[i]][[1]])]] <- final_tbl
  }
  
  compress <- reduce(.x = result,
                     .f = dplyr::union)
  
  return(compress)
  
  
}



compute_ecp_ssanom <- function(cohort,
                               grouped_list,
                               ecp_concept_file = read_codeset('ecp_concepts', 'cccc')){
  
  ecp_list <- split(ecp_concept_file, seq(nrow(ecp_concept_file)))
  
  result <- list()
  
  for(i in 1:length(ecp_list)){
    
    concept_group <- ecp_list[[i]][[1]]
    
    join_cols <- set_names('concept_id', ecp_list[[i]][[3]])
    
    domain_tbl <- cdm_tbl(ecp_list[[i]][[2]]) %>%
      inner_join(cohort) %>%
      inner_join(load_codeset(ecp_list[[i]][[5]]), by = join_cols) %>%
      group_by(!!!syms(grouped_list)) %>%
      mutate(variable = concept_group) %>%
      select(person_id,
             all_of(group_vars(cohort)),
             variable) %>%
      group_by(person_id, variable, .add = TRUE) %>%
      summarise(ct = n())
    
    result[[i]] <- domain_tbl
    
  }
  
  domain_reduce <- purrr::reduce(.x = result,
                                .f = dplyr::union) %>%
    collect() %>%
    unite(facet_col, !!!syms(grouped_list), sep = '\n')
  
  facet_list <- group_split(domain_reduce %>% group_by(facet_col))
  
  jacc_list <- list()
  
  for(i in 1:length(facet_list)){
    
  grp <- facet_list[[i]] %>% distinct(facet_col) %>% pull()
  
  jaccards <- compute_jaccard_ecp(jaccard_input_tbl = facet_list[[i]]) %>%
    mutate(grp = grp)
  
  jacc_list[[i]] <- jaccards
  
  }
  
  jacc_reduce <- purrr::reduce(.x = jacc_list,
                               .f = dplyr::union)
  
  return(jacc_reduce)
}




compute_jaccard_ecp <- function(jaccard_input_tbl) {
  
  persons_concepts <- 
    jaccard_input_tbl %>% ungroup %>% #distinct() %>% collect()
    select(person_id,
           variable) %>% distinct() %>% collect()
  
  persons_concepts_cts <- 
    persons_concepts %>% 
    group_by(variable) %>% 
    summarise(concept_person_ct=n_distinct(person_id))
  
  concord <- 
    persons_concepts %>% table() %>% crossprod()
  diag(concord) <- -1
  
  best <- as_tibble(concord, rownames='concept1') %>% 
    pivot_longer(!concept1, names_to = 'concept2', values_to='cocount') %>% 
    filter(cocount != -1L) %>% mutate(across(.cols = c(cocount), .fns=as.integer)) %>%
    left_join(persons_concepts_cts, by = c('concept1'='variable'))%>%
    rename(concept1_ct=concept_person_ct)%>%
    left_join(persons_concepts_cts, by = c('concept2'='variable'))%>%
    rename(concept2_ct=concept_person_ct) %>%
    mutate(concept_count_union=concept1_ct+concept2_ct-cocount,
           jaccard_index=cocount/concept_count_union) %>% 
    mutate(concept1_prop=round(cocount/concept1_ct,2),
           concept2_prop=round(cocount/concept2_ct,2)) %>% 
    filter(concept1_ct > 0 & concept2_ct > 0 & cocount > 0) %>% 
    filter(concept1 > concept2) 
  
  best
  
}
