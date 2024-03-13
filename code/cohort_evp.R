

#' EVP Base Function
#'
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param grouped_list list of columns that should be used to group the table
#' @param time logical to determine whether the function is being run as part of `compute_fot` or not
#' @param evp_concept_file CSV file with information about each of the concept sets that should be
#'                         examined in the function. contains the following columns:
#'                         
#'                         `concept_group`, `default_tbl`, `field_name`, `date_field`, `codeset_name`
#'
#' @return dataframe with patient/row counts and proportions that are computed per group defined in
#'         grouped_list, and if time = TRUE, for each time period defined in compute_fot
#' 
compute_evp <- function(cohort,
                        grouped_list,
                        time = FALSE,
                        evp_concept_file = read_codeset('evp_concepts', 'cccc')){
  
  evp_list <- split(evp_concept_file, seq(nrow(evp_concept_file)))
  
  result <- list()
  
  for(i in 1:length(evp_list)){
    
    concept_group <- evp_list[[i]][[1]]
    
    message(paste0('Starting ', concept_group))
    
    domain_tbl <- cdm_tbl(evp_list[[i]][[2]]) %>%
      inner_join(cohort) %>%
      group_by(!!!syms(grouped_list))
    
    if(time){
      domain_tbl <- domain_tbl %>% 
        filter(!!sym(evp_list[[i]][[4]]) >= start_date &
                 !!sym(evp_list[[i]][[4]]) <= end_date) %>%
        group_by(time_start, time_increment, .add = TRUE)
      }
    
    total_pts <- domain_tbl %>%
      summarise(total_pt_ct = n_distinct(person_id),
                total_row_ct = n()) %>%
      collect()
    
    join_cols <- set_names('concept_id', evp_list[[i]][[3]])
    
    fact_pts <- domain_tbl %>%
      inner_join(load_codeset(evp_list[[i]][[5]]), by = join_cols) %>%
      summarise(concept_pt_ct = n_distinct(person_id),
                concept_row_ct = n()) %>% collect()
    
    final_tbl <- total_pts %>%
      left_join(fact_pts) %>%
      mutate(prop_pt_concept = round(as.numeric(concept_pt_ct/total_pt_ct), 3),
             prop_row_concept = round(as.numeric(concept_row_ct/total_row_ct), 3),
             concept_group = concept_group)
    
    final_tbl[is.na(final_tbl)] <- 0
    
    result[[paste0(evp_list[[i]][[1]])]] <- final_tbl
  }
  
  compress <- reduce(.x = result,
                     .f = dplyr::union)
  
  return(compress)
  
  
}



compute_evp_ssanom <- function(cohort,
                               grouped_list,
                               evp_concept_file = read_codeset('evp_concepts', 'cccc')){
  
  evp_list <- split(evp_concept_file, seq(nrow(evp_concept_file)))
  
  result <- list()
  
  for(i in 1:length(evp_list)){
    
    concept_group <- evp_list[[i]][[1]]
    
    join_cols <- set_names('concept_id', evp_list[[i]][[3]])
    
    domain_tbl <- cdm_tbl(evp_list[[i]][[2]]) %>%
      inner_join(cohort) %>%
      inner_join(load_codeset(evp_list[[i]][[5]]), by = join_cols) %>%
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
  
  jaccards <- compute_jaccard_evp(jaccard_input_tbl = facet_list[[i]]) %>%
    mutate(grp = grp)
  
  jacc_list[[i]] <- jaccards
  
  }
  
  jacc_reduce <- purrr::reduce(.x = jacc_list,
                               .f = dplyr::union)
  
  return(jacc_reduce)
}




compute_jaccard_evp <- function(jaccard_input_tbl) {
  
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


#' *MS Anomaly Across Time Output*
#' 
#' @param process_output the input tbl to compute an AUC for the multi-site across time analysis  
#' @param grp_vars variables to group by to compute the aggregated proportion for all sites
#' @param var_col column for which to compute the AUC
#' 
#' @return a dataframe with the AUC values for each variable included in the function input table
#' 

compute_evp_auc <- function(process_output,
                            grp_vars=c('time_start',
                                       'time_increment',
                                       'concept_group'),
                            output_level = 'row') {
  
  
  if(output_level == 'row'){
    var_col <- 'prop_row_concept'
  }else if(output_level == 'patient'){
    var_col <- 'prop_pt_concept'
  }else(stop('Please select a valid output level for AUC computation: `patient` or `row`'))
  
  x <- compute_dist_mean_conc(tbl=process_output,
                              grp_vars=grp_vars,
                              var_col=var_col,
                              num_sd = 2,
                              num_mad = 2)  %>% 
    rename(mean_allsiteprop=mean)
  
  x_filtered <- 
    x %>% select(site,
                 !!!syms(grp_vars),
                 !!sym(var_col),
                 mean_allsiteprop)
  
  # x_variableconcepts <- 
  #   x_filtered %>% distinct(variable,concept_id)
  
  x_concepts <- 
    x_filtered %>% ungroup() %>% distinct(concept_group) %>% pull()
  
  output <- list()
  
  for(i in 1:length(x_concepts)) {
    
    aucs <- compute_auc_at(tbl_name= x_filtered %>% filter(concept_group==x_concepts[[i]]) %>%
                             ungroup(),
                           iterate_var = 'site',
                           time_var = 'time_start',
                           outcome_var = var_col,
                           gold_standard_var = 'mean_allsiteprop') %>% 
      mutate(concept_group=x_concepts[[i]],
             auc_mean=round(mean(auc_value, na.rm = TRUE),4),
             auc_sd=round(sd(auc_value, na.rm = TRUE),4))
    
    
    
    
    output[[i]] <- aucs  
    
  }
  
  output_reduced <- reduce(.x=output,
                           .f=dplyr::union) #%>% 
    #inner_join(x_variableconcepts)
  
  
}