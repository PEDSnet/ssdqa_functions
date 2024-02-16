

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