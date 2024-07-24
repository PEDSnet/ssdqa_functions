

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
                        evp_variable_file = read_codeset('evp_variables', 'cccc')){
  
  evp_list <- split(evp_variable_file, seq(nrow(evp_variable_file)))
  
  result <- list()
  
  for(i in 1:length(evp_list)){
    
    variable <- evp_list[[i]][[1]]
    
    message(paste0('Starting ', variable))
    
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
    
    if(is.na(evp_list[[i]][[6]])){
      fact_pts <- domain_tbl %>%
        inner_join(load_codeset(evp_list[[i]][[5]]), by = join_cols) %>%
        summarise(variable_pt_ct = n_distinct(person_id),
                  variable_row_ct = n()) %>% collect()
    }else{
      fact_pts <- domain_tbl %>%
        inner_join(load_codeset(evp_list[[i]][[5]]), by = join_cols) %>%
        filter(!! rlang::parse_expr(evp_list[[i]][[6]])) %>%
        summarise(variable_pt_ct = n_distinct(person_id),
                  variable_row_ct = n()) %>% collect()
    }
    
    final_tbl <- total_pts %>%
      left_join(fact_pts) %>%
      mutate(prop_pt_variable = round(as.numeric(variable_pt_ct/total_pt_ct), 3),
             prop_row_variable = round(as.numeric(variable_row_ct/total_row_ct), 3),
             variable = variable)
    
    final_tbl[is.na(final_tbl)] <- 0
    
    result[[paste0(evp_list[[i]][[1]])]] <- final_tbl
  }
  
  compress <- reduce(.x = result,
                     .f = dplyr::union)
  
  return(compress)
  
  
}



#' Single site anomaly no time processing for EVP
#'
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param grouped_list list of columns that should be used to group the table
#' @param evp_concept_file CSV file with information about each of the concept sets that should be
#'                         examined in the function. contains the following columns:
#'                         
#'                         `concept_group`, `default_tbl`, `field_name`, `date_field`, `codeset_name`
#'
#' @return one dataframe with the jaccard similarity index for each concept group provided
#'         in the concept file
#'
compute_evp_ssanom <- function(cohort,
                               grouped_list,
                               evp_variable_file = read_codeset('evp_variables', 'cccc')){
  
  evp_list <- split(evp_variable_file, seq(nrow(evp_variable_file)))
  
  result <- list()
  
  for(i in 1:length(evp_list)){
    
    variable <- evp_list[[i]][[1]]
    
    join_cols <- set_names('concept_id', evp_list[[i]][[3]])
    
    domain_tbl <- cdm_tbl(evp_list[[i]][[2]]) %>%
      inner_join(cohort) %>%
      inner_join(load_codeset(evp_list[[i]][[5]]), by = join_cols) %>%
      group_by(!!!syms(grouped_list)) %>%
      mutate(variable = variable) %>%
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
  
  jaccards <- compute_jaccard(jaccard_input_tbl = facet_list[[i]],
                              var_col = 'variable') %>%
    mutate(grp = grp)
  
  jacc_list[[i]] <- jaccards
  
  }
  
  jacc_reduce <- purrr::reduce(.x = jacc_list,
                               .f = dplyr::union)
  
  return(jacc_reduce)
}
