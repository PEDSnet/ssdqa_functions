

compute_event_counts <- function(cohort,
                                 grouped_list,
                                 site_col,
                                 intermediate_tbl = FALSE,
                                 time = FALSE,
                                 event_csv = read_codeset('pes_events_1', 'ccccc')){
  
  ## Make sure time var is included in group if needed
  if(time){grouped_list <- grouped_list %>% 
             append('time_start') %>% append('time_increment') %>% unique()}
  
  ## Pull event information
  event_list <- split(event_csv, seq(nrow(event_csv)))
  
  if(length(event_list) > 2){cli::cli_abort('Please only select 2 events to compare')}
  
  ## Get event data
  event_rslt <- list()
  total_rslt <- list()
  
  for(i in 1:length(event_list)){
    
    if(!time){
      event_domain <- cdm_tbl(event_list[[i]][[3]]) %>%
        inner_join(cohort) %>%
        group_by(person_id, !!!syms(grouped_list)) %>%
        filter(!!sym(event_list[[i]][[5]]) >= start_date,
               !!sym(event_list[[i]][[5]]) <= end_date)
    }else{
      event_domain <- cdm_tbl(event_list[[i]][[3]]) %>%
        inner_join(cohort) %>%
        filter(!!sym(event_list[[i]][[5]]) >= time_start,
               !!sym(event_list[[i]][[5]]) <= time_end) %>%
        group_by(person_id, !!!syms(grouped_list))
      
      
      total_rslt[[i]] <- event_domain %>% ungroup() %>% 
        distinct(!!sym(site_col), person_id)
      }
    
    join_cols <- set_names('concept_id', event_list[[i]][[4]])
    
    if(is.na(event_list[[i]][[7]])){
      event_cts <- event_domain %>%
        inner_join(load_codeset(event_list[[i]][[6]]), by = join_cols) %>%
        summarise(event_count = n(), .groups = 'keep') %>%
        distinct(person_id, !!!syms(grouped_list), event_count) %>%
        collect() %>%
        mutate(event_type = event_list[[i]][[1]],
               event_name = event_list[[i]][[2]])
    }else{
      event_cts <- event_domain %>%
        inner_join(load_codeset(event_list[[i]][[6]]), by = join_cols) %>%
        filter(!! rlang::parse_expr(event_list[[i]][[7]])) %>%
        summarise(event_count = n(), .groups = 'keep') %>%
        distinct(person_id, !!!syms(grouped_list), event_count) %>%
        collect() %>%
        mutate(event_type = event_list[[i]][[1]],
               event_name = event_list[[i]][[2]])
    }
    
    event_rslt[[i]] <- event_cts
    
  }
  
  event_combo <- purrr::reduce(.x = event_rslt,
                               .f = dplyr::union)
  
  if(time){cohort <- purrr::reduce(.x = total_rslt,
                                   .f = dplyr::union) %>% collect()}
  
  ## Reformat event data
  grp <- group_vars(event_combo)
  new_grp <- grp[!grp %in% 'person_id']
  
  eventa <- event_combo %>% filter(toupper(event_type) == 'A') %>%
    rename(event_a_num = event_count,
           event_a_name = event_name) %>% select(-event_type)
  
  eventb <- event_combo %>% filter(toupper(event_type) == 'B') %>%
    rename(event_b_num = event_count,
           event_b_name = event_name) %>% select(-event_type)
  
  event_ptlv <- eventa %>% full_join(eventb) %>% 
    full_join(cohort %>% select(!!sym(site_col), person_id) %>% collect()) %>%
    mutate(event_a_num = ifelse(is.na(event_a_num), 0, event_a_num),
           event_b_num = ifelse(is.na(event_b_num), 0, event_b_num)) %>%
    ungroup() %>%
    fill(event_b_name, .direction = 'updown') %>% fill(event_a_name, .direction = 'updown')
  
  if(time){
    time_increment_str <- event_combo %>% ungroup() %>% distinct(time_increment) %>% pull()
    time_start_str <- event_combo %>% ungroup() %>% distinct(time_start) %>% pull()
    
    event_ptlv <- event_ptlv %>%
      mutate(time_start = time_start_str,
             time_increment = time_increment_str)
  }
  
  ## OPTIONAL: output patient level file
  if(intermediate_tbl){
    write_csv(x = event_ptlv, file = paste0(base_dir, 
                                            '/results/event_cts_patient_output.csv'))
    
    }
  
  ## Aggregate results
  
  event_agg <- event_ptlv %>%
    group_by(!!!syms(new_grp), event_a_num, event_a_name,
             event_b_num, event_b_name) %>%
    summarise(pt_ct = n()) %>%
    group_by(!!sym(site_col)) %>%
    mutate(total_pts = sum(pt_ct)) %>% ungroup()
  
  return(event_agg)
  
}


#' Title
#'
#' @param prc_input_tbl 
#' @param grouped_list 
#'
#' @return
#' @export
#'
#' @examples
compute_prc_ntanom <- function(cohort = results_tbl('jspa_cohort'),
                               site_col,
                               grouped_list = 'site',
                               event_csv = read_codeset('pes_events_1', 'ccccc'),
                               grp_breaks = c(0, 1, 3, 8, 11, 15, 25, 50, 100)){
  
  ## Pull event information
  event_list <- split(event_csv, seq(nrow(event_csv)))
  
  if(length(event_list) > 2){cli::cli_abort('Please only select 2 events to compare')}
  
  ## Get event data
  event_rslt <- list()
  
  for(i in 1:length(event_list)){

      event_domain <- cdm_tbl(event_list[[i]][[3]]) %>%
        inner_join(cohort) %>%
        group_by(person_id, !!!syms(grouped_list)) %>%
        filter(!!sym(event_list[[i]][[5]]) >= start_date,
               !!sym(event_list[[i]][[5]]) <= end_date)
    
    join_cols <- set_names('concept_id', event_list[[i]][[4]])
    
    if(is.na(event_list[[i]][[7]])){
      event_cts <- event_domain %>%
        inner_join(load_codeset(event_list[[i]][[6]]), by = join_cols) %>%
        summarise(event_count = n(), .groups = 'keep') %>%
        distinct(person_id, !!!syms(grouped_list), event_count) %>%
        collect() %>%
        mutate(event_type = event_list[[i]][[1]],
               event_name = event_list[[i]][[2]])
    }else{
      event_cts <- event_domain %>%
        inner_join(load_codeset(event_list[[i]][[6]]), by = join_cols) %>%
        filter(!! rlang::parse_expr(event_list[[i]][[7]])) %>%
        summarise(event_count = n(), .groups = 'keep') %>%
        distinct(person_id, !!!syms(grouped_list), event_count) %>%
        collect() %>%
        mutate(event_type = event_list[[i]][[1]],
               event_name = event_list[[i]][[2]])
    }
    
    event_rslt[[i]] <- event_cts
    
  }
  
  event_combo <- purrr::reduce(.x = event_rslt,
                               .f = dplyr::union)
  
  ## Reformat event data
  grp <- group_vars(event_combo)
  new_grp <- grp[!grp %in% 'person_id']
  
  eventa <- event_combo %>% filter(toupper(event_type) == 'A') %>%
    rename(event_a_num = event_count,
           event_a_name = event_name) %>% select(-event_type)
  
  eventb <- event_combo %>% filter(toupper(event_type) == 'B') %>%
    rename(event_b_num = event_count,
           event_b_name = event_name) %>% select(-event_type)
  
  ## Get Patient - Level data
  event_ptlv <- eventa %>% full_join(eventb) %>% 
    full_join(cohort %>% select(!!sym(site_col), person_id, fu) %>% collect()) %>%
    mutate(event_a_num = ifelse(is.na(event_a_num), 0, event_a_num),
           event_b_num = ifelse(is.na(event_b_num), 0, event_b_num)) %>%
    ungroup() %>%
    fill(event_b_name, .direction = 'updown') %>% fill(event_a_name, .direction = 'updown')
  
  
  ## Bin FU time
  grp_breaks <- grp_breaks %>% append(Inf) %>% unique()
  grouped_list <- grouped_list %>% append('fu_bins')
  
  event_type_cts <- as_tibble(event_ptlv) %>%
        pivot_wider(names_from = 'event_a_name',
                    values_from = 'event_a_num') %>%
        pivot_wider(names_from = 'event_b_name',
                    values_from = 'event_b_num') %>%
        pivot_longer(cols = !c('person_id', !!sym(site_col), fu),
                     names_to = 'event_name') %>%
        filter(value != 0)
  
  binned_fu_time <- event_type_cts %>%
    ungroup() %>%
    mutate(fu_bins = cut(fu, breaks = grp_breaks, right = FALSE)) %>%
    # group_by(fu_bins) %>%
    # mutate(fu_min = round(min(fu), 3),
    #        fu_max = round(max(fu), 3)) %>%
    # mutate(bin_cat = fu_bins) %>%
    unite(facet_col, !!!syms(grouped_list), sep = '_')

  facet_list <- group_split(binned_fu_time %>% group_by(facet_col))
  
  # if(target_col == 'bin_col'){
  # 
  #   ## Create event bins
  #   binned_event_cts <- as_tibble(event_ptlv) %>%
  #     ungroup() %>%
  #     mutate(eventa_bin = case_when(event_a_num >= 0 & event_a_num < 10 ~ as.character(event_a_num),
  #                                   event_a_num >= 10 & event_a_num < 100 ~ as.character(cut_interval(event_a_num, length = 10),
  #                                                                                        right = FALSE),
  #                                   event_a_num >= 100 & event_a_num < 1000 ~ as.character(cut_interval(event_a_num, length = 100)),
  #                                   event_a_num >= 1000 ~ as.character(cut_interval(event_a_num, length = 1000))),
  #            eventb_bin = case_when(event_b_num >= 0 & event_b_num < 10 ~ as.character(event_b_num),
  #                                   event_b_num >= 10 & event_b_num < 100 ~ as.character(cut_interval(event_b_num, length = 10),
  #                                                                                        right = FALSE),
  #                                   event_b_num >= 100 & event_b_num < 1000 ~ as.character(cut_interval(event_b_num, length = 100)),
  #                                   event_b_num >= 1000 ~ as.character(cut_interval(event_b_num, length = 1000)))) %>%
  #     pivot_longer(cols = c(eventa_bin, eventb_bin)) %>%
  #     mutate(bin_col = paste0(name, '_', value)) %>% select(-c(name, value)) %>%
  #     unite(facet_col, !!!syms(grouped_list), sep = '\n')
  #   
  #   facet_list <- group_split(binned_event_cts %>% group_by(facet_col))
  # 
  # }else if(target_col == 'event_name'){
  #   
  #   event_type_cts <- event_ptlv %>%
  #     pivot_wider(names_from = 'event_a_name', 
  #                 values_from = 'event_a_num') %>% 
  #     pivot_wider(names_from = 'event_b_name', 
  #                 values_from = 'event_b_num') %>% 
  #     pivot_longer(cols = !c('person_id', 'site'),
  #                  names_to = 'event_name') %>% 
  #     filter(value != 0) %>%
  #     unite(facet_col, !!!syms(grouped_list), sep = '\n')
  #     
  #   facet_list <- group_split(event_type_cts %>% group_by(facet_col))
  #   
  # }
  
  jacc_list <- list()
  
  for(i in 1:length(facet_list)){
    
    grp <- facet_list[[i]] %>% distinct(facet_col) %>% pull()
    
    jaccards <- compute_jaccard(jaccard_input_tbl = facet_list[[i]],
                                var_col = 'event_name') %>%
      mutate(grp = grp)
    
    jacc_list[[i]] <- jaccards
    
  }
  
  jacc_reduce <- purrr::reduce(.x = jacc_list,
                               .f = dplyr::union)
  
  return(jacc_reduce)
  
}