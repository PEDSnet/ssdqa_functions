
#' Compute sequence of 2 events
#'
#' @param cohort a table with the cohort of interest; should minimally include:
#' 
#'               a site column | @patid | @start_date | @end_date
#' 
#' @param grouped_list list of columns in the data that should be used to group
#'                     the results
#' @param user_cutoff a user specified threshold against which to measure the
#'                    number of days between the two events
#' @param time a logical to indicate whether this analysis is being conducted
#'             over time or not
#' @param n_event_a the number of times Event A should occur before establishing
#'                  the index date
#' @param n_event_b the number of times Event B should occur before esrablishing
#'                  the occurrence date
#' @param intermediate_tbl a logical to indicate whether an intermediate table with
#'                         patient level results should be output. if TRUE, a CSV
#'                         file with one row per patient will be returned in addition
#'                         to aggregate output
#' @param event_csv a csv file with the definitions for each event with the columns
#'                  
#'                  - @event - A or B
#'                  - @event_label - a descriptive label for the event
#'                  - @default_tbl - the default CDM table from which data is retrieved
#'                  - @field_name - the field in the table where the codes of interest are
#'                                  stored
#'                  - @date_field - the date field to be used to establish the index &
#'                                  occurrence dates
#'                  - @codeset_name - the name of the codeset in the specs directory to
#'                                    define the variable of interest
#'                  - @filter_logic - a string indicating any filter logic that should be
#'                                    applied to establish the event
#'                                    
#'                                    ex: an Hba1c > 6.5
#'
#' @return an aggregated dataframe outlining the number of patients that had the two events
#'         occur in X number of days
#'         
#'         if intermediate_tbl = TRUE, a patient level CSV is also returned
#'

compute_event_sequence <- function(cohort,
                                   grouped_list,
                                   site_col,
                                   user_cutoff = 30,
                                   n_event_a = 1,
                                   n_event_b = 1,
                                   intermediate_tbl = FALSE,
                                   time = FALSE,
                                   time_period = 'year',
                                   time_span = c('2011-01-01', '2023-12-31'),
                                   event_csv = read_codeset('pes_events', 'ccccc')){
  
  ## Pull event information
  event_list <- split(event_csv, seq(nrow(event_csv)))
  
  if(length(event_list) > 2){cli::cli_abort('Please only select 2 events to compare')}
  
  ## Get event data
  event_rslt <- list()
  
  for(i in 1:length(event_list)){
    
    if(toupper(event_list[[i]][[1]]) == 'A'){
      event_ct <- n_event_a}else{event_ct <- n_event_b}
  
    event_domain <- cdm_tbl(event_list[[i]][[3]]) %>%
      inner_join(cohort) %>%
      group_by(person_id, !!!syms(grouped_list))
    
    join_cols <- set_names('concept_id', event_list[[i]][[4]])
    
    if(is.na(event_list[[i]][[7]])){
      event_index <- event_domain %>%
        inner_join(load_codeset(event_list[[i]][[6]]), by = join_cols) %>%
        window_order(!!sym(event_list[[i]][[5]])) %>%
        mutate(event_n = row_number()) %>%
        filter(event_n == event_ct) %>%
        mutate(event_date = !!sym(event_list[[i]][[5]])) %>%
        distinct(person_id, !!!syms(grouped_list), event_date) %>%
        collect() %>%
        mutate(event_type = event_list[[i]][[1]],
               event_name = event_list[[i]][[2]])
    }else{
      event_index <- event_domain %>%
        inner_join(load_codeset(event_list[[i]][[6]]), by = join_cols) %>%
        filter(!! rlang::parse_expr(event_list[[i]][[7]])) %>%
        window_order(!!sym(event_list[[i]][[5]])) %>%
        mutate(event_n = row_number()) %>%
        filter(event_n == event_ct) %>%
        mutate(event_date = !!sym(event_list[[i]][[5]])) %>%
        distinct(person_id, !!!syms(grouped_list), event_date) %>%
        collect() %>%
        mutate(event_type = event_list[[i]][[1]],
               event_name = event_list[[i]][[2]])
    }
      
    event_rslt[[i]] <- event_index
    
  }
  
  event_combo <- purrr::reduce(.x = event_rslt,
                               .f = dplyr::union)
  
  ## Reformat event data
  grp <- group_vars(event_combo)
  new_grp <- grp[!grp %in% 'person_id']
  
  eventa <- event_combo %>% filter(toupper(event_type) == 'A') %>%
    rename(event_a_index_date = event_date,
           event_a_name = event_name) %>% select(-event_type)
  
  eventb <- event_combo %>% filter(toupper(event_type) == 'B') %>%
    rename(event_b_occurrence_date = event_date,
           event_b_name = event_name) %>% select(-event_type)
  
  event_ptlv <- eventa %>% left_join(eventb) %>%
    mutate(num_days = as.numeric(event_b_occurrence_date - event_a_index_date)) %>%
    mutate(user_cutoff = user_cutoff) %>% ungroup() %>% fill(event_b_name, .direction = 'updown')
  
  ## OPTIONAL: output patient level file
  if(intermediate_tbl){
    write_csv(x = event_ptlv, file = paste0(base_dir, 
                                            '/results/event_seq_patient_output.csv'))}
  
  if(time){
    
    t1 <- seq(from=ymd(time_span[[1]]),to=ymd(time_span[[2]]),by=time_period)
    t2 <- ceiling_date(t1, time_period) - 1
    
    time_df <- tibble('time_start' = t1,
                      'time_end' = t2)
    
    cat_df <- event_ptlv %>% select(!!sym(site_col), user_cutoff, event_a_name, event_b_name) %>%
      distinct() %>% cross_join(time_df) %>% select(-time_end)
    
    event_agg <- event_ptlv %>%
      cross_join(time_df) %>%
      filter(event_a_index_date <= time_end,
             event_a_index_date >= time_start) %>%
      group_by(!!!syms(new_grp), num_days, user_cutoff, time_start,
               event_a_name, event_b_name) %>%
      summarise(pt_ct = n()) %>%
      group_by(!!!syms(new_grp), user_cutoff, time_start) %>%
      mutate(total_pts = sum(pt_ct)) %>%
      ungroup() %>%
      filter(!is.na(!!sym(site_col))) %>%
      full_join(cat_df) %>%
      mutate(user_cutoff = user_cutoff,
             total_pts = ifelse(is.na(total_pts), 0, total_pts),
             pt_ct = ifelse(is.na(pt_ct), 0, pt_ct),
             time_increment = time_period)
    
    
  }else{
    
    ## Aggregate patient level output
    event_agg <- event_ptlv %>%
      group_by(!!!syms(new_grp), num_days, user_cutoff,
               event_a_name, event_b_name) %>%
      summarise(pt_ct = n()) %>%
      ungroup() %>%
      mutate(total_pts = sum(pt_ct))
    
  }
  
  return(event_agg)
  
}