
#' FOT function (generally applicable)
#'
#' @param cohort 
#' @param check_func 
#' @param reduce_id 
#' @param time_period 
#' @param time_span 
#' @param site_list 
#'
#' @return
#' 
compute_fot <- function(cohort,
                        check_func,
                        reduce_id = 'visit_type',
                        time_period='year',
                        time_span= c('2012-01-01','2022-12-31'),
                        site_list=list('stanford',
                                       'colorado',
                                       'chop')
) {
  
  site_list_v <- unlist(site_list)
  
  final_results <- list()
  
  t1 <- seq(from=ymd(time_span[[1]]),to=ymd(time_span[[2]]),by=time_period)
  t2 <- ceiling_date(t1, time_period) - 1
  
  
  # narrows the visit time to cohort_entry and end date
  for(k in 1:length(t1)) {
    
    message(paste0('Starting ',t1[[k]]))
    
    target <- ymd(t1[[k]])
    
    baseline_start_date <- target
    baseline_end_date <- ceiling_date(target, time_period) - 1
    
    cohort_narrowed <- cohort %>% 
      mutate(start_date = as_date(baseline_start_date),
             end_date = as_date(baseline_end_date))
    
    
    cohort_narrow_prepped <- cohort_narrowed %>%
      filter(site %in% site_list_v) %>% 
      mutate(time_start=start_date,
             time_increment=time_period)
    
    output <- check_func(dat = cohort_narrow_prepped)
    
    if(is.list(output)&!any(class(output)=='tbl_sql')){
      output_reduced <- dplyr::bind_rows(output, .id= reduce_id)
    }else{output_reduced <- output}
    
    final_results[[k]] <- output_reduced
    
  }
  
  rslt = reduce(.x=final_results,
                .f=dplyr::union)
  
  return(rslt)
  
}
  
  
  
  
  #' Prepare cohort for check execution
  #' requirement: fields must have columns: 
  #' `person_id`, `start_date`, `end_date`
  #' 
  #' @param cohort_tbl table with required fields for each member of the cohort
  #' @param age_groups option to read in a CSV with age group designations to allow for stratification
  #'                   by age group in output. defaults to `NULL`. 
  #'                   sample CSV can be found in `specs/age_group_definitions.csv`
  #' @param codeset option to read in a CSV with codeset metadata to allow for labelling of 
  #'                cohort members based on a user-provided codeset. the codeset itself should be
  #'                a CSV file with at least a `concept_id` column and a `flag` column with user-provided
  #'                labels.
  #'                a sample metadata CSV, where the user can provide the correct table and column information,
  #'                can be found in `specs/codeset_metadata.csv`
  #' 
  #' 
  #' @return a tbl with person_id and the following:
  #'          `start_date` the cohort entry date
  #'          `end_date` the last visit
  #'          `fu`: length of follow up
  #'          `site` : patient site
  #'        if age_groups is not NULL: 
  #'          `age_ce`: patient age at cohort entry
  #'          `age_grp`: user-provided age grouping
  #'        if codeset is not NULL:
  #'          `flag`: flag that indiciates patient is a member of a user-specified group in the codeset
  #' 
  
  prepare_cohort <- function(cohort_tbl,
                             age_groups = NULL,
                             codeset = NULL) {
    
    ct <- cohort_tbl
    
    stnd <- 
      ct %>% 
      mutate(fu = round((end_date - start_date + 1)/365.25,3)) #%>% 
      #select(site, person_id, start_date, end_date, fu) #%>% 
      #add_site()
    
    if(!is.data.frame(age_groups)){
      final_age <- stnd
    }else{
      final_age <- compute_age_groups(cohort_tbl = stnd,
                                      person_tbl = cdm_tbl('person'),
                                      age_groups = age_groups)}
    
    if(!is.data.frame(codeset)){
      final_cdst <- stnd
    }else{
      final_cdst <- cohort_codeset_label(cohort_tbl = stnd,
                                         codeset_meta = codeset) %>%
        add_site()}
    
    final <- stnd %>%
      left_join(final_age) %>%
      left_join(final_cdst)
    
    return(final)
    
  }
  
  
  
## Function to check correct inputs for multi/single site argument
  
#' Check site type (single vs multi) against number of sites provided in list
#'
#' @param cohort 
#' @param multi_or_single_site 
#' @param site_list 
#'
#' @return
#' 
check_site_type <- function(cohort,
                            multi_or_single_site,
                            site_list){
  
  if('site' %in% colnames(cohort)){
    
    # count number of sites in site list that also exist in the cohort
    n_site <- cohort %>% filter(site %in% site_list) %>% 
      summarise(n_distinct(site)) %>% pull()
    
    if(multi_or_single_site == 'single' && n_site > 1){
    # create new "summary" site column / name, add that to grouped list
    # instead of site, and create new site list to account for new site name
      cohort_final <- cohort %>%
        filter(site %in% site_list) %>%
        mutate(site_summ = 'combined')
      
      grouped_list <- c('site_summ')
      site_list_adj <- 'combined'
      
    }else if(multi_or_single_site == 'multi' && n_site == 1){
      
      stop('Please include data from multiple sites in your cohort to 
           conduct a multi-site analysis.')
      
    }else if((multi_or_single_site == 'single' && n_site == 1) ||
             (multi_or_single_site == 'multi' && n_site > 1)){
      
      cohort_final <- cohort %>%
        filter(site %in% site_list)
      
      grouped_list <- c('site')
      site_list_adj <- site_list
      
    }else{stop('Invalid argument for multi_or_single_site. Please select either `single` or `multi`')}
  }else{stop('Please include a `site` column in your cohort.')}
  
  final <- list('cohort' = cohort_final, 
                'grouped_list' = grouped_list, 
                'site_list_adj' = site_list_adj)
  
  return(final)
}

#' check for `site_summ` column and switch to `site`
#' 
#' @param tbl the tbl with to use for 
#' replacement of `site_summ`
#' 
#' 
#' @return tbl with `site` replacing `site_summ`
#' 

replace_site_col <- function(tbl) {
  
  site_summ_exist <- 'site_summ' %in% colnames(tbl)
  site_exist <- 'site' %in% colnames(tbl)
  if(site_summ_exist & ! site_exist) 
    {final_tbl_site <- 
        tbl %>% rename(site = site_summ)} 
  else if(site_summ_exist & site_exist)
    {final_tbl_site <- 
        tbl %>% select(-site_summ)} 
  else {final_tbl_site <- tbl}
  
}

#' Join to vocabulary table
#'
#' @param tbl 
#' @param vocab_tbl 
#' @param col 
#'
#' @return
#' 
join_to_vocabulary <- function(tbl,
                               vocab_tbl,
                               col){
  
  final <- select(vocab_tbl, concept_id, concept_name) %>%
    rename('join_col' = concept_id) %>%
    right_join(tbl %>% rename('join_col' = col), by = c('join_col'),
               copy = TRUE) %>%
    rename_with(~col, join_col) %>%
    collect()
}