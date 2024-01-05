
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
      filter(site %in% site_list_v) %>% mutate(time_start=start_date,
                                               time_increment=time_period)
    
    output <- check_func(dat = cohort_narrow_prepped)
    
    if(is.list(output)){
    output_reduced <- dplyr::bind_rows(output, .id= reduce_id) 
    }else(output_reduced <- output)
    
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
      mutate(fu = round((end_date - start_date + 1)/365.25,3)) %>% 
      select(person_id, start_date, end_date, fu) %>% 
      add_site()
    
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