

#' Base SCV function
#' 
#' @param cohort 
#' @param concept_set 
#' @param code_type 
#' @param code_domain 
#' @param time 
#' @param domain_tbl 
#' 
#' @return 
#' 
check_code_dist <- function(cohort,
                            concept_set,
                            code_type,
                            code_domain,
                            time = FALSE,
                            domain_tbl = read_codeset('scv_domains', 'cccc')){
  
  # pick the right domain/columns
  domain_filter <- domain_tbl %>% filter(domain == code_domain)
  concept_col <- domain_filter$concept_col
  source_col <- domain_filter$source_col
  
  if(code_type=='source') {
     final_col = source_col
  }else if(code_type == 'cdm'){
    final_col = concept_col
  }else{stop(paste0(code_type, ' is not a valid argument. Please select either "source" or "cdm"'))}
  
  if(time){
    
    domain_tbl <- cohort %>%
      inner_join(cdm_tbl(code_domain)) %>%
      filter(!!sym(domain_filter$date_col) >= start_date,
             !!sym(domain_filter$date_col) <= end_date)
    
    
    fact_tbl <- 
      domain_tbl %>% 
      inner_join(concept_set,
                 by=setNames('concept_id',final_col)) %>% 
      select(all_of(group_vars(cohort)),
             all_of(concept_col),
             all_of(source_col),
             time_start,
             time_increment) %>% 
      rename('concept_id' = concept_col,
             'source_concept_id' = source_col) %>%
      group_by(time_start, time_increment, .add = TRUE)
      
  }else{
    
    domain_tbl <- cohort %>%
      inner_join(cdm_tbl(code_domain))
    
    
    fact_tbl <- 
      domain_tbl %>% 
      inner_join(concept_set,
                 by=setNames('concept_id',final_col)) %>% 
      select(all_of(group_vars(cohort)),
             all_of(concept_col),
             all_of(source_col)) %>% 
      rename('concept_id' = concept_col,
             'source_concept_id' = source_col)
    
    }
  
  grouped_output <- 
    fact_tbl %>% 
    group_by(
      concept_id,
      source_concept_id, 
      .add = TRUE
    ) %>% summarise(ct=n()) %>% 
    compute_new()
  
  
  denom_concepts <- 
    fact_tbl %>% 
    group_by(
      concept_id,
      .add = TRUE
    ) %>% summarise(denom_concept_ct=n()) %>% 
    compute_new()
  
  denom_source <- 
    fact_tbl %>% 
    group_by(
      source_concept_id,
      .add = TRUE
    ) %>% summarise(denom_source_ct=n()) %>% 
  compute_new()
  
  grouped_output_totals <- 
    grouped_output %>% left_join(denom_concepts) %>% 
    left_join(denom_source) %>% collect() %>% 
    mutate(concept_prop = round(ct/denom_concept_ct, 2),
           source_prop = round(ct/denom_source_ct,2)) 
    
  
  
}