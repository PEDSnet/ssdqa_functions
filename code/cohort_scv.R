

#' Base SCV function
#' 
#' @param cohort table of cohort members with at least `site`, `person_id`, `start_date`, and `end_date`
#' @param concept_set for analyses where time = FALSE, a csv file with the source or cdm codes of interest for the analysis.
#'                    should contain at least a `concept_id` column
#'                    
#'                    for analyses where time = TRUE, a vector with up to 5 source or cdm codes of interest for the analysis.
#' @param code_type the type of code to be examined in the check; either `source` or `cdm`
#' @param code_domain the domain related to the codes in the `concept_set`; should match a domain and
#'                    its associated metadata in the `domain_tbl` file
#' @param time logical to indicate whether the user would like to examine mappings over time or not
#' @param domain_tbl a table with a list of domains and associated metadata; should include the name
#'                   of the domain, the name of the column where source codes can be found, the name
#'                   of the column where CDM codes can be found, and the date column that should
#'                   be used during over time computations
#' 
#' @return one dataframe with counts and proportions of source -> cdm or cdm -> source mapping pairs 
#'         for each facet group. if time = FALSE, these counts are computed overall. if time = TRUE, 
#'         these counts are computed for each user-specified time increment.
#'         
#'         
#'
check_code_dist <- function(cohort,
                            concept_set,
                            code_type,
                            code_domain,
                            time = FALSE,
                            domain_tbl = read_codeset('scv_domains', 'cccc')){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  # pick the right domain/columns
  domain_filter <- domain_tbl %>% filter(domain == code_domain)
  concept_col <- domain_filter$concept_col
  source_col <- domain_filter$source_col
  
  if(code_type=='source') {
     final_col = source_col
  }else if(code_type == 'cdm'){
    final_col = concept_col
  }else{cli::cli_abort(paste0(code_type, ' is not a valid argument. Please select either {.code source} or {.code cdm}'))}
  
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

