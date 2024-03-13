

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




#' *MS Anomaly Across Time Output*
#' 
#' @param process_output the input tbl to compute an AUC for the multi-site across time analysis  
#' @param grp_vars variables to group by to compute the aggregated proportion for all sites
#' @param code_type the code type input into the function which will determine which concept column
#'                  to use for the analysis
#'                  
#' @return dataframe with AUC values for each mapped concept in the cohort 
#'         (i.e. if code_type = cdm, AUC for source_concept_id will be computed and vice versa)  
#' 

compute_scv_auc <- function(process_output,
                            grp_vars=c('time_start',
                                       'time_increment',
                                       'concept_id',
                                       'source_concept_id'),
                            code_type = 'cdm') {
  
  
  if(code_type == 'cdm'){
    var_col <- 'concept_prop'
    id_col <- 'source_concept_id'
  }else if(code_type == 'source'){
    var_col <- 'source_prop'
    id_col <- 'concept_id'
  }else(stop('Please select a valid code type for AUC computation: `cdm` or `source`'))
  
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
  
  x_variableconcepts <-
    x_filtered %>% distinct(site,concept_id,source_concept_id,concept_prop)
  
  x_concepts <- 
    x_filtered %>% ungroup() %>% distinct(!!sym(id_col)) %>% pull()
  
  output <- list()
  
  for(i in 1:length(x_concepts)) {
    
    aucs <- compute_auc_at(tbl_name= x_filtered %>% filter(!!sym(id_col)==x_concepts[[i]]),
                           iterate_var = 'site',
                           time_var = 'time_start',
                           outcome_var = var_col,
                           gold_standard_var = 'mean_allsiteprop') %>% 
      mutate(mapped_id=x_concepts[[i]],
             auc_mean=round(mean(auc_value, na.rm = TRUE),4),
             auc_sd=round(sd(auc_value, na.rm = TRUE),4))
    
    
    
    
    output[[i]] <- aucs  
    
  }
  
  output_reduced <- reduce(.x=output,
                           .f=dplyr::union) %>% 
    inner_join(x_filtered, by = c('mapped_id' = id_col, var_col, 'site', 'time_start', 'mean_allsiteprop')) %>%
    select(-time_increment)
  
  
}