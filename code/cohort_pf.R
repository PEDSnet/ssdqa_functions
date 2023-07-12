
#' compute age at cohort entry
#' 
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param person_tbl the CDM person table
#' @param age_groups a csv file (template found in specs folder) where the user defines the minimum and maximum
#'                   age allowed for a group and provides a string name for the group
#' 
#' @return `cohort_tbl` with the age at cohort entry and age group for each patient
#' 
#' 

compute_age_groups <- function(cohort_tbl,
                               person_tbl,
                               age_groups) {
  
  cohorts <- cohort_tbl %>% 
    inner_join(select(person_tbl,
                      person_id,
                      birth_date)) %>% 
    mutate(age_ce = floor((start_date - birth_date)/365.25)) %>%
    collect_new()
  
  cohorts_grpd <- cohorts %>%
    cross_join(age_groups) %>%
    mutate(age_grp = case_when(age_ce >= min_age & age_ce <= max_age ~ group,
                               TRUE ~ as.character(NA))) %>%
    filter(!is.na(age_grp)) %>%
    select(-c(birth_date, min_age, max_age, group))
  
  copy_to_new(df = cohorts_grpd)
  
}

#' function to take in domain and compute patient facts
#' 
#' @param cohort the cohort for which to iterate 
#' @param pf_input_tbl the tbl that should be iterated 
#' so person facts are computed
#' @param grouped_list a vector to group input by. Defaults to `person_id`,
#' `start_date`, `end_date`, `fu`, `site`
#' @param domain_tbl the config CSV file
#' 
#' @return the column `person_id`, for the specified visit type, 
#' the number of facts for the person - each fact corresponds to the 
#' fact in the CSV file; each column is a domain
#' 

compute_pf <- function(cohort, 
                       pf_input_tbl,
                       grouped_list,
                       domain_tbl) {
  
  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))
  
  
  for (i in 1:length(domain_list)) {
    
    domain_name = domain_list[[i]][[1]]
    message(paste0('Starting domain ', domain_list[[i]][1]))
    
    ## checks to see if the table needs to be filtered in any way; 
    ## allow for one filtering operation
    if(! is.na(domain_list[[i]][[3]])) {
      filter_var <- domain_list[[i]][[3]] 
      filter_vec <- as.integer(strsplit(domain_list[[i]][[4]],split=',',fixed = TRUE)[[1]])
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
        filter(!! sym(filter_var) %in% c(filter_vec))
    } else {domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]]))}
    
    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    pf <- 
      pf_input_tbl %>% 
      inner_join(select(domain_tbl_use,
                        visit_occurrence_id)) %>% 
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>% 
      ungroup() %>% 
      mutate(domain=domain_name) %>% 
      mutate(fact_ct_strat=round(total_strat_ct/fu,2)) %>% 
      select(-total_strat_ct) %>% 
      select(person_id,
             domain,
             fact_ct_strat) %>%
      pivot_wider(names_from=domain,
                  values_from=fact_ct_strat) %>% 
      right_join(cohort) %>%
      relocate(person_id) %>%
      compute_new(indexes=list('person_id'))
    
    domain_results[[domain_name]] <- pf
  }
  
  domain_results_left_join <- 
    reduce(.x=domain_results,
           .f=left_join)
}

#' loops through visit types and sites to compute patient facts
#' 
#' @param cohort_tbl the tbl that comes from `prepare_pf`
#' @param combine_sites a logical that tells the program to either loop through sites or run it once for all sites
#' @param age_groups option to read in a CSV with age group designations to allow for stratification
#'                   by age group in output. defaults to `NULL`. 
#'                   sample CSV can be found in `specs/age_group_definitions.csv`
#' @param visit_type_tbl The visit_concept_ids of interest for the analysis. `all` may be used in this field
#'                      to select every visit type; defaults to `pf_visit_types` in specs folder
#' @param visit_tbl the cdm visit_occurrence tbl; defaults to `cdm_tbl('visit_occurrence')`
#' @param site_list the sites to iterate through
#' @param visit_list the list of visit types to iterate through
#' @param grouped_list the input for which to group variables
#' @param domain_tbl defaults to `pf_domains` in the specs folder; 
#'      @domain: the domain name; output will have this domain
#'      @default_tbl: the table to pull from 
#'      @field_name the field name to filter by; leave null if no filter
#'      @field_filter: the filtered codes
#' 
#' @return a returned list stratified by visit type
#' 

loop_through_visits <- function(cohort_tbl,
                                combine_sites = TRUE,
                                age_groups = NULL,
                                visit_type_tbl=read_codeset('pf_visit_types','ic'),
                                visit_tbl=cdm_tbl('visit_occurrence'),
                                site_list=list('stanford',
                                               'colorado'),
                                visit_list=c('inpatient','outpatient'),
                                grouped_list=c('person_id','start_date','end_date',
                                               'fu','site'),
                                domain_tbl=read_codeset('pf_domains','cccc')) {
  
  # apply age groupings if selected
  if(!is.data.frame(age_groups)){
    cohort_tbl <- cohort_tbl
    grouped_list <- grouped_list
  }else{
    cohort_tbl <- compute_age_groups(cohort_tbl = cohort_tbl,
                                         person_tbl = cdm_tbl('person'),
                                         age_groups = age_groups)
    grouped_list <- grouped_list %>% append('age_grp')}
  
  
  # iterates through visits
  visit_output <- list()
  for(j in 1:length(visit_list)) {
    
    # iterates through sites
    site_output <- list()
    for(k in 1:length(site_list)) {
      
      if(combine_sites) {
        site_list_thisrnd <- unlist(site_list)
      } else {site_list_thisrnd <- site_list[[k]]}
      
      # filters by site
      cohort_site <- cohort_tbl %>% filter(site%in%c(site_list_thisrnd))
      
      # pulls the visit_concept_id's that correspond to the visit_list
      visit_types <- 
        visit_type_tbl %>% 
        filter(visit_type %in% c(visit_list[[j]])) %>% 
        select(visit_concept_id) %>% pull()
      
      # narrows the visit time to cohort_entry and end date
      visits <- 
        cohort_site %>% 
        inner_join(
          select(visit_tbl,
                 person_id,
                 visit_occurrence_id,
                 visit_concept_id,
                 visit_start_date)
        ) %>% 
        filter(visit_concept_id %in% c(visit_types)) %>% 
        filter(visit_start_date >= start_date,
               visit_start_date <= end_date) %>% 
        compute_new(temporary=TRUE,
                    indexes=list('person_id'))
      
      # calls function `compute_pf` and adds site, with cohort_tbl filtered by site as input
      domain_compute <- compute_pf(cohort=cohort_site, pf_input_tbl=visits,
                                   grouped_list=grouped_list,
                                   domain_tbl=domain_tbl) %>% add_site()
      
      
      site_output[[k]] <- domain_compute
      
      if(combine_sites) break;
      
    }
    
    
    all_site <- reduce(.x=site_output,
                       .f=dplyr::union)
    
    visit_output[[paste0('pf_',config('cohort'),'_',(visit_list[j]))]] <- all_site
    
  }
  
  visit_output
  
}