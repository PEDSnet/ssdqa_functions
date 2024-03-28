
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

compute_age_groups_pcnt <- function(cohort_tbl,
                                    person_tbl,
                                    age_groups) {
  
  cohorts <- cohort_tbl %>% 
    inner_join(select(person_tbl,
                      patid,
                      birth_date)) %>% 
    mutate(age_ce = floor((start_date - birth_date)/365.25)) %>%
    collect_new()
  
  cohorts_grpd <- cohorts %>%
    cross_join(age_groups) %>%
    mutate(age_grp = case_when(age_ce >= min_age & age_ce <= max_age ~ group,
                               TRUE ~ as.character(NA))) %>%
    filter(!is.na(age_grp)) %>%
    right_join(cohorts) %>%
    select(-c(birth_date, min_age, max_age, group)) %>%
    mutate(age_grp = case_when(is.na(age_grp) ~ 'No Group',
                               TRUE ~ age_grp))
  
  copy_to_new(df = cohorts_grpd)
  
}

#' intake codeset to customize patient labels
#'
#' @param cohort_tbl table of cohort members with at least `person_id`, `start_date`, and `end_date`
#' @param codeset_meta a CSV file with metadata relating to a codeset with customized group labels
#'                     
#'                     this file should have `table`, `column`, and `file_name` columns

cohort_codeset_label_pcnt <- function(cohort_tbl,
                                      codeset_meta){
  
  codeset <- load_codeset(codeset_meta$file_name)
  
  filter_tbl <- select(cdm_tbl('encounter'), patid, encounterid, providerid, facilityid) %>%
    left_join(cdm_tbl(codeset_meta$table)) %>%
    rename('concept_id' = codeset_meta$column) %>%
    inner_join(codeset, by = 'concept_id') %>%
    select(person_id, flag) %>%
    distinct() %>%
    right_join(cohort_tbl, by = 'person_id') %>%
    mutate(flag = case_when(is.na(flag) ~ 'None',
                            TRUE ~ flag))
  
  
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

prepare_cohort_pcnt <- function(cohort_tbl,
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
                                    person_tbl = cdm_tbl('demographic'),
                                    age_groups = age_groups)}
  
  if(!is.data.frame(codeset)){
    final_cdst <- stnd
  }else{
    final_cdst <- cohort_codeset_label(cohort_tbl = stnd,
                                       codeset_meta = codeset)}
  
  final <- stnd %>%
    left_join(final_age) %>%
    left_join(final_cdst)
  
  return(final)
  
}


#' loops through visit types and sites to compute patient facts
#' 
#' @param cohort_tbl the tbl that comes from `prepare_pf`
#' @param check_func the base function for the check that needs to be executed across time; this argument
#'                   should be structured as the following, where cht is the cohort and t is the input data 
#'                   for the function:
#'                   
#'                   function(cht, t){check_function(param1 = cht, param2 = t, param3 = param3_input, ..., 
#'                   paramX = paramX_input)}
#'                   
#'                   all parameters for the base check function should be included if any defaults are not being
#'                   used
#' @param site_col the column in the data where the site variable can be found
#' 
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
loop_through_visits_pcnt <- function(cohort_tbl,
                                     check_func,
                                     site_col,
                                     #time=FALSE,
                                     visit_type_tbl=read_codeset('pf_visit_types_pcnt','cc'),
                                     visit_tbl=cdm_tbl('encounter'),
                                     site_list=list('stanford',
                                                    'colorado'),
                                     visit_list=c('inpatient','outpatient'),
                                     grouped_list=c('person_id','start_date','end_date',
                                                    'fu','site'),
                                     domain_tbl=read_codeset('pf_domains_pcnt','cccc')) {
  
  # iterates through visits
  visit_output <- list()
  for(j in 1:length(visit_list)) {
    
    # iterates through sites
    site_output <- list()
    for(k in 1:length(site_list)) {
      
      site_list_thisrnd <- site_list[[k]]
      
      # filters by site
      cohort_site <- cohort_tbl %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))
      
      # pulls the visit_concept_id's that correspond to the visit_list
      visit_types <- 
        visit_type_tbl %>% 
        filter(visit_type %in% c(visit_list[[j]])) %>% 
        select(enc_type) %>% pull()
      
      # narrows the visit time to cohort_entry and end date
      visits <- 
        cohort_site %>% 
        inner_join(
          select(visit_tbl,
                 patid,
                 encounterid,
                 enc_type,
                 admit_date)
        ) %>% 
        filter(enc_type %in% c(visit_types)) %>% 
        filter(admit_date >= start_date,
               admit_date <= end_date) %>% 
        compute_new(temporary=TRUE,
                    indexes=list('patid'))
      
      # execute function
      domain_compute <- check_func(cht = cohort_site,
                                   t = visits)
      
      site_output[[k]] <- domain_compute
      
    }
    
    
    all_site <- reduce(.x=site_output,
                       .f=dplyr::union)
    
    #visit_output[[paste0('pf_',config('cohort'),'_',(visit_list[j]))]] <- all_site
    visit_output[[visit_list[j]]] <- all_site
    
  }
  
  visit_output
  
}