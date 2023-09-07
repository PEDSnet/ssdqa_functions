
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

#' intake codeset to customize patient labels

cohort_codeset_label <- function(cohort_tbl,
                                 codeset_meta){
  
  codeset <- load_codeset(codeset_meta$file_name)
  
  filter_tbl <- select(cdm_tbl('visit_occurrence'), person_id, visit_occurrence_id, provider_id, care_site_id) %>%
    left_join(cdm_tbl(codeset_meta$table)) %>%
    rename('concept_id' = codeset_meta$column) %>%
    inner_join(codeset, by = 'concept_id') %>%
    select(person_id, flag) %>%
    distinct() %>%
    right_join(cohort_tbl, by = 'person_id') %>%
    mutate(flag = case_when(is.na(flag) ~ 'None',
                             TRUE ~ flag))
  
  
}

#' prepare for pf function
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

prepare_pf <- function(cohort_tbl,
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
#' @param one_site_output a logical that tells the program to either loop through sites or run it once for all sites combined, 
#' or if it is just one site
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
                                time=FALSE,
                                collapse_sites=FALSE,
                                #combine_sites = TRUE,
                                visit_type_tbl=read_codeset('pf_visit_types','ic'),
                                visit_tbl=cdm_tbl('visit_occurrence'),
                                site_list=list('stanford',
                                               'colorado'),
                                visit_list=c('inpatient','outpatient'),
                                grouped_list=c('person_id','start_date','end_date',
                                               'fu','site'),
                                domain_tbl=read_codeset('pf_domains_short','cccc')) {
  
  # iterates through visits
  visit_output <- list()
  for(j in 1:length(visit_list)) {
    
    # iterates through sites
    site_output <- list()
    for(k in 1:length(site_list)) {
      
      if(collapse_sites) {
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
      if(!time) {
        domain_compute <- compute_pf(cohort=cohort_site, pf_input_tbl=visits,
                                     grouped_list=grouped_list,
                                     domain_tbl=domain_tbl) %>% add_site()
      } else {
        domain_compute <- compute_pf_for_fot(cohort=cohort_site, pf_input_tbl=visits,
                                             grouped_list=grouped_list,
                                             domain_tbl=domain_tbl) %>% add_site()
      }
      
      
      
      site_output[[k]] <- domain_compute

      if(collapse_sites) break;
      
    }
    
    
    all_site <- reduce(.x=site_output,
                       .f=dplyr::union)
    
    #visit_output[[paste0('pf_',config('cohort'),'_',(visit_list[j]))]] <- all_site
    visit_output[[paste0('pf_',visit_list[j])]] <- all_site
    
  }
  
  visit_output
  
}


#' combine PF output into one table per study
#'
#' @param study_abbr string that corresponds with the study/studies of interest
#' @param visit_type_list the types of visits in the PF output; defaults to:
#'                        inpatient, outpatient, other_visit, all
#'
#' @return a list of dataframes that contain all sites and all domains for each study
#'         listed in `study_abbr`. the resulting dataframes will also have an
#'         age grouping column added.
#'         
#' 
combine_study_facts <- function(pf_tbl,
                                study_abbr,
                                domain_list,
                                time = FALSE,
                                visit_type_list = list('inpatient','outpatient',
                                                       'other_visit','all')) {
  final_list <- list()
  
  pf_visits <- 
    str_remove(names(pf_tbl),'(pf_)')
  
  names(pf_tbl) <- str_remove(names(pf_tbl), '(pf_)')
  
  for(i in 1:length(visit_type_list)) {
    
    possible_cols <-  domain_list %>% 
      select(domain) %>% c()
    
    visit_type_pulled <- 
      paste0(visit_type_list[[i]])
    
    pf_tbl_visittype <- 
      pf_tbl[[visit_type_pulled]]
    
    # visit_type_pulled <- 
    #   get_results(paste0(visit_type_list[[i]]))  %>% 
    #   select(-any_of('fact_ct_strat')) 
    
    tbl_cols <- pf_tbl_visittype %>% colnames()
    
    selected_cols <- 
      intersect(possible_cols[[1]],
                tbl_cols)
    
    if(!time){
    mutated_tbl <- 
      pf_tbl_visittype %>% 
      pivot_longer(cols=all_of(selected_cols),
                   names_to='var_name',
                   values_to='var_val') %>%
      mutate(var_ever=case_when(!is.na(var_val)~1L,
                                TRUE~0L)) %>% 
      mutate(var_val=case_when(is.na(var_val) ~ 0,
                                TRUE ~ var_val)) %>% 
       mutate(study=study_abbr,
              visit_type=visit_type_pulled)
    } else {mutated_tbl <- pf_tbl_visittype %>% mutate(study=study_abbr,
                                                 visit_type=visit_type_pulled)}
    
    
    final_list[[i]] <- mutated_tbl
    
  }
  
  final_list_reduce <- reduce(.x=final_list,
                              .f=dplyr::union)
  final_list_reduce
  
}


## Get results_tbl

get_results <- function(tbl_name) {
  
  rslt <- results_tbl(tbl_name) %>% collect()
  
}


#' output a list of tables to the database 
#' 
#' @param output_list list of tables to output
#' @param append logical to determine if you want to append if the table exists
#' 
#' @return tables output to the database; if 
#' table already exists, it will be appended
#' 

output_list_to_db <- function(output_list,
                              append=FALSE) {
  
  
  if(append) {
    
    for(i in 1:length(output_list)) {
      
      output_tbl_append(data=output_list[[i]],
                        name=names(output_list[i]))
      
    }
    
  } else {
    
    for(i in 1:length(output_list)) {
      
      output_tbl(data=output_list[[i]],
                 name=names(output_list[i]))
      
    }
    
  }
  
}

#' output table to database if it does not exist, or
#' append it to an existing table with the same name if it does
#' 
#' @param data the data to output
#' @param name the name of the table to output 
#' 
#' Parameters are the same as `output_tbl`
#' 
#' @return The table as it exists on the databse, with the new data
#' appended, if the table already existts.
#' 

output_tbl_append <- function(data, name = NA, local = FALSE,
                              file = ifelse(config('execution_mode') !=
                                              'development', TRUE, FALSE),
                              db = ifelse(config('execution_mode') !=
                                            'distribution', TRUE, FALSE),
                              results_tag = TRUE, ...) {
  
  if (is.na(name)) name <- quo_name(enquo(data))
  
  if(db_exists_table(config('db_src'),intermed_name(name,temporary=FALSE))) {
    
    tmp <- results_tbl(name) %>% collect_new 
    new_tbl <- 
      dplyr::union(tmp,
                   data %>% collect())
    output_tbl(data=new_tbl,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  } else {
    output_tbl(data=data,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  }
  
  
}