#' Function to prepare cohort and data for the visit+specialty concordance check
#' Not currently in use
prepare_conc <- function(cohort_tbl,
                         age_groups = NULL,
                         codeset = NULL) {
  
  ct <- cohort_tbl
  
  stnd <- 
    ct %>% 
    # mutate(fu = round((end_date - start_date + 1)/365.25,3)) %>% 
    # select(person_id, start_date, end_date, fu) %>% 
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

#' THIS is the main function I'm working with
#' function to compute fact + visit specialty concordance
#' @param cohort the cohort for which to look for events
#' @param conc_input_tbl the tbl that should be iterated --> not currently using, but would be passed in after prepare_conc
#' @param grouped_list a vector to group input by. Defaults to `site`
#' @param codeset_tbl table in the specs directory with the columns:
#'                        domain: name of the domain
#'                        default_tbl: name of the cdm_tbl
#'                        field_name: column name in the default_tbl for which to search the codeset concept_ids
#'                        codeset_name: name of a codeset in the specs directory
#' @param care_site boolean indicating whether to search care_site (at visit level) for specialty
#' @param provider boolean indicating whether to search provider (at visit level) for specialty
#' @return table with cols:
#'                        codeset_name: name of the codeset in the specs directory with event facts
#'                        specialty_concept_id: concept_id of specialty from either care_site or provider
#'                        specialty_concept_name: concept_name of specialty from either care_site or provider
#'                        num_visits: number of visits with the specialty+fact
#'                        ... any columns in the `grouped_list`
#' 

compute_conc <- function(cohort,
                         conc_input_tbl,
                         grouped_list=c('site'),
                         codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                         care_site,
                         provider) {
  
  codeset_results <- list()
  codeset_list <- split(codeset_tbl, seq(nrow(codeset_tbl)))
  grp_vis <- grouped_list %>% append('visit_occurrence_id')
  grp_vis_spec <- grp_vis %>% append(c('spec_flag','total_gp_ct'))
  grp_spec <- grouped_list%>%append(c('specialty_concept_id', 'specialty_concept_name'))
  
  
  for (i in 1:length(codeset_list)) {
    
    codeset_name = codeset_list[[i]][[4]]
    fact_col_name <- sym(codeset_list[[i]][[3]])
    
    message(paste0('Starting codeset ', codeset_list[[i]][4]))
    
    domain_tbl_use <- cdm_tbl(paste0(codeset_list[[i]][[2]]))
    # name the concept_id column in the codeset so it matches the cdm_tbl
    codes_to_use <- load_codeset(codeset_name) %>%
      rename(!!fact_col_name:=concept_id)
    # find occurrences of codeset codes with specialty
    visit_specs <- find_fact_spec_conc(cohort,
                                       fact_codes=codes_to_use,
                                       fact_tbl=domain_tbl_use,
                                       care_site,
                                       provider)
    
    # calculate the concordance per the grouping parameters
    conc_prop<- visit_specs %>%
      group_by(!!!syms(grp_spec))%>%
      summarise(num_visits=n()) %>%
      ungroup()%>%
      mutate(codeset_name=codeset_name)
    
    codeset_results[[codeset_name]] <- conc_prop
    
    
  }
  
  codeset_results_all <- 
    reduce(.x=codeset_results,
           .f=dplyr::union_all)
}

#' Function to find the visit_occurrences during which patient had a dx of interest + specialty of interest
#' @param cohort table with at least person_id
#' @param fact_codes codeset with _concept_id for the dx/px/etc of interest
#' @param fact_tbl cdm_tbl in which to search for fact_codes
#' @return table with all occurrences of the fact_codes for the cohort, and visit info only if visit was to a specialty in the codeset
find_fact_spec_conc <- function(cohort,
                                fact_codes,
                                fact_tbl,
                                care_site,
                                provider){
  
  
  fact_occurrences <- fact_tbl %>%
    inner_join(fact_codes) %>% # should join on the joincol since it was renamed, but may want to find a way to specify
    inner_join(select(cohort,person_id), by = 'person_id') %>%# may want to specify join columns
    select(-c(provider_id, site_id)) %>%# keeping out since may not match visit_occurrence
    inner_join(cdm_tbl('visit_occurrence'))
  
  if(care_site&provider){
    pv_spec <- fact_occurrences %>%
      left_join(select(cdm_tbl('provider'),c(provider_id, specialty_concept_id, specialty_concept_name)),
                by = 'provider_id')%>%
      rename(specialty_concept_id_pv=specialty_concept_id,
             specialty_concept_name_pv=specialty_concept_name)
    cs_spec <- fact_occurrences %>%
      left_join(select(cdm_tbl('care_site'),c(care_site_id, specialty_concept_id, specialty_concept_name)),
                by = 'care_site_id')%>%
      rename(specialty_concept_id_cs=specialty_concept_id,
             specialty_concept_name_cs=specialty_concept_name)
    
    spec_full <- pv_spec %>%
      full_join(cs_spec) %>%
      mutate(specialty_concept_id=case_when(!is.na(specialty_concept_id_pv)~specialty_concept_id_pv,
                                            !is.na(specialty_concept_id_cs)~specialty_concept_id_cs,
                                            TRUE~NA_integer_),
             specialty_concept_name=case_when(!is.na(specialty_concept_name_pv)~specialty_concept_name_pv,
                                              !is.na(specialty_concept_name_cs)~specialty_concept_name_cs,
                                              TRUE~NA_character_))
    
  }else if(provider&!care_site){
    spec_full <- fact_occurrences %>%
      left_join(select(cdm_tbl('provider'),c(provider_id, specialty_concept_id, specialty_concept_name)),
                by = 'provider_id')
  }else if(care_site&!provider){
    spec_full <- fact_occurrences %>%
      left_join(select(cdm_tbl('care_site'),c(care_site_id, specialty_concept_id, specialty_concept_name)),
                by = 'care_site_id')
  }
  return(spec_full)
  
}


#' loops through visit types and sites to compute concordance - NOT functional yet
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

loop_through_visits_conc <- function(cohort_tbl,
                                     time=FALSE,
                                     collapse_sites=FALSE,
                                     #combine_sites = TRUE,
                                     visit_type_tbl=read_codeset('conc_visit_types','ic'),
                                     visit_tbl=cdm_tbl('visit_occurrence'),
                                     site_list=list('stanford',
                                                    'colorado'),
                                     visit_list=c('inpatient','outpatient'),
                                     grouped_list=c('person_id','start_date','end_date',
                                                    'site'),
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
    visit_output[[visit_list[j]]] <- all_site
    
  }
  
  visit_output
  
}