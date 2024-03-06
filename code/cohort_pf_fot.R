



#' loops through visit types and sites to compute facts over time
#' 
#' @param cohort_tbl a cohort tbl with a column for `site` and `person_id`, and `start_date` and `end_date`
#' @param age_groups CSV with age groups specified; see `prepare_pf`;
#' @param codeset CSV for stratifying by a particular codeset
#' @param group_list the variable by which the output should be grouped; 
#' defaults to `person_id`, `start_date`, `end_date`, `fu`, `site`; 
#' @param combine_sites a logical that tells the program to either loop through sites or run it once for all sites
#' @param visit_type_tbl The visit_concept_ids of interest for the analysis. `all` may be used in this field
#'                      to select every visit type; defaults to `fot_visit_types` in specs folder
#' @param visit_tbl the cdm visit_occurrence tbl; defaults to `cdm_tbl('visit_occurrence')`
#' @param site_list the sites to iterate through
#' @param visit_list the list of visit types to iterate through; defaults to `outpatient` and `all`
#' 
#' @return a returned list stratified by visit type
#' 


#### ADD THE DENOMINATOR FOR THE TOTAL NUMBER OF PATIENTS WITH THE VISIT TYPE

compute_fot_pf <- function(cohort,
                           age_groups=NULL,
                           codeset=NULL,
                                grouped_list=c('person_id','start_date','end_date',
                                               'fu','site'),
                                time_period='year',
                                time_span= c('2012-01-01','2022-12-31'),
                                #collapse_sites = FALSE,
                                visit_type_tbl=read_codeset('pf_visit_types_short','ic'),
                                visit_tbl=cdm_tbl('visit_occurrence'),
                                site_list=list('stanford',
                                               'colorado',
                                               'chop'),
                                visit_list=c('inpatient', 'outpatient'),
                                domain_tbl=read_codeset('pf_domains_short','cccc')
                                # grouped_list=c('person_id','start_date','end_date',
                                #                'fu','site'),
                                #domain_tbl=read_codeset('pf_domains','cccc')
) {
  

  site_list_v <- unlist(site_list)
  
  # cohort_full_prepped <- 
  #   cohort_tbl %>% filter(site %in% site_list_v) 
  # 
  # pf_full <- loop_through_visits_pf(cohort_tbl = cohort_full_prepped,
  #                              combine_sites=combine_sites,
  #                              visit_type_tbl=visit_type_tbl,
  #                              site_list=site_list,
  #                              visit_list=visit_list,
  #                              grouped_list=grouped_list,
  #                              domain_tbl=domain_tbl)
  
  final_results <- list()
  
  t1 <- seq(from=ymd(time_span[[1]]),to=ymd(time_span[[2]]),by=time_period)
  t2 <- ceiling_date(t1, time_period) - 1
  
    
    # narrows the visit time to cohort_entry and end date
  for(k in 1:length(t1)) {
    
    message(paste0('Starting ',t1[[k]]))
    
    target <- ymd(t1[[k]])
    
    baseline_start_date <- target
    baseline_end_date <- ceiling_date(target, time_period) - 1
    
    cohort_narrowed <-
      cohort %>% #collect() %>% 
      mutate(start_date = as_date(baseline_start_date),
             end_date = as_date(baseline_end_date))
    
    
    cohort_narrow_prepped <- cohort_narrowed %>%
      # prepare_pf(cohort_narrowed,
      #            age_groups = age_groups,
      #            codeset = codeset) %>% 
      filter(site %in% site_list_v) %>% mutate(time_period=start_date,
                                               time_increment=time_period)
    
    pf <- loop_through_visits(cohort_tbl = cohort_narrow_prepped,
                              time = TRUE,
                              visit_type_tbl=visit_type_tbl,
                              site_list=site_list,
                              visit_list=visit_list,
                              grouped_list=grouped_list,
                              domain_tbl=domain_tbl)
    
    pf_reduced <- dplyr::bind_rows(pf, .id='visit_type') %>% 
      mutate(time_increment=time_period)
    
    final_results[[k]] <- pf_reduced
    
  }
  
  # final_results[[paste0(check_string,'_meta')]] <- meta
  output = reduce(.x=final_results,
                  .f=dplyr::union)
  
  return(output)
  
}



#' loops through visit types and sites to compute patient facts
#' 
#' @param cohort_tbl the tbl that comes from `prepare_pf`
#' @param combine_sites a logical that tells the program to either loop through sites or run it once for all sites
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

loop_through_visits_pf <- function(cohort_tbl,
                                combine_sites = TRUE,
                                visit_type_tbl=read_codeset('pf_visit_types_short','ic'),
                                visit_tbl=cdm_tbl('visit_occurrence'),
                                site_list=list('stanford',
                                               'colorado'),
                                visit_list=c('inpatient','outpatient'),
                                grouped_list=c('person_id','start_date','site'),
                                domain_tbl=read_codeset('pf_domains_short','cccc')) {
  
  # iterates through visits
  visit_output <- list()
  for(j in 1:length(visit_list)) {
    
    # iterates through sites
    site_output <- list()
    for(k in 1:length(site_list)) {
      
      if(combine_sites) {
        site_list_thisrnd <- unlist(site_list)
      } else {site_list_thisrnd <- site_list[[k]]}
      
      #if(combine_sites) {k=length(unlist(site_list))} else {k=k}
      
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
      domain_compute <- compute_pf_for_fot(cohort=cohort_site, pf_input_tbl=visits,
                                           grouped_list=grouped_list,
                                           domain_tbl=domain_tbl) %>% add_site()
      
      
      site_output[[k]] <- domain_compute
      
      if(combine_sites) break;
      
    }
    
    
    all_site <- reduce(.x=site_output,
                       .f=dplyr::union)
    
    #visit_output[[paste0('pf_',config('cohort'),'_',(visit_list[j]))]] <- all_site
    visit_output[[paste0(visit_list[j])]] <- all_site
    
  }
  
  visit_output
  
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

compute_pf_for_fot <- function(cohort, pf_input_tbl,
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
      mutate(domain=domain_name) %>% ungroup()
    
    new_group <- grouped_list[! grouped_list %in% c('person_id')]
    
    pf_cohort_final <- 
      pf %>% right_join(select(cohort,
                               person_id)) %>% 
      distinct(person_id) %>% summarise(ct=n()) %>% pull()
    
    site_visit_ct_num <- 
      pf_input_tbl %>% summarise(ct=n_distinct(person_id)) %>% 
      pull()
    
    pf_final <- 
      pf %>% group_by(
        !!! syms(new_group)
      ) %>% group_by(domain, .add = TRUE) %>% 
        summarise(fact_ct_denom=n(),
                  sum_fact_ct=sum(total_strat_ct),
                  median_fact_ct=median(total_strat_ct)) %>% 
      #relocate(site) %>% 
      ungroup() 
    
    
    finalized <- 
      pf_final %>% 
      mutate(pt_ct_denom=pf_cohort_final,
             site_visit_ct=site_visit_ct_num) %>% collect()
    
    
    domain_results[[domain_name]] <- finalized
  }
  
  
  reduce(.x=domain_results,
         .f=dplyr::union)
  # domain_results_left_join <- 
  #   reduce(.x=domain_results,
  #          .f=left_join)
}
