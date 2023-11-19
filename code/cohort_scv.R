
### base function skeleton (will eventually go into loop_through_visits)
#' code_type either source or CDM to decide where theyre starting
#' 
#' the domain_tbl csv has info about which columns are the source_concept vs concept
#' 
#' code domain tells the function which line of the csv to use as reference
#' 
#' need to figure out how to clearly differentiate between the concept set param and
#' the codeset utilization param
#' 


check_code_dist <- function(cohort,
                            #grouped_list,
                            concept_set,
                            code_type,
                            code_domain,
                            domain_tbl){
  
  # pick the right domain/columns
  domain_filter <- domain_tbl %>% filter(domain == code_domain)
  concept_col <- domain_filter$concept_col
  source_col <- domain_filter$source_col
  
  if(code_type=='source') {
     final_col = source_col
  } else {final_col = concept_col}
  
  fact_tbl <- 
    cohort %>% 
    inner_join(cdm_tbl(code_domain)) %>% 
    inner_join(concept_set,
               by=setNames('concept_id',source_col)) %>% 
    select(all_of(group_vars(cohort)),
           all_of(concept_col),
           all_of(source_col)) %>% 
    rename('concept_id' = concept_col,
           'source_concept_id' = source_col)
  
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



### output generation skeleton

#' use the right denominator to sort the output and find the most common codes for filtering purposes
#' sort the output by the most commonly occurring codes; group where needed
#' 
#' thinking about options for output:
#' -- snomed --> icd as a lot of codes, even when excluding those that don't appear in the data,
#'    so its a little better to facet by snomed code (still a little messy for some codes)
#' -- icd --> snomed is easier to read without facetting, and facetting may make the graph a little
#'    sparse looking
#' -- how does this look in other source <--> concept mappings

ss_exp_nt <- function(process_output = scv_final,
                      output,
                      facet){
  
  # picking columns / titles 
  if(output == 'code_prop'){
    denom <-  'code_denom'
    title <-  'Proportion of Code Representation'
  }else if(output == 'site_code_prop'){
    denom <- 'site_code_denom'
    title <- 'Proportion of Code Representation by Site'
  }
  
  # sorting output to select the most commonly occurring codes and using those in the output
  # is this the best way to filter down the output?
  if(source){
    if(!is.null(facet)){
      filter <- process_output %>%
        select(icd_concept, denom) %>%
        distinct() %>%
        group_by(!!! syms(facet)) %>%
        arrange(desc(!! sym(denom))) %>%
        slice(1:10)
    }else{
      filter <- process_output %>%
        select(icd_concept, denom) %>%
        distinct() %>%
        arrange(desc(!! sym(denom))) %>%
        slice(1:20)
    }
  }else{
    if(!is.null(facet)){
      filter <- process_output %>%
        select(snomed_concept, denom) %>%
        distinct() %>%
        group_by(!!! syms(facet)) %>%
        arrange(desc(!! sym(denom))) %>%
        slice(1:10)
    }else{
      filter <- process_output %>%
        select(snomed_concept, denom) %>%
        distinct() %>%
        arrange(desc(!! sym(denom))) %>%
        slice(1:10)
    }
    
    final <- process_output %>% 
      inner_join(filter) 
    }
  
  # option 1: heatmap, option to facet by group, not legible for snomed --> icd mappings
  final %>% ggplot(aes(x = as.character(snomed_concept), y = as.character(icd_concept), fill = code_prop)) + 
    geom_tile() + 
    geom_text(aes(label = code_prop), size = 2, color = 'black') +
    scale_fill_gradient2(low = 'pink', high = 'maroon') + 
    facet_wrap((facet))
  
  # option 2: heatmap, already facets by snomed code (how to account for group?), would look very empty
  #           for icd --> snomed mappings
  final %>% ggplot(aes(x = as.character(snomed_concept), y = as.character(icd_concept), fill = code_prop)) + 
    geom_tile() + 
    geom_text(aes(label = code_prop), size = 2, color = 'black') +
    scale_fill_gradient2(low = 'pink', high = 'maroon') + 
    facet_wrap(~as.character(snomed_concept), scales = 'free')
  
  
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

loop_through_visits2 <- function(cohort_tbl,
                                 code_type,
                                 code_domain,
                                 concept_set,
                                 grouped_list,
                                 time=FALSE,
                                 collapse_sites=FALSE,
                                 visit_type_tbl=read_codeset('pf_visit_types','ic'),
                                 visit_tbl=cdm_tbl('visit_occurrence'),
                                 site_list=list('stanford','colorado'),
                                 visit_list=c('inpatient','outpatient'),
                                 domain_tbl=read_codeset('scv_domains', 'ccc')) {
  
  # iterates through visits
  visit_output <- list()
  for(j in 1:length(visit_list)) {
    
    visit_type_name <- visit_list[j]

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
        mutate(visit_type=visit_type_name) %>% 
        group_by(!!! syms(grouped_list)) %>% 
        compute_new(temporary=TRUE,
                    indexes=list('person_id'))
      
      # calls function `compute_pf` and adds site, with cohort_tbl filtered by site as input
      if(!time) {
        domain_compute <- check_code_dist(cohort = visits,
                                          code_type = code_type,
                                          code_domain = code_domain,
                                          concept_set = concept_set,
                                          grouped_list = grouped_list) #%>% add_site()
      } else {
        'time function tbd'
      }
      
      site_output[[k]] <- domain_compute

      if(collapse_sites) break;

    }
    
    visit_type <- visit_list[j]

    all_site <- reduce(.x=site_output,
                       .f=dplyr::union) %>% mutate(visit_type = visit_type)

    #visit_output[[paste0('pf_',config('cohort'),'_',(visit_list[j]))]] <- all_site
    visit_output[[visit_list[j]]] <- all_site

  }
  
  all_visit <- reduce(.x = visit_output,
                      .f = dplyr::union)

  all_visit
  #visit_output
  
}






#' putting it all together

scv_process <- function(cohort = cohort,
                        site_list = c('seattle','cchmc'),
                        domain_tbl=read_codeset('scv_domains', 'ccc'),
                        #study_name = 'glom',
                        code_type = 'source',
                        concept_set = dplyr::union(load_codeset('jia_codes'),
                                                   load_codeset('jia_codes_icd')),
                        code_domain = 'condition_occurrence',
                        multi_or_single_site = 'single',
                        collapse_sites = FALSE,
                        anomaly_or_exploratory='exploratory',
                        time = FALSE,
                        time_span = c('2014-01-01', '2023-01-01'),
                        #visit_types = c('outpatient','inpatient'),
                        age_groups = NULL
                        #codeset = NULL,
                        #visit_type_table=read_codeset('pf_visit_types','ic'),
                        ){
  
  # Set up grouped list
  
  grouped_list <- c('site', 'domain')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  if(is.data.frame(codeset)){grouped_list <- grouped_list %>% append('flag')}
  #if(is.vector(visit_types)) {grouped_list <- grouped_list %>% append('visit_type')}
  
  # Prep cohort
  
  cohort_prep <- prepare_pf(cohort = cohort, age_groups = age_groups, codeset = codeset) %>% 
    mutate(domain = code_domain) %>% 
    group_by(!!! syms(grouped_list)) %>% 
    group_by(domain, .add = TRUE)
  
  site_output <- list()
  
  if(! time) {
    if(anomaly_or_exploratory == 'exploratory') {
      
      for(k in 1:length(site_list)) {
        
        if(collapse_sites) {
          site_list_thisrnd <- unlist(site_list)
        } else {site_list_thisrnd <- site_list[[k]]}
        
        # filters by site
        cohort_site <- cohort_prep %>% filter(site%in%c(site_list_thisrnd))
      
      domain_compute <- check_code_dist(cohort = cohort_site,
                                        code_type = code_type,
                                        code_domain = code_domain,
                                        concept_set = concept_set,
                                        domain_tbl = domain_tbl) 
      
      site_output[[k]] <- domain_compute
      
      all_site <- reduce(.x=site_output,
                         .f=dplyr::union) 
    
      
      if(collapse_sites) break;
      
      }
    
      scv_tbl <- reduce(.x=site_output,
                         .f=dplyr::union)
    }
    
    else {'Anomaly TBD'}
  } else {'Time Stuff Here'}
  
 
}
