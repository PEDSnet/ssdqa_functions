
### base function skeleton (will eventually go into loop_through_visits)
#' code_type either source or CDM to decide where theyre starting
#' 
#' the domain_tbl csv has info about which columns are the source_concept vs concept
#' 
#' code domain tells the function which line of the csv to use as reference
#' 
#' need to figure out how to clearly differentiate between the concept set param and
#' the codeset utilization param

check_code_dist <- function(cohort,
                            concept_set = load_codeset('jia_codes_icd') %>% distinct(),
                            code_type = 'source',
                            code_domain = 'condition_occurrence',
                            domain_tbl = read_codeset('scv_domains', 'ccc'),
                            grouped_list){
  
  # pick the right domain/columns
  domain_filter <- domain_tbl %>% filter(domain == code_domain)
  concept_col <- domain_filter$concept_col
  source_col <- domain_filter$source_col
  
  # pull the table
  fact_tbl <- cdm_tbl(code_domain) %>% 
    inner_join(cohort, by = c('site', 'person_id')) %>%
    select(site, concept_col, source_col) %>%
    rename('concept_id' = concept_col)
  
  if(code_type == 'source'){
    
    # pull the icd -> snomed mappings
    icd_snomed_map <- vocabulary_tbl('concept_relationship') %>%
      filter(relationship_id == 'Maps to') %>%
      inner_join(concept_set, by = c('concept_id_1' = 'concept_id')) %>%
      select(concept_id_1, concept_id_2) %>%
      rename('icd_concept' = concept_id_1,
             'snomed_concept' = concept_id_2) %>%
      compute_new()
    
    # filter the fact table down to just the appropriate mappings
    fact_tbl_filter <- icd_snomed_map %>%
      inner_join(fact_tbl %>% select(-source_col), by = c('snomed_concept' = 'concept_id')) %>%
      compute_new()
    
    # compute overall counts + proportions for each icd code and icd/snomed code pair
    # join back into mappings to pull in any NAs (heat map best viz?? looks ok but a lot of blanks)
    overall <- fact_tbl_filter %>%
      select(-site) %>%
      group_by(icd_concept) %>%
      mutate(code_denom = n()) %>%
      group_by(icd_concept, snomed_concept) %>%
      mutate(code_ct = n(),
             code_prop = round((as.numeric(code_ct) / as.numeric(code_denom)), 2)) %>% 
      full_join(icd_snomed_map) %>%
      distinct() %>% compute_new()
    
    # compute overall counts + proportions for each site/icd pait and site/icd/snomed combo
    # NAs will appear in final step after left joining with the overall counts
    by_site <- fact_tbl_filter %>%
      group_by(site, icd_concept) %>%
      mutate(site_code_denom = n()) %>%
      group_by(site, icd_concept, snomed_concept) %>%
      mutate(site_code_ct = n(),
             site_code_prop = round(as.numeric(site_code_ct) / as.numeric(site_code_denom), 2)) %>% 
      distinct() %>% compute_new()
    
    # compute overall counts + proportions for each group list/icd combo and group list/icd/snomed combo
    # NAs will appear in final step after left joining with the overall counts
    
    # by_group <- fact_tbl_filter %>%
    #   group_by(all_of(grouped_list), icd_concept) %>%
    #   mutate(grp_code_denom = n()) %>%
    #   group_by(all_of(grouped_list), icd_concept, snomed_concept) %>%
    #   mutate(grp_code_ct = n(),
    #          grp_code_prop = round(as.numeric(grp_code_ct) / as.numeric(grp_code_denom), 2)) %>% 
    #   distinct() %>% compute_new()
    
    
    # left join overall, site, and group counts together
    scv_final <- overall %>%
      left_join(by_site) %>%
      #left_join(by_group) %>% 
      distinct()
    
    
  }else{
    # pull the snomed -> icd mappings
    snomed_icd_map <- vocabulary_tbl('concept_relationship') %>%
      filter(relationship_id == 'Mapped from') %>%
      inner_join(concept_set, by = c('concept_id_1' = 'concept_id')) %>%
      select(concept_id_1, concept_id_2) %>%
      rename('snomed_concept' = concept_id_1,
             'icd_concept' = concept_id_2) %>%
      compute_new()
    
    # filter fact table down to necessary mappings
    fact_tbl_filter <- fact_tbl %>%
      inner_join(concept_set, by = c('concept_id')) %>%
      select(site, concept_id, source_col) %>%
      rename('icd_concept' = source_col,
             'snomed_concept' = concept_id) %>%
      inner_join(snomed_icd_map, by = c('icd_concept', 'snomed_concept')) %>% 
      compute_new()
    
    # compute counts + proportions for each snomed code and snomed/icd combo
    # join back into mappings to pull in NAs (a lot -- need to figure out best visualization here)
    overall <- fact_tbl_filter %>%
      select(-site) %>%
      group_by(snomed_concept) %>%
      mutate(code_denom = n()) %>%
      group_by(icd_concept, snomed_concept) %>%
      mutate(code_ct = n(),
             code_prop = round((as.numeric(code_ct) / as.numeric(code_denom)), 2)) %>%
      full_join(snomed_icd_map, by = c('icd_concept', 'snomed_concept')) %>%
      distinct() %>% compute_new()
    
    # compute counts + proportions for each site/snomed combo and site/snomed/icd combo
    # NAs will appear in final step when left joining with overall counts
    by_site <- fact_tbl_filter %>%
      group_by(site, snomed_concept) %>%
      mutate(site_code_denom = n()) %>%
      group_by(site, snomed_concept, icd_concept) %>%
      mutate(site_code_ct = n(),
             site_code_prop = round(as.numeric(site_code_ct) / as.numeric(site_code_denom), 2)) %>% 
      distinct() %>% compute_new()
    
    # compute counts + proportions for each grouped list/snomed combo and grouped list/snomed/icd combo
    # NAs will appear in final step when left joining with overall counts
    by_group <- fact_tbl_filter %>%
      group_by(all_of(grouped_list), snomed_concept) %>%
      mutate(site_code_denom = n()) %>%
      group_by(all_of(grouped_list), snomed_concept, icd_concept) %>%
      mutate(grp_code_ct = n(),
             grp_code_prop = round(as.numeric(grp_code_ct) / as.numeric(grp_code_denom), 2)) %>% 
      distinct() %>% compute_new()
    
    # left join overall, site, and group computations together
    scv_final <- overall %>%
      left_join(by_site) %>%
      #left_join(by_group) %>% 
      distinct()
    
  }
}



### output generation skeleton

output_gen <- function(process_output,
                       output,
                       facet){
  
  ss_exp_nt <- process_output %>%
    arrange(code_denom) %>%
    head(30) %>%
    ggplot(aes(y = as.character(icd_concept), x = as.character(snomed_concept), fill = !! sym(output))) +
    geom_tile() +
    facet_wrap((facet))
  
  
}










#' create csv with domain to column mappings for concept and source fields
#'  -- user can edit this as needed if their field names are different or if they want to add more fields
#' 
#' argument to select which domain the codeset belongs to (based off csv file)
#' 
#' for this domain, use code_type arg to decide which column to use
#'  -- need to enforce structure for codeset so we can reference concept_id/concept_code columns reliably
#' 
#' inner join cohort to domain tbl, inner join codeset to that tbl
#' 
#' get denominator -- group by *_concept_id OR *_source_concept_id depending on selection and count (code_denom)
#' 
#' for total counts, group by *_concept_id and *_source_concept_id and count (code_ct, code_prop)
#' 
#' for site counts, add site to this grouping (site_code_ct, site_code_prop)
#' 
#' for more specific group counts, append to grouped list (it will separate by visit type automatically using the loop)
#' (grp_code_ct, grp_code_prop)
#' 
#' 
#' 
#' 