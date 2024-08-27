
#' Reformat cohort definition tables 
#'
#' @param base_cohort table with the base cohort & inclusion criteria; must include
#'                    site, person_id, start_date, end_date
#' @param alt_cohorts list of each table with an alternative inclusion criteria definition 
#'                 applied to the base_cohort
#' @param cohort_string the name of the cohort, defaults to `config('cohort')`
#'
#' @return one combined dataframe with one row per patient in the original cohort
#'         with flags to show which cohort definitions apply to the patient. 
#'         
#'         flag columns will include `base_cohort` and `alt_cohort_#` columns with numbers 
#'         appended that correspond to the tables position in the `def_tbls` list
#'         
#'         `base_cohort` should equal 1 for all patients
#' 

compare_cohort_def <- function(base_cohort,
                               alt_cohorts,
                               pt_level_tbl = FALSE,
                               person_tbl = cdm_tbl('person'),
                               visit_tbl = cdm_tbl('visit_occurrence'),
                               provider_tbl = cdm_tbl('provider'),
                               grouped_list,
                               black_codes = c('8516'),
                               white_codes = c('8527'),
                               asian_codes = c('8515'),
                               mixrace_codes = c('44814659'),
                               unknown_codes = c('44814660', '44814650', '44814653'),
                               other_codes = c('44814649', '8657', '8557'),
                               hispanic_codes = c('38003563'),
                               female_codes = c('8532'),
                               specialty_concepts = NULL,
                               outcome_concepts = NULL,
                               domain_defs = read_codeset('gen_domains', 'ccccc'),
                               domain_select = c('inpatient_visits', 'outpatient_visits', 'emergency_visits',
                                                 'other_visits', 'all_px', 'prescription_medications',
                                                 'all_conds'),
                               cohort_string = config('cohort')){
  
  ## Filter to necessary domains
  domain_defs <- domain_defs %>% filter(label %in% domain_select)
  
  ## Add flags to cohorts
  bc_flag <- base_cohort %>%
    mutate(cohort_id = 'base_cohort') %>%
    collect()
  
  def_flag_list <- list()
  
  num_seq <- seq(1:length(alt_cohorts))
  
  names(alt_cohorts) <- num_seq
    
  for(i in 1:length(alt_cohorts)){
    
    number <- names(alt_cohorts[i])
    
    def_flag <- alt_cohorts[[i]] %>%
      mutate(cohort_id = paste0('alt_cohort_', number)) %>%
      collect()
    
    def_flag_list[[i]] <- def_flag
    
  }
  
  def_flag_final <- purrr::reduce(.x = def_flag_list,
                                  .f = dplyr::union)
  
  ## Combine cohorts with labels, prep for analysis
  cohort_combo <- dplyr::union(bc_flag, def_flag_final)
  
  cohort_totals <- cohort_combo %>% group_by(site, cohort_id) %>%
    summarise(cohort_total_pt = n())
  
  cohort_prep <- prepare_cohort(cohort_combo) %>% copy_to_new(df = .)
  
  
  fact_list <- list()
  
  ## Domain summaries
  domain_summary <- compute_domains_ssc(cohort = cohort_prep,
                                        grouped_list = c('person_id','start_date','end_date',
                                                         'fu', 'cohort_id'),
                                        domain_tbl = domain_defs)
  
  fact_list[['domain']] <- domain_summary %>% collect()
  
  ## Demographic summaries
  demo_summary <- compute_demographic_summary(cohort_tbl = cohort_prep,
                                              person_tbl = person_tbl,
                                              visit_tbl = visit_tbl,
                                              black_codes = black_codes,
                                              white_codes = white_codes,
                                              asian_codes = asian_codes,
                                              mixrace_codes = mixrace_codes,
                                              unknown_codes = unknown_codes,
                                              other_codes = other_codes,
                                              hispanic_codes = hispanic_codes,
                                              female_codes = female_codes)
  
  fact_list[['demo']] <- demo_summary
  
  ## Specialty summaries
  if(!is.null(specialty_concepts)){
    
    spec_visits <- find_specialty_visits(cohort = cohort_prep,
                                         specialty_concepts = specialty_concepts,
                                         grouped_list = c('person_id','start_date','end_date',
                                                          'fu', 'cohort_id'),
                                         provider_tbl = provider_tbl,
                                         visit_tbl = visit_tbl)
    
    fact_list[['spec']] <- spec_visits %>% collect() %>% select(-visit_occurrence_id) %>% distinct()
  }
  
  ## Outcome summaries
  if(!is.null(outcome_concepts)){
    
    outcome_cts <- find_outcomes_ssc(cohort = cohort_prep,
                                     domain_tbl = domain_defs,
                                     outcome_concepts = outcome_concepts)
    
    fact_list[['outcome']] <- outcome_cts
    
  }
  
  fact_list_final <- reduce(.x = fact_list,
                            .f = left_join) %>%
    mutate(across(where(is.numeric), ~replace_na(.,0)))
  
  ## Output patient level data if requested
  if(pt_level_tbl){
    ssc_pt_level_tbl <<- fact_list_final
  }
  
  
  ## Summarise patient level data into medians (ppy vars) and proportions (demographic & outcome vars)
  find_medians <- fact_list_final %>% select(site, cohort_id, where(is.numeric)) %>% 
    select(-person_id) %>% group_by(site, cohort_id) %>% 
    summarise(across(where(is.numeric), median)) %>% rename_with(~str_c("median_", .), .cols = is.numeric)
  
  find_props <- fact_list_final %>% select(site, cohort_id, where(is.logical)) %>%
    mutate(across(where(is.logical), ~as.numeric(.))) %>% 
    mutate(across(where(is.numeric), ~replace_na(.,0))) %>%
    group_by(site, cohort_id) %>%
    summarise(across(where(is.numeric), ~sum(.))) %>%
    left_join(cohort_totals) %>%
    group_by(site, cohort_id) %>%
    summarise(across(where(is.numeric), ~(./cohort_total_pt))) %>%
    rename_with(~str_c("prop_", .), .cols = is.numeric) %>%
    select(-prop_cohort_total_pt) %>%
    left_join(cohort_totals)
  
  summ_tbl <- find_medians %>% left_join(find_props) %>% distinct()
  
  return(summ_tbl)
}


#' Compute patient level fact summary
#'
#' @param cohort_tbl table with the original cohort & inclustion criteria; must include
#'                   site, person_id, start_date, end_date
#' @param person_tbl CDM `person` table
#' @param visit_tbl CDM `visit_occurrence` table
#' @param cohort_string the name of the cohort, defaults to `config('cohort')`
#' @param black_codes list of codes that indicate that a patient is Black/African-American
#'                    defaults to standard OMOP vocabulary -- `8516`
#' @param white_codes list of codes that indicate that a patient is White/Caucasian
#'                    defaults to standard OMOP vocabulary -- `8527`
#' @param asian_codes list of codes that indicate that a patient is Asian
#'                    defaults to standard OMOP vocabulary -- `8515`
#' @param mixrace_codes list of codes that indicate that a patient is Mixed Race
#'                      defaults to standard OMOP vocabulary -- `44814659`
#' @param unknown_codes list of codes that indicate that a patient's race is Unknown
#'                      defaults to standard OMOP vocabulary -- `44814660`, `44814650`, `44814653`
#' @param hispanic_codes list of codes that indicate that a patient is Hispanic or Latino
#'                       defaults to standard OMOP vocabulary -- `38003563`
#' @param female_codes list of codes that inidicate that a patient is Female
#'                     defaults to standard OMOP vocabulary -- `8532`
#'
#' @return one dataframe with one row for each patient with columns
#'         to show which facts apply to each patient:
#'         
#'         fu, age_cohort_entry, black, white, asian, 
#'         mixed, unknown, hispanic, female, visits_yr
#'         

compute_demographic_summary <- function(cohort_tbl,
                                        person_tbl = cdm_tbl('person'),
                                        visit_tbl = cdm_tbl('visit_occurrence'),
                                        cohort_string = config('cohort'),
                                        black_codes = c('8516'),
                                        white_codes = c('8527'),
                                        asian_codes = c('8515'),
                                        mixrace_codes = c('44814659'),
                                        unknown_codes = c('44814660', '44814650', '44814653'),
                                        other_codes = c('44814649', '8657', '8557'),
                                        hispanic_codes = c('38003563'),
                                        female_codes = c('8532')){
  
  demographic <- cohort_tbl %>%
    inner_join(person_tbl) %>%
    mutate(age_cohort_entry = round((start_date - birth_date) / 365.25, 2),
           black = case_when(race_concept_id %in% black_codes ~ TRUE,
                             TRUE ~ FALSE),
           white = case_when(race_concept_id %in% white_codes ~ TRUE,
                             TRUE ~ FALSE),
           asian = case_when(race_concept_id %in% asian_codes ~ TRUE,
                             TRUE ~ FALSE),
           mixed_race = case_when(race_concept_id %in% mixrace_codes ~ TRUE,
                             TRUE ~ FALSE),
           unknown_race = case_when(race_concept_id %in% unknown_codes ~ TRUE,
                               TRUE ~ FALSE),
           other_race = case_when(race_concept_id %in% other_codes ~ TRUE,
                                  TRUE ~ FALSE),
           hispanic = case_when(ethnicity_concept_id %in% hispanic_codes ~ TRUE,
                                TRUE ~ FALSE),
           female = case_when(gender_concept_id %in% female_codes ~ TRUE,
                              TRUE ~ FALSE)) %>%
    select(site, person_id, start_date, end_date, fu, cohort_id, age_cohort_entry:female) %>%
    collect()
  
  age_first_visit <- person_tbl %>%
    inner_join(cohort_tbl) %>%
    inner_join(select(visit_tbl, site, person_id, visit_start_date)) %>%
    group_by(site, person_id, cohort_id, birth_date) %>%
    summarise(min_visit = min(visit_start_date)) %>%
    mutate(age_first_visit = round((min_visit - birth_date) / 365.25, 2)) %>%
    select(-c(min_visit, birth_date)) %>%
    collect()
  
  summ_tbl <- demographic %>%
    left_join(age_first_visit)
  
  
}



#' Domains PPY
#'
#' @param cohort 
#' @param grouped_list 
#' @param domain_tbl 
#'
#' @return
#' @export
#'
#' @examples
compute_domains_ssc <- function(cohort,
                                grouped_list,
                                domain_tbl) {
  
  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))
  
  
  for (i in 1:length(domain_list)) {
    
    domain_name = domain_list[[i]][[1]]
    message(paste0('Starting domain ', domain_list[[i]][1]))
    
    ## checks to see if the table needs to be filtered in any way; 
    ## allow for one filtering operation
    
    domain_tbl_use <- cdm_tbl(domain_list[[i]][[2]])
    
    if(! is.na(domain_list[[i]][[6]])) {
      domain_tbl_use <- domain_tbl_use %>%
        filter(!! rlang::parse_expr(domain_list[[i]][[6]]))
    } else {domain_tbl_use <- domain_tbl_use}
    
    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    ssc <- 
      domain_tbl_use %>%
      inner_join(cohort) %>%
      filter(!!sym(domain_list[[i]][[5]]) >= start_date,
             !!sym(domain_list[[i]][[5]]) <= end_date) %>%
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_fact_ct=n()) %>% 
      ungroup() %>% 
      mutate(domain=domain_name) %>% 
      mutate(k_mult = case_when(fu < 0.1 ~ 100,
                                fu >= 0.1 & fu < 1 ~ 10,
                                TRUE ~ 1),
             fact_ppy=ifelse(fu != 0,round(total_fact_ct/(fu * k_mult),2),0)) %>% 
      #select(-c(total_strat_ct, k_mult)) %>% 
      select(person_id,
             domain,
             fact_ppy) %>%
      pivot_wider(names_from=domain,
                  values_from=fact_ppy) %>% 
      #right_join(cohort) %>%
      #relocate(person_id) %>%
      compute_new(indexes=list('person_id'))
    
    domain_results[[domain_name]] <- ssc
  }
  
  domain_results_left_join <- 
    reduce(.x=domain_results,
           .f=left_join) %>% right_join(cohort)
}


#' Specialty visits
#'
#' @param cohort 
#' @param specialty_concepts 
#' @param grouped_list 
#' @param provider_tbl 
#' @param visit_tbl 
#'
#' @return
#' @export
#'
#' @examples
find_specialty_visits <- function(cohort,
                                  specialty_concepts,
                                  grouped_list,
                                  provider_tbl = cdm_tbl('provider'),
                                  visit_tbl = cdm_tbl('visit_occurrence')){
  
  spec_visits <- visit_tbl %>%
    inner_join(cohort) %>%
    inner_join(provider_tbl, by = c('site', 'provider_id')) %>%
    inner_join(specialty_concepts, by = c('specialty_concept_id' = 'concept_id')) %>%
    select(site, person_id, start_date, end_date, fu, cohort_id, visit_occurrence_id)
  
  domain_tbl <- tibble('domain' = 'specialty_visits',
                       'date_field' = 'visit_start_date',
                       'default_tbl' = 'visit_occurrence',
                       'field_name' = NA,
                       'filter_logic' = NA)
  
  spec_visit_ppy <- compute_domains_ssc(cohort = spec_visits,
                                        grouped_list = grouped_list,
                                        domain_tbl = domain_tbl)
  
  
}


find_outcomes_ssc <- function(cohort,
                              domain_tbl,
                              outcome_concepts){
  
  concept_domain <- outcome_concepts %>%
    distinct(domain) %>% pull()
  
  otcm_list <- list()
  
  for(i in 1:length(concept_domain)){
    
    domain_info <- domain_tbl %>% filter(domain_tbl == concept_domain[i])
    
    colnm <- domain_info$concept_col
    
    outcome_present <- cdm_tbl(domain_info$domain_tbl) %>%
      rename('join_col' = colnm) %>%
      inner_join(outcome_concepts, by = c('join_col' = 'concept_id')) %>%
      distinct(site, person_id, variable) %>% collect() %>%
      mutate(has_outcome = TRUE)
    
    
    otcm_list[[i]] <- outcome_present
    
  }
  
  outcome_final <- reduce(.x = otcm_list,
                          .f = union)
  
  outcome_pivot <- outcome_final %>%
    pivot_wider(names_from = variable,
                values_from = has_outcome) %>%
    mutate(across(where(is.logical), ~replace_na(.,FALSE)))
  
}

#' Compute definition level summary
#'
#' @param flag_tbl table with patient-level definition based flags output 
#'                 by `format_cohort_output`
#' @param pt_summ_tbl table with patient-level fact based flags output 
#'                    by `compute_patient_summary`
#' @param cohort_string the name of the cohort within the study, 
#'                      defaults to `config('cohort')`
#' @param study_string the name of the study, defaults to `config('cohort')`
#'
#' @return one dataframe with summarized facts based on site, definition, and attribute
#'         
#'         fu, visits_yr, and age_cohort_entry are summarized by median;
#'         all other attributes are summed
#'         
#'         `prop_of_inc` where definition is `inclusion_def` will always equal 0
#' 

compute_definition_summary <- function(flag_tbl,
                                       pt_summ_tbl,
                                       cohort_string = config('cohort'),
                                       study_string = config('cohort')){
  # Pivot tables
  flag_new <- flag_tbl %>%
    pivot_longer(cols = c(inclusion_def, def_1, def_2),
                 names_to = 'definition',
                 values_to = 'flag') %>%
    filter(flag != 0)
  
  total_cohort <- flag_new %>%
    group_by(site, definition) %>%
    summarise(def_pts = n())
  
  pt_summ_new <- pt_summ_tbl %>%
    pivot_longer(cols = c(fu:visits_yr),
                 names_to = 'attribute',
                 values_to = 'value') %>%
    mutate(value = as.numeric(value))
  
  # Find inclusion criteria counts
  base_cohort_cts <- flag_new %>%
    filter(definition == 'inclusion_def') %>%
    left_join(pt_summ_new, multiple = 'all') %>%
    left_join(total_cohort) %>%
    group_by(site, definition, attribute) %>%
    reframe(base_ct = case_when(attribute != 'fu' & attribute != 'visits_yr' & 
                                   attribute != 'age_cohort_entry' ~ sum(value, na.rm = TRUE),
                                attribute == 'fu' | attribute == 'visits_yr' |
                                  attribute == 'age_cohort_entry' ~ median(value, na.rm = TRUE)),
            base_value = case_when(attribute != 'fu' & attribute != 'visits_yr' & 
                                   attribute != 'age_cohort_entry' ~ (sum(value, na.rm = TRUE)/def_pts),
                                   TRUE ~ base_ct)) %>%
    distinct() %>%
    select(-definition)
  
  # Compute full summary
  full_summ <- flag_new %>%
    left_join(pt_summ_new, multiple = 'all') %>%
    left_join(total_cohort, multiple = 'all') %>%
    group_by(site, definition, attribute, def_pts) %>%
    reframe(def_ct = case_when(attribute != 'fu' & attribute != 'visits_yr' & 
                                       attribute != 'age_cohort_entry' ~ sum(value, na.rm = TRUE),
                                 attribute == 'fu' | attribute == 'visits_yr' |
                                   attribute == 'age_cohort_entry' ~ median(value, na.rm = TRUE)),
            def_value = case_when(attribute != 'fu' & attribute != 'visits_yr' & 
                                   attribute != 'age_cohort_entry' ~ sum(value, na.rm = TRUE)/def_pts,
                                TRUE ~ def_ct)) %>%
    distinct() %>%
    left_join(base_cohort_cts, multiple = 'all') %>%
    mutate(pct_change = case_when(attribute != 'fu' & attribute != 'visits_yr' & 
                                     attribute != 'age_cohort_entry' ~ round((def_value - base_value) / base_value, 2),
                                   TRUE ~ round((def_ct - base_ct) / base_ct, 2)),
           study = study_string,
           cohort = cohort_string) 
  
  return(full_summ)
  
}

