
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
                               multi_or_single_site,
                               person_tbl = cdm_tbl('person'),
                               visit_tbl = cdm_tbl('visit_occurrence'),
                               provider_tbl = cdm_tbl('provider'),
                               #grouped_list,
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
                                                 'all_conds')){
  
  ## Filter to necessary domains
  domain_defs <- domain_defs %>% filter(label %in% domain_select)
  
  ## Add flags to cohorts
  bc_flag <- base_cohort %>%
    mutate(cohort_id = 'base_cohort') %>%
    collect()
  
  def_flag_list <- list()
  
  name_test <- names(alt_cohorts)
  
  if(is.null(name_test)){
    num_seq <- seq(1:length(alt_cohorts))
    names(alt_cohorts) <- num_seq
  }
    
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
  
  # Add site check
  site_filter <- check_site_type(cohort = cohort_combo,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  
  cohort_prep <- prepare_cohort(cohort_filter) %>% copy_to_new(df = .)
  
  
  fact_list <- list()
  
  ## Domain summaries
  domain_summary <- compute_domains_ssc(cohort = cohort_prep,
                                        site_col = site_col,
                                        grouped_list = c(site_col, 'person_id','start_date','end_date',
                                                         'fu', 'cohort_id'),
                                        domain_tbl = domain_defs)
  
  fact_list[['domain']] <- domain_summary %>% collect()
  
  ## Demographic summaries
  demo_summary <- compute_demographic_summary(cohort_tbl = cohort_prep,
                                              site_col = site_col,
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
                                         site_col = site_col,
                                         specialty_concepts = specialty_concepts,
                                         grouped_list = c(site_col, 'person_id','start_date','end_date',
                                                          'fu', 'cohort_id'),
                                         provider_tbl = provider_tbl,
                                         visit_tbl = visit_tbl)
    
    fact_list[['spec']] <- spec_visits %>% collect() %>% distinct()
  }
  
  ## Outcome summaries
  if(!is.null(outcome_concepts)){
    
    outcome_cts <- find_outcomes_ssc(cohort = cohort_prep,
                                     site_col = site_col,
                                     domain_tbl = domain_defs,
                                     outcome_concepts = outcome_concepts)
    
    fact_list[['outcome']] <- outcome_cts 
    
  }
  
  fact_list_final <- reduce(.x = fact_list,
                            .f = left_join) %>%
    mutate(across(where(is.numeric), ~replace_na(.,0))) %>%
    mutate(across(where(is.logical), ~replace_na(.,FALSE)))
  
  ## Output patient level data, will be summarized later
  return(fact_list_final)
  
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
                                        site_col,
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
    select(site_col, person_id, start_date, end_date, fu, cohort_id, age_cohort_entry:female) %>%
    collect()
  
  age_first_visit <- person_tbl %>%
    inner_join(cohort_tbl) %>%
    inner_join(select(visit_tbl, site, person_id, visit_start_date)) %>%
    group_by(!!sym(site_col), person_id, cohort_id, birth_date) %>%
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
                                site_col,
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
                  values_from=fact_ppy,
                  names_glue = "{domain}_ppy") %>% 
      #right_join(cohort) %>%
      #relocate(person_id) %>%
      compute_new(indexes=list('person_id'))
    
    domain_results[[domain_name]] <- ssc
  }
  
  domain_results_left_join <- 
    reduce(.x=domain_results,
           .f=left_join) %>% right_join(select(cohort, site_col, person_id, cohort_id))
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
                                  site_col,
                                  specialty_concepts,
                                  grouped_list,
                                  provider_tbl = cdm_tbl('provider'),
                                  visit_tbl = cdm_tbl('visit_occurrence')){
  
  spec_visits <- visit_tbl %>%
    inner_join(cohort) %>%
    inner_join(provider_tbl, by = c('site', 'provider_id')) %>%
    inner_join(specialty_concepts, by = c('specialty_concept_id' = 'concept_id')) %>%
    select(all_of(grouped_list), cohort_id, visit_occurrence_id)
  
  domain_tbl <- tibble('label' = 'specialty_visits',
                       'domain_tbl' = 'visit_occurrence',
                       'source_col' = 'visit_source_concept_id',
                       'concept_col' = 'visit_concept_id',
                       'date_col' = 'visit_start_date',
                       'filter_logic' = NA)
  
  spec_visit_ppy <- compute_domains_ssc(cohort = spec_visits,
                                        site_col = site_col,
                                        grouped_list = grouped_list,
                                        domain_tbl = domain_tbl)
  
  
}


#' Find presence of outcomes in cohort definitions
#'
#' @param cohort 
#' @param domain_tbl 
#' @param outcome_concepts 
#'
#' @return
#' @export
#'
#' @examples
find_outcomes_ssc <- function(cohort,
                              site_col,
                              domain_tbl,
                              outcome_concepts){
  
  concept_domain <- outcome_concepts %>%
    distinct(domain) %>% pull()
  
  otcm_list <- list()
  
  for(i in 1:length(concept_domain)){
    
    domain_info <- domain_tbl %>% filter(domain_tbl == concept_domain[i])
    
    colnm <- domain_info$concept_col
    
    outcome_present <- cdm_tbl(domain_info$domain_tbl) %>%
      inner_join(cohort) %>% 
      rename('join_col' = colnm) %>%
      inner_join(outcome_concepts, by = c('join_col' = 'concept_id')) %>%
      distinct(!!sym(site_col), person_id, variable) %>% collect() %>%
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
#' @param cohort_def_output 
#' 
#' @return
#' 
#' 

compute_cohort_summaries <- function(cohort_def_output){
  
  ## Get patient totals
  cohort_totals <- cohort_def_output %>% group_by(site, cohort_id) %>%
    summarise(cohort_total_pt = n())
  
  ## Flip table
  cohort_flip <- cohort_def_output %>%
    pivot_longer(cols = !c(site, person_id, start_date, end_date, cohort_id),
                 names_to = 'cohort_characteristic',
                 values_to = 'fact_value') %>%
    apply_cohort_labels()
  
  ## Summarise patient level data into medians (ppy vars) and proportions (demographic & outcome vars)
  find_medians <- cohort_flip %>% 
    filter(fact_group %in% c('Cohort Details', 'Utilization', 'Clinical Facts')) %>%
    select(-person_id) %>% group_by(site, cohort_id, cohort_characteristic, fact_group) %>% 
    summarise(fact_summary = median(fact_value)) %>% 
    mutate(cohort_characteristic = paste0('median_', cohort_characteristic)) %>%
    left_join(cohort_totals)
  
  find_props <- cohort_flip %>% 
    filter(fact_group %in% c('Demographics', 'Outcomes')) %>%
    group_by(site, cohort_id, cohort_characteristic, fact_group) %>%
    summarise(fact_summary = sum(fact_value)) %>%
    left_join(cohort_totals) %>%
    group_by(site, cohort_id, cohort_characteristic, fact_group) %>%
    summarise(fact_summary = (fact_summary/cohort_total_pt)) %>%
    mutate(cohort_characteristic = paste0('prop_', cohort_characteristic)) %>%
    left_join(cohort_totals)
  
  summ_tbl <- find_medians %>% union(find_props) %>% distinct()
  
  ## Find overlap between cohorts
  cht_overlap <- cohort_def_output %>%
    select(site, person_id, cohort_id) %>%
    pivot_wider(names_from = cohort_id,
                values_from = cohort_id) %>%
    relocate(base_cohort, .after = person_id) %>%
    unite('cohort_group', base_cohort:last_col(), na.rm = TRUE,
          sep = "&") %>%
    group_by(site, cohort_group) %>%
    summarise(group_ct = n()) %>%
    ungroup()
  
  otpt <- list('summary_values' = summ_tbl,
               'cohort_overlap' = cht_overlap)
  
  return(otpt)
  
}




compare_cohort_smd <- function(cohort_def_output){
  
  prep_tbl <- cohort_def_output %>%
    select(-c(start_date, end_date)) %>%
    mutate(across(where(is.logical), ~replace_na(.,FALSE)))
  
  var_vec <- names(prep_tbl)[!names(prep_tbl) %in% c('site', 'person_id', 'cohort_id')]
  
  alt_cht_list <- prep_tbl %>% filter(grepl('alt', cohort_id)) %>%
    distinct(cohort_id) %>%
    pull()
  
  smd_list <- list()
  
  for(i in alt_cht_list){
  
    smd_tbl <- prep_tbl %>%
      filter(cohort_id %in% c('base_cohort', i)) %>%
      summarize_at(
        .vars = vars(var_vec),
        .funs = list(smd = ~ smd(., g = cohort_id)$estimate)
      )
    
    smd_list[[i]] <- smd_tbl
    
  }
  
  smd_reduce <- bind_rows(smd_list, .id = 'cohort_id') %>%
    pivot_longer(cols = !cohort_id,
                 names_to = 'cohort_characteristic',
                 values_to = 'smd_vs_baseline') %>%
    cross_join(distinct(cohort_def_output, site)) %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, '_smd')) %>%
    apply_cohort_labels()
  
}


compare_cohort_mse <- function(cohort_def_output){
  
  summ_cht <- compute_cohort_summaries(cohort_def_output = cohort_def_output)
  
  gold_std <- summ_cht[[1]] %>%
    ungroup() %>%
    filter(cohort_id == 'base_cohort') %>%
    select(-c(cohort_id, cohort_total_pt)) %>%
    rename('gold_standard' = fact_summary)
  
  alt_comp <- summ_cht[[1]] %>%
    ungroup() %>%
    filter(cohort_id != 'base_cohort') %>%
    select(-c(cohort_total_pt)) %>%
    # pivot_longer(cols = !c(site, cohort_id),
    #              names_to = 'cohort_characteristic') %>%
    pivot_wider(names_from = cohort_id,
                values_from = fact_summary)
  
  comp_df <- gold_std %>%
    left_join(alt_comp) %>%
    mutate(across(matches("alt"), ~ (. - gold_standard)^2, .names = "{col}_dif")) %>%
    pivot_longer(cols = matches('_dif'),
                 names_to = 'cohort_id') %>%
    mutate(cohort_id = str_remove(cohort_id, '_dif'))
  
  site_level_grpd_mse <- comp_df %>%
    group_by(site, cohort_id, fact_group) %>%
    summarise(mse_sum = sum(value)) %>%
    group_by(cohort_id, fact_group) %>%
    mutate(n_grp = n()) %>%
    group_by(site, cohort_id, fact_group) %>%
    summarise(mse = mse_sum / n_grp)
  
  site_level_mse <- comp_df %>%
    group_by(site, cohort_id) %>%
    summarise(mse_sum = sum(value)) %>%
    group_by(cohort_id) %>%
    mutate(n_grp = n()) %>%
    group_by(site, cohort_id) %>%
    summarise(mse = mse_sum / n_grp)
  
  var_level_mse <- comp_df %>%
    group_by(cohort_id, cohort_characteristic, fact_group) %>%
    summarise(mse_sum = sum(value)) %>%
    group_by(cohort_id) %>%
    mutate(n_grp = n()) %>%
    group_by(cohort_id, cohort_characteristic, fact_group) %>%
    summarise(mse = mse_sum / n_grp)
  
  opt <- list('site_level' = site_level_mse,
              'site_level_grpd' = site_level_grpd_mse,
              'var_level' = var_level_mse)
  
  return(opt)
}



apply_cohort_labels <- function(df){
  
  df %>%
    mutate(fact_group = case_when(grepl('fu|age', cohort_characteristic) ~ 'Cohort Details',
                                  grepl('visit', cohort_characteristic) ~ 'Utilization',
                                  cohort_characteristic %in% c('white', 'unknown_race', 'mixed_race', 'hispanic', 'female',
                                                               'black', 'asian', 'other_race') ~ 'Demographics',
                                  grepl('ppy', cohort_characteristic) ~ 'Clinical Facts',
                                  TRUE ~ 'Outcomes'))
  
}