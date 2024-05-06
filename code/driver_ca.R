.run <- function(){
  
  #' Get site name
  site_nm <- config('qry_site')
  
  #' Step 0: All patients with a visit between 2009-01-01 to 2023-12-31.
  
  step0 <- cdm_tbl('encounter') %>%
    filter(admit_date >= '2009-01-01' & admit_date <= '2023-12-31')
  
  init_sum(num_pts = distinct_ct(step0, id_col = 'patid'),
           step_number = 0,
           attrition_step = 'All patients with a visit between 2009-01-01 to 2023-12-31',
           site = site_nm)
  
  #' Step 1: Patients with at least 5 outpatient visits between 2009-01-01 to 2023-12-31
  
  step1 <- step0 %>% 
    filter(enc_type %in% c('AV', 'TH')) %>%
    group_by(patid) %>%
    summarise(n_visit = n_distinct(encounterid)) %>%
    filter(n_visit >= 5)
  
  append_sum(num_pts = distinct_ct(step1, id_col = 'patid'),
             step_number = 1,
             attrition_step = 'Patients with at least 5 outpatient visits between 2009-01-01 to 2023-12-31',
             site = site_nm)
  
  #' Step 2: Patients with at least 5 outpatient visits between 2009-01-01 to 2023-12-31, 
  #' where time between first and last visit is at least two years
  
  step2 <- cdm_tbl('encounter') %>%
    select(patid, encounterid, admit_date) %>%
    inner_join(select(step1, patid)) %>%
    group_by(patid) %>%
    mutate(min_visit = min(admit_date),
           max_visit = max(admit_date),
           visit_diff = (max_visit - min_visit) / 365.25) %>%
    filter(visit_diff >= 2) %>%
    ungroup() %>%
    distinct(patid) %>% compute_new()
  
  append_sum(num_pts = distinct_ct(step2, id_col = 'patid'),
             step_number = 2,
             attrition_step = 'Patients with at least 5 outpatient visits between 2009-01-01 to 2023-12-31, where time between first and last visit is at least two years',
             site = site_nm)
  
  #' Step 3: Patients diagnosed with any Type II diabetes code.
  
  step3 <- cdm_tbl('diagnosis') %>%
    inner_join(step2) %>%
    inner_join(load_codeset('t2d', 'cicc'), by = c('dx' = 'concept_code')) %>%
    left_join(cdm_tbl('demographic') %>% select(patid, birth_date)) %>%
    mutate(age_at_dx = (dx_date - birth_date) / 365.25) %>% compute_new()
  
  append_sum(num_pts = distinct_ct(step3, id_col = 'patid'),
             step_number = 3,
             attrition_step = 'Patients diagnosed with any Type II diabetes code',
             site = site_nm)
  
  #' Step 4: Patients at least 8 years old at time of first Type II diabetes code
  
  step4 <- step3 %>%
    group_by(patid) %>%
    filter(dx_date == min(dx_date),
           age_at_dx >= 8) %>% ungroup()
  
    ## Get index dates for cohort -- date of first T2D diagnosis
    index_dates <- step4 %>% select(patid, dx_date) %>% distinct() %>% rename('start_date' = dx_date)
  
  append_sum(num_pts = distinct_ct(step4, id_col = 'patid'),
             step_number = 4,
             attrition_step = 'Patients at least 8 years old at time of first Type II diabetes code',
             site = site_nm)
  
  #' Step 5: Patients with at least two Type II diabetes codes.
  
  step5 <- step3 %>% 
    inner_join(step4 %>% select(patid)) %>%
    select(diagnosisid, encounterid, patid, dx, dx_date) %>%
    distinct() %>%
    group_by(patid) %>%
    summarise(n_dx = n()) %>%
    filter(n_dx >= 2)
  
  append_sum(num_pts = distinct_ct(step5, id_col = 'patid'),
             step_number = 5,
             attrition_step = 'Patients with at least two Type II diabetes codes',
             site = site_nm)
  
  #' Step 6: Patients with an Hba1c lab in the record.
  
  step6 <- cdm_tbl('lab_result_cm') %>%
    inner_join(select(step5, patid)) %>%
    inner_join(load_codeset('hba1c', 'cicc'), by = c('lab_loinc' = 'concept_code')) %>% compute_new()
  
  append_sum(num_pts = distinct_ct(step6, id_col = 'patid'),
             step_number = 6,
             attrition_step = 'Patients with an Hba1c lab in the record',
             site = site_nm)
  
  #' Step 7: Patients with an Hba1c lab with valid results.
  
  step7 <- step6 %>%
    filter(!is.na(result_num) & ! (result_num < 0 & result_num > 100)) ## using this range just bc it is a %, may need to change
  
  append_sum(num_pts = distinct_ct(step7, id_col = 'patid'),
             step_number = 7,
             attrition_step = 'Patients with an Hba1c lab with valid results',
             site = site_nm)
  
  #' Step 8: Patients with an Hba1c lab >= 6.5%
  
  step8 <- step7 %>%
    filter(result_num >= 6.5)
  
  primary_cohort <- step8 %>% distinct(patid) %>%
    left_join(index_dates) %>% mutate(site = site_nm)
  
  append_sum(num_pts = distinct_ct(step8, id_col = 'patid'),
             step_number = 8,
             attrition_step = 'Patients with an Hba1c lab >= 6.5%',
             site = site_nm)
  
  ## END PRIMARY COHORT ##
  
  #' Step 9: Patients with at least two Hba1c labs >= 6.5%
  
  step9 <- step8 %>%
    select(patid, encounterid, lab_result_cm_id, lab_loinc, result_num) %>%
    distinct() %>%
    group_by(patid) %>%
    summarise(n_lab = n()) %>%
    filter(n_lab >= 2)
  
  append_sum(num_pts = distinct_ct(step9, id_col = 'patid'),
             step_number = 9,
             attrition_step = 'Patients with at least two Hba1c labs >= 6.5%',
             site = site_nm)
  
  #' Step 10: Patients with an outpatient prescription for antidiabetic drug
  
  step10 <- cdm_tbl('prescribing') %>%
    inner_join(select(step9, patid)) %>%
    inner_join(select(cdm_tbl('encounter'), encounterid, enc_type)) %>%
    filter(enc_type %in% c('AV', 'TH')) %>%
    inner_join(load_codeset('rx_diabetes'), by = c('rxnorm_cui' = 'concept_code'))
  
  append_sum(num_pts = distinct_ct(step10, id_col = 'patid'),
             step_number = 10,
             attrition_step = 'Patients with an outpatient prescription for antidiabetic drug',
             site = site_nm)
  
  output_sum(file = TRUE,
             name = paste0(config('qry_site'), '_diabetes_attrition'))
  
  
}