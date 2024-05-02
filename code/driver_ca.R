.run <- function(){
  
  #' Get site name
  config('qry_site', 'test')
  
  #' Step 0: All patients with a visit between 2009-01-01 to 2023-12-31.
  
  step0 <- cdm_tbl('encounter') %>%
    filter(admit_date >= '2009-01-01' & admit_date <= '2023-12-31')
  
  init_sum(num_pts = distinct_ct(step0, id_col = 'patid'),
           stepnum = 0,
           attrition_step = 'All patients with a visit between 2009-01-01 to 2023-12-31')
  
  #' Step 1: Patients with at least 5 outpatient visits between 2009-01-01 to 2023-12-31
  
  step1 <- step0 %>% 
    filter(enc_type %in% c('AV', 'TH')) %>%
    group_by(patid) %>%
    summarise(n_visit = n_distinct(encounterid)) %>%
    filter(n_visit >= 5) %>% compute_new()
  
  append_sum(num_pts = distinct_ct(step1, id_col = 'patid'),
             stepnum = 1,
             attrition_step = 'Patients with at least 5 outpatient visits between 2009-01-01 to 2023-12-31')
  
  #' Step 2: Patients with at least 5 outpatient visits between 2009-01-01 to 2023-12-31, 
  #' where time between first and last visit is at least two years
  
  step2 <- cdm_tbl('encounter') %>%
    select(patid, encounterid, admit_date) %>%
    inner_join(select(step1, patid)) %>%
    group_by(patid) %>%
    mutate(min_visit = min(admit_date),
           max_visit = max(admit_date),
           visit_diff = (max_visit - min_visit) / 365.25) %>%
    filter(visit_diff >= 2)
  
  append_sum(num_pts = distinct_ct(step2, id_col = 'patid'),
             stepnum = 2,
             attrition_step = 'Patients with at least 5 outpatient visits between 2009-01-01 to 2023-12-31, 
             where time between first and last visit is at least two years')
  
  #' Step 3: Patients diagnosed with any Type II diabetes code.
  
  #' Step 4: Patients at least 8 years old at time of first Type II diabetes code
  
  #' Step 5: Patients with at least two Type II diabetes codes.
  
  #' Step 6: Patients with an Hba1c lab in the record.
  
  #' Step 7: Patients with an Hba1c lab with valid results.
  
  #' Step 8: Patients with an Hba1c lab >= 6.5%
  
  ## END PRIMARY COHORT ##
  
  #' Step 9: Patients with at least two Hba1c labs >= 6.5%
  
  #' Step 10: Patients with an outpatient prescription for antidiabetic drug
  
  
  
  
  
  
  
  
}