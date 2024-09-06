


ssc_process <- function(base_cohort,
                        alt_cohorts,
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory = 'exploratory',
                        time = FALSE,
                        time_period = 'year',
                        time_span = c('2013-01-01', '2020-01-01'),
                        person_tbl = cdm_tbl('person'),
                        visit_tbl = cdm_tbl('visit_occurrence'),
                        provider_tbl = cdm_tbl('provider'),
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
  
  ## Generate patient level output
  pt_lv_chars <- compare_cohort_def(base_cohort = base_cohort,
                                    alt_cohorts = alt_cohorts,
                                    multi_or_single_site = multi_or_single_site,
                                    person_tbl = person_tbl,
                                    visit_tbl = visit_tbl,
                                    provider_tbl = provider_tbl,
                                    black_codes = black_codes,
                                    white_codes = white_codes,
                                    asian_codes = asian_codes,
                                    mixrace_codes = mixrace_codes,
                                    unknown_codes = unknown_codes,
                                    other_codes = other_codes,
                                    hispanic_codes = hispanic_codes,
                                    female_codes = female_codes,
                                    specialty_concepts = specialty_concepts,
                                    outcome_concepts = outcome_concepts,
                                    domain_defs = domain_defs,
                                    domain_select = domain_select)
  
  pt_lv_chars <- pt_lv_chars %>% replace_site_col()
  
  if(!time){
    
    if(anomaly_or_exploratory == 'exploratory'){
    
      ssc_tbl <- compute_cohort_summaries(cohort_def_output = pt_lv_chars)
      
    }else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'single'){
      
      ssc_tbl <- compare_cohort_smd(cohort_def_output = pt_lv_chars)
      
    }else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'multi'){
      
      ssc_tbl <- compare_cohort_mse(cohort_def_output = pt_lv_chars)
      
    }
    
  }
  
  return(ssc_tbl)
}