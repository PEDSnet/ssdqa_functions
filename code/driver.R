# Vector of additional packages to load before executing the request
config_append('extra_packages', c('lubridate','tidyr','ggplot2','RColorBrewer',
                                  'timetk','stringr','plotly', 'gtExtras','ggiraph',
                                  'factoextra', 'qicharts2'))

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function() {

    setup_pkgs() # Load runtime packages as specified above

    message('Starting execution with framework version ',
          config('framework_version'))

  # Set up the step log with as many attrition columns as you need.
  # For example, this call sets up the log with a `persons` count that will be
  # required at each step.
  init_sum(cohort = 'Start', persons = 0)

  # By convention, accumulate execution results in a list rather than as
  # independent variables, in order to make returning the entire set easier
  rslt <- list()
  
  visit_list <- c('all', 'outpatient')
  
  cohort <- results_tbl(in_schema('ssdqa_output', 'cohort_glom_stud_1279')) %>%
    left_join(results_tbl(in_schema('ssdqa_glom', 'cohort_matched_stud_1279'))) %>%
    filter(cohort == 1) %>%
    select(person_id, start_date, end_date, fu, site) %>% compute_new()

  ## Single Site, Exploratory, No Time
  ss_exp_nt <- pf_process(cohort = cohort,
                          site_list = c('colorado', 'chop', 'stanford'),
                          time = FALSE,
                          multi_or_single_site = 'single',
                          collapse_sites = FALSE,
                          anomaly_or_exploratory = 'exploratory')
  
  output_tbl(ss_exp_nt, 'ss_exp_nt')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ## Single Site, Anomaly, No Time
  ss_anom_nt <- pf_process(cohort = cohort,
                           site_list = c('colorado', 'chop', 'stanford'),
                          time = FALSE,
                          multi_or_single_site = 'single',
                          collapse_sites = FALSE,
                          anomaly_or_exploratory = 'anomaly')
  
  output_tbl(ss_anom_nt, 'ss_anom_nt')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ## Single Site, Exploratory, Across Time
  ss_exp_at <- pf_process(cohort = cohort,
                          site_list = c('colorado', 'chop', 'stanford'),
                          time = TRUE,
                          multi_or_single_site = 'single',
                          collapse_sites = FALSE,
                          anomaly_or_exploratory = 'exploratory',
                          grouped_list = c('person_id','start_date','end_date',
                                          'site'))
  
  output_tbl(ss_exp_at, 'ss_exp_at')
  
  db_remove_table(name = in_schema(config('results_schema'), paste0('pf_fot_stud_1279')))
  
  ## Single Site, Anomaly, Across Time
  ss_anom_at <- pf_process(cohort = cohort,
                           site_list = c('colorado', 'chop', 'stanford'),
                           time = TRUE,
                           multi_or_single_site = 'single',
                           collapse_sites = FALSE,
                           anomaly_or_exploratory = 'anomaly',
                           grouped_list = c('person_id','start_date','end_date',
                                            'site'))
  
  output_tbl(ss_anom_at, 'ss_anom_at')
  
  db_remove_table(name = in_schema(config('results_schema'), paste0('pf_fot_stud_1279')))
  
  ## Multi-Site, Exploratory, No Time
  ms_exp_nt <- pf_process(cohort = cohort,
                          site_list = c('colorado', 'chop', 'stanford',
                                        'nemours', 'seattle'),
                          time = FALSE,
                          multi_or_single_site = 'multi',
                          collapse_sites = TRUE,
                          anomaly_or_exploratory = 'exploratory')
  
  output_tbl(ms_exp_nt, 'ms_exp_nt')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ## Multi-Site, Anomaly, No Time
  ms_anom_nt <- pf_process(cohort = cohort,
                           site_list = c('colorado', 'chop', 'stanford',
                                         'nemours', 'seattle'),
                           time = FALSE,
                           multi_or_single_site = 'multi',
                           collapse_sites = TRUE,
                           anomaly_or_exploratory = 'anomaly') %>% 
    as.data.frame() %>%
    rownames_to_column(var = 'site')
  
  output_tbl(ms_anom_nt, 'ms_anom_nt')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ## Multi-Site, Exploratory, Across Time
  ms_exp_at <- pf_process(cohort = cohort,
                          site_list = c('colorado', 'chop', 'stanford',
                                        'nemours', 'seattle'),
                          time = TRUE,
                          multi_or_single_site = 'multi',
                          collapse_sites = TRUE,
                          anomaly_or_exploratory = 'exploratory',
                          grouped_list = c('person_id','start_date','end_date',
                                           'site'))
  
  output_tbl(ms_exp_at, 'ms_exp_at')
  
  db_remove_table(name = in_schema(config('results_schema'), paste0('pf_fot_stud_1279')))
  
  ## Multi-Site, Anomaly, Across Time
  ms_anom_at <- pf_process(cohort = cohort,
                           site_list = c('colorado', 'chop', 'stanford',
                                         'nemours', 'seattle'),
                           time = TRUE,
                           multi_or_single_site = 'multi',
                           collapse_sites = TRUE,
                           anomaly_or_exploratory = 'anomaly',
                           grouped_list = c('person_id','start_date','end_date',
                                            'site'))
  
  output_tbl(ms_anom_at, 'ms_anom_at')
  
  db_remove_table(name = in_schema(config('results_schema'), paste0('pf_fot_stud_1279')))
  
  ## Single Site, Age Group Stratification
  ss_age <- pf_process(cohort = cohort,
                       site_list = c('colorado', 'chop', 'stanford'),
                          time = FALSE,
                          multi_or_single_site = 'single',
                          collapse_sites = FALSE,
                          anomaly_or_exploratory = 'exploratory',
                          age_groups = read.csv(file.path(base_dir, 'specs', 'age_group_definitions.csv')))
  
  output_tbl(ss_age, 'ss_age')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ## Multi-Site, Age Group Stratification
  ms_age <- pf_process(cohort = cohort,
                       site_list = c('colorado', 'chop', 'stanford',
                                     'nemours', 'seattle'),
                       time = FALSE,
                       multi_or_single_site = 'multi',
                       collapse_sites = TRUE,
                       anomaly_or_exploratory = 'exploratory',
                       age_groups = read.csv(file.path(base_dir, 'specs', 'age_group_definitions.csv')))
  
  output_tbl(ms_age, 'ms_age')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ## Single Site, Chronic Disease Stratification
  ss_cancer <- pf_process(cohort = cohort,
                          site_list = c('colorado', 'chop', 'stanford'),
                       time = FALSE,
                       multi_or_single_site = 'single',
                       collapse_sites = FALSE,
                       anomaly_or_exploratory = 'exploratory',
                       codeset = read.csv(file.path(base_dir, 'specs', 'codeset_cancer_metadata.csv')))
  
  output_tbl(ss_cancer, 'ss_cancer')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ss_cardiac <- pf_process(cohort = cohort,
                           site_list = c('colorado', 'chop', 'stanford'),
                          time = FALSE,
                          multi_or_single_site = 'single',
                          collapse_sites = FALSE,
                          anomaly_or_exploratory = 'exploratory',
                          codeset = read.csv(file.path(base_dir, 'specs', 'codeset_cardiac_metadata.csv')))
  
  output_tbl(ss_cardiac, 'ss_cardiac')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ## Multi-Site, Chronic Disease Stratification
  ms_cancer <- pf_process(cohort = cohort,
                          site_list = c('colorado', 'chop', 'stanford',
                                        'nemours', 'seattle'),
                          time = FALSE,
                          multi_or_single_site = 'multi',
                          collapse_sites = TRUE,
                          anomaly_or_exploratory = 'exploratory',
                          codeset = read.csv(file.path(base_dir, 'specs', 'codeset_cancer_metadata.csv')))
  
  output_tbl(ms_cancer, 'ms_cancer')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }
  
  ms_cardiac <- pf_process(cohort = cohort,
                           site_list = c('colorado', 'chop', 'stanford',
                                         'nemours', 'seattle'),
                           time = FALSE,
                           multi_or_single_site = 'multi',
                           collapse_sites = TRUE,
                           anomaly_or_exploratory = 'exploratory',
                           codeset = read.csv(file.path(base_dir, 'specs', 'codeset_cardiac_metadata.csv')))
  
  output_tbl(ms_cardiac, 'ms_cardiac')
  
  for(i in 1:length(visit_list)){
    db_remove_table(name = in_schema(config('results_schema'), paste0(visit_list[i], '_stud_1279')))
  }

  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()

  message('Done.')

  invisible(rslt)

}
