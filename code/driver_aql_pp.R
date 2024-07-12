# Vector of additional packages to load before executing the request
config_append('extra_packages', c())

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
  
  ## Unzip files
  
  zip_files <- list.files(path = paste0(base_dir, '/aql3-data/'),
                          pattern = '*.zip', full.names = TRUE, recursive = TRUE)
  
  lapply(zip_files, function(x) {unzip(x, exdir = paste0(base_dir, '/aql3-data/'))})
  
  ## Specialty Concordance
  cnc_sp_names <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                             name_string = 'specialty_names_')
  
  rslt$cnc_sp_vs <- read_codeset('pcornet_cnc_sp_vs', col_types='ccc') %>%
    mutate(vs_text=gsub(".*=","",valueset_item_descriptor))
  
  
  rslt$cnc_output_names <- cnc_sp_names %>%
    left_join(rslt$cnc_sp_vs, by = c('specialty_concept_id'='valueset_item'))
  
  
    output_tbl(data = rslt$cnc_output_names,
               name='cnc_sp_vs_names',
               db=TRUE,
               file=TRUE)
  
  cnc_sp_cts <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                           name_string = 'cnc_sp_ss_nt_') %>% mutate(site = toupper(site))
  
  output_tbl(cnc_sp_cts, 'cnc_sp_ss_nt_merge')
  
  ## Cohort Attrition
  
  attrition_files <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                                name_string = 'diabetes_attrition_') %>% mutate(site = toupper(site),
                                                                                qry_site = toupper(qry_site))
  
  output_tbl(attrition_files, 'attrition_merge')
  
  ca_output_step0 <- ca_process(attrition_tbl = attrition_files,
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                start_step_num = 0)
  
  output_tbl(ca_output_step0, 'ca_exp_nt_step0_merge')
  write_csv(ca_output_step0, paste0(base_dir, '/aql3-data/processed_files/ca_ms_exp_nt_step0.csv'))
  
  ca_output_step3 <- ca_process(attrition_tbl = attrition_files,
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                start_step_num = 3)
  
  output_tbl(ca_output_step3, 'ca_exp_nt_step3_merge')
  write_csv(ca_output_step3, paste0(base_dir, '/aql3-data/processed_files/ca_ms_exp_nt_step3.csv'))
  
  ### Multi-Site Anomaly
  ca_step0_int <- compute_dist_anomalies(df_tbl = ca_output_step0,
                                         grp_vars = c('step_number', 'attrition_step'),
                                         var_col = 'prop_retained_prior')
  
  ca_step0_final <- detect_outliers(df_tbl = ca_step0_int,
                                    p_input = 0.9,
                                    column_analysis = 'prop_retained_prior',
                                    column_variable = c('step_number', 'attrition_step'))
  
  output_tbl(ca_step0_final, 'ca_ms_anom_nt_step0_merge')
  write_csv(ca_step0_final, paste0(base_dir, '/aql3-data/processed_files/ca_ms_anom_nt_step0.csv'))
  
  ca_step3_int <- compute_dist_anomalies(df_tbl = ca_output_step3,
                                         grp_vars = c('step_number', 'attrition_step'),
                                         var_col = 'prop_retained_prior')
  
  ca_step3_final <- detect_outliers(df_tbl = ca_step3_int,
                                    p_input = 0.9,
                                    column_analysis = 'prop_retained_prior',
                                    column_variable = c('step_number', 'attrition_step'))
  
  output_tbl(ca_step3_final, 'ca_ms_anom_nt_step3_merge')
  write_csv(ca_step3_final, paste0(base_dir, '/aql3-data/processed_files/ca_ms_anom_nt_step3.csv'))
  
  ## Runtime computation
  
  runtime_files <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                              name_string = 'check_runtime_') %>% mutate(qry_site = toupper(qry_site))
  
  runtime_comps <- compute_runtime(merged_attrition_tbl = attrition_files,
                                   merged_runtime_tbl = runtime_files)
  
  output_tbl(runtime_comps$per_attrition, 'runtime_attrition_merge')
  output_tbl(runtime_comps$per_check, 'runtime_check_merge')
  output_tbl(runtime_comps$per_site, 'runtime_site_merge')
  
  write_csv(runtime_comps$per_site, paste0(base_dir, '/aql3-data/processed_files/runtime_site.csv'))
  
  ## Table 1s
  
  table1_files <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                             name_string = 'demographic_') %>%
    mutate(site = toupper(site)) %>%
    mutate(label = case_when(value %in% c('OT', 'NI', 'UN') | is.na(value) ~ 'Other/Unknown',
                             category == 'race' & value == '01' ~ 'American Indian or Alaska Native',
                             category == 'race' & value == '02' ~ 'Asian',
                             category == 'race' & value == '03' ~ 'Black or African American',
                             category == 'race' & value == '04' ~ 'Native Hawaiian or Other Pacific Islander',
                             category == 'race' & value == '05' ~ 'White',
                             category == 'race' & value == '06' ~ 'Multiple Race',
                             category == 'race' & value == '07' ~ 'Other/Unknown',
                             category == 'sex' & value == 'M' ~ 'Male',
                             category == 'sex' & value == 'F' ~ 'Female',
                             category == 'sex' & value == 'A' ~ 'Other/Unknown',
                             category == 'ethnicity' & value == 'Y' ~ 'Hispanic',
                             category == 'ethnicity' & value == 'N' ~ 'Non-Hispanic',
                             category == 'ethnicity' & value == 'R' ~ 'Other/Unknown',
                             TRUE ~ value))
  
  output_tbl(table1_files, 'table1_merge')
  write_csv(table1_files, paste0(base_dir, '/aql3-data/processed_files/table1.csv'))
  
  ## PF
  
  ### Across time
  pf_at_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                            name_string = 'pf_ss_exp_at_') %>% mutate(site = toupper(site))
  
  output_tbl(pf_at_merge, 'pf_ss_at_merge')
  write_csv(pf_at_merge, paste0(base_dir, '/aql3-data/processed_files/pf_ss_at.csv'))
  
  ### No time
  pf_nt_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                            name_string = 'pf_ss_exp_nt_') %>% mutate(site = toupper(site))
  
  output_tbl(pf_nt_merge, 'pf_ss_nt_merge')
  write_csv(pf_nt_merge, paste0(base_dir, '/aql3-data/processed_files/pf_ss_nt.csv'))
  
  ## EVP
  
  ### Across time
  evp_at_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                             name_string = 'evp_ss_exp_at_') %>% mutate(site = toupper(site))
  
  evp_ms_at_eucl <- ms_anom_euclidean(fot_input_tbl = evp_at_merge,
                                      grp_vars = c('site', 'variable'),
                                      var_col = 'prop_pt_variable')
  
  output_tbl(evp_at_merge, 'evp_exp_at_merge')
  write_csv(evp_at_merge, paste0(base_dir, '/aql3-data/processed_files/evp_ss_anom_at.csv'))
  
  output_tbl(evp_ms_at_eucl, 'evp_ms_anom_at_merge')
  write_csv(evp_ms_at_eucl, paste0(base_dir, '/aql3-data/processed_files/evp_ms_anom_at.csv'))
  
  ### No Time 
  evp_nt_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                             name_string = 'evp_ss_exp_nt_') %>% mutate(site = toupper(site))
  
  output_tbl(evp_nt_merge, 'evp_exp_nt_merge')
  write_csv(evp_nt_merge, paste0(base_dir, '/aql3-data/processed_files/evp_ms_exp_nt.csv'))
  
  ## CSD
  
  ### Across time
  #### Year
  csd_at_year_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                                  name_string = 'csd_ss_exp_at_year_') %>% mutate(site = toupper(site))
  
  csd_ms_at_year_eucl <- ms_anom_euclidean(fot_input_tbl = csd_at_year_merge %>% filter(time_increment == 'year'),
                                           grp_vars = c('site', 'concept_code'),
                                           var_col = 'prop_concept')
  
  output_tbl(csd_at_year_merge, 'csd_exp_at_year_merge')
  write_csv(csd_at_year_merge, paste0(base_dir, '/aql3-data/processed_files/csd_ms_exp_at_year.csv'))
  
  output_tbl(csd_ms_at_year_eucl, 'csd_ms_anom_at_year_merge')
  write_csv(csd_ms_at_year_eucl, paste0(base_dir, '/aql3-data/processed_files/csd_ms_anom_at_year.csv'))
  
  #### Quarter
  csd_at_qrtr_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                                  name_string = 'csd_ss_exp_at_quarter_') %>% mutate(site = toupper(site))
  
  csd_ms_at_qrtr_eucl <- ms_anom_euclidean(fot_input_tbl = csd_at_qrtr_merge %>% filter(time_increment == 'quarter'),
                                           grp_vars = c('site', 'concept_code'),
                                           var_col = 'prop_concept')
  
  site_list <- csd_at_qrtr_merge %>% distinct(site) %>% pull()
  
  anom_list <- list()
  
  for(i in site_list){
  
    csd_ss_anom_qrtr <- anomalize_ss_anom_at(fot_input_tbl = csd_at_qrtr_merge %>%
                                               filter(site == i),
                                             grp_vars = c('concept_code'),
                                             time_var = 'time_start',
                                             var_col = 'prop_concept')
    
    anom_list[[i]] <- csd_ss_anom_qrtr %>% mutate(site = i)
  
  }
  
  csd_ss_anom_qrtr_final <- reduce(.x = anom_list,
                                   .f = dplyr::union)
  
  output_tbl(csd_at_qrtr_merge, 'csd_exp_at_qrtr_merge')
  write_csv(csd_at_qrtr_merge, paste0(base_dir, '/aql3-data/processed_files/csd_exp_at_qrtr.csv'))
  
  output_tbl(csd_ms_at_qrtr_eucl %>% mutate(site = toupper(site)), 'csd_ms_anom_at_qrtr_merge')
  write_csv(csd_ms_at_qrtr_eucl, paste0(base_dir, '/aql3-data/processed_files/csd_ms_anom_at_qrtr.csv'))
  
  output_tbl(csd_ss_anom_qrtr_final, 'csd_ss_anom_at_qrtr_merge')
  write_csv(csd_ss_anom_qrtr_final, paste0(base_dir, '/aql3-data/processed_files/csd_ss_anom_at_qrtr.csv'))
  
  ### No Time
  csd_nt_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                             name_string = 'csd_ss_exp_nt_') %>% mutate(site = toupper(site))
  
  csd_ms_nt_anom_int <- compute_dist_anomalies(df_tbl = csd_nt_merge,
                                               grp_vars = c('variable', 'concept_code'), 
                                               var_col = 'prop_concept') 
  
  csd_ms_nt_anom_final <- detect_outliers(df_tbl = csd_ms_nt_anom_int,
                                          tail_input = 'both',
                                          p_input = 0.9,
                                          column_analysis = 'prop_concept',
                                          column_variable = 'concept_code')
  
  output_tbl(csd_nt_merge, 'csd_exp_nt_merge')
  write_csv(csd_nt_merge, paste0(base_dir, '/aql3-data/processed_files/csd_exp_nt.csv'))
  
  output_tbl(csd_ms_nt_anom_final, 'csd_ms_anom_nt_merge')
  write_csv(csd_ms_nt_anom_final, paste0(base_dir, '/aql3-data/processed_files/csd_ms_anom_nt.csv'))
  
  ## PES
  pes_nt1_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                              name_string = 'pes_ss_exp_nt1_') %>% mutate(site = toupper(site))
  
  output_tbl(pes_nt1_merge, 'pes_exp_nt1_merge')
  write_csv(pes_nt1_merge, paste0(base_dir, '/aql3-data/processed_files/pes_exp_nt1.csv'))
  
  pes_nt2_merge <- merge_csvs(output_directory = paste0(base_dir, '/aql3-data/results/'),
                              name_string = 'pes_ss_exp_nt2_') %>% mutate(site = toupper(site))
  
  output_tbl(pes_nt2_merge, 'pes_exp_nt2_merge')
  write_csv(pes_nt2_merge, paste0(base_dir, '/aql3-data/processed_files/pes_exp_nt2.csv'))
  
  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()
  
  message('Done.')
  
  invisible(rslt)
  
}
