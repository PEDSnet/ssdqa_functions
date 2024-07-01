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
  
  ## Specialty Concordance
  rslt$cnc_sp_vs<-read_codeset('pcornet_cnc_sp_vs', col_types='ccc') %>%
    mutate(vs_text=gsub(".*=","",valueset_item_descriptor))
  rslt$cnc_output_names<-results_tbl('cnc_sp_specialty_names_loc_mugjx', results_tag = FALSE)%>%collect()%>%
    left_join(rslt$cnc_sp_vs, by = c('specialty_concept_id'='valueset_item'))%>%
    output_tbl(name='cnc_sp_vs_names',
               db=FALSE,
               file=TRUE)
  
  ## Cohort Attrition
  
  attrition_files <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                                name_string = 'diabetes_attrition_')
  
  ca_output_step0 <- ca_process(attrition_tbl = attrition_files,
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                start_step_num = 0)
  
  output_tbl(ca_output_step0, 'ca_ms_exp_nt_step0', file = TRUE)
  
  ca_output_step3 <- ca_process(attrition_tbl = attrition_files,
                                multi_or_single_site = 'multi',
                                anomaly_or_exploratory = 'exploratory',
                                start_step_num = 3)
  
  output_tbl(ca_output_step3, 'ca_ms_exp_nt_step3', file = TRUE)
  
  ### Multi-Site Anomaly
  ca_step0_int <- compute_dist_anomalies(df_tbl = ca_output_step0,
                                         grp_vars = c('step_number', 'attrition_step'),
                                         var_col = 'prop_diff_prior')
  
  ca_step0_final <- detect_outliers(df_tbl = ca_step0_int,
                                    p_input = 0.9,
                                    column_analysis = 'prop_diff_prior',
                                    column_variable = c('step_number', 'attrition_step'))
  
  output_tbl(ca_step0_final, 'ca_ms_anom_nt_step0', file = TRUE)
  
  ca_step3_int <- compute_dist_anomalies(df_tbl = ca_output_step3,
                                         grp_vars = c('step_number', 'attrition_step'),
                                         var_col = 'prop_diff_prior')
  
  ca_step3_final <- detect_outliers(df_tbl = ca_step3_int,
                                    p_input = 0.9,
                                    column_analysis = 'prop_diff_prior',
                                    column_variable = c('step_number', 'attrition_step'))
  
  output_tbl(ca_step3_final, 'ca_ms_anom_nt_step3', file = TRUE)
  
  ## Runtime computation
  
  runtime_files <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                              name_string = 'check_runtime_')
  
  runtime_comps <- compute_runtime(merged_attrition_tbl = attrition_files,
                                   merged_runtime_tbl = runtime_files)
  
  output_tbl(runtime_comps$per_site, 'runtime_site', file = TRUE)
  
  ## Table 1s
  
  table1_files <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                             name_string = 'demographic_') %>%
    mutate(label = case_when(value %in% c('OT', 'NI', 'UN') ~ 'Other/Unknown',
                             category == 'race' & value == '01' ~ 'American Indian or Alaska Native',
                             category == 'race' & value == '02' ~ 'Asian',
                             category == 'race' & value == '03' ~ 'Black or African American',
                             category == 'race' & value == '04' ~ 'Native Hawaiian or Other Pacific Islander',
                             category == 'race' & value == '05' ~ 'White',
                             category == 'race' & value == '06' ~ 'Multiple Race',
                             category == 'race' & value == '07' ~ 'Other/Unknown',
                             category == 'sex' & value == 'M' ~ 'Male',
                             category == 'sex' & value == 'F' ~ 'Female',
                             category == 'ethnicity' & value == 'Y' ~ 'Hispanic',
                             category == 'ethnicity' & value == 'N' ~ 'Non-Hispanic',
                             category == 'ethnicity' & value == 'R' ~ 'Other/Unknown',
                             TRUE ~ value))
  
  output_tbl(table1_files, 'table1', file = TRUE)
  
  ## PF
  
  ### Across time
  pf_at_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                            name_string = 'pf_ss_exp_at_')
  
  output_tbl(pf_at_merge, 'pf_ss_at', file = TRUE)
  
  ### No time
  pf_nt_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                            name_string = 'pf_ss_exp_nt_')
  
  output_tbl(pf_nt_merge, 'pf_ss_nt', file = TRUE)
  
  ## EVP
  
  ### Across time
  evp_at_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                             name_string = 'evp_ss_exp_at_')
  
  evp_ms_at_eucl <- ms_anom_euclidean(fot_input_tbl = evp_at_merge,
                                      grp_vars = c('site', 'variable'),
                                      var_col = 'prop_pt_variable')
  
  output_tbl(evp_at_merge, 'evp_ss_anom_at', file = TRUE)
  output_tbl(evp_ms_at_eucl, 'evp_ms_anom_at', file = TRUE)
  
  ### No Time 
  evp_nt_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                             name_string = 'evp_ss_exp_nt_')
  
  output_tbl(evp_nt_merge, 'evp_ms_exp_nt', file = TRUE)
  
  ## CSD
  
  ### Across time
  #### Year
  csd_at_year_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                                  name_string = 'csd_ss_exp_at_year_')
  
  csd_ms_at_year_eucl <- ms_anom_euclidean(fot_input_tbl = csd_at_year_merge %>% filter(time_increment == 'year'),
                                           grp_vars = c('site', 'concept_code'),
                                           var_col = 'prop_concept')
  
  output_tbl(csd_at_year_merge, 'csd_ms_exp_at_year', file = TRUE)
  output_tbl(csd_ms_at_year_eucl, 'csd_ms_anom_at_year', file = TRUE)
  
  #### Quarter
  csd_at_qrtr_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                                  name_string = 'csd_ss_exp_at_quarter_')
  
  csd_ms_at_qrtr_eucl <- ms_anom_euclidean(fot_input_tbl = csd_at_qrtr_merge %>% filter(time_increment == 'quarter'),
                                           grp_vars = c('site', 'concept_code'),
                                           var_col = 'prop_concept')
  
  csd_ss_anom_qrtr <- anomalize_ss_anom_at(fot_input_tbl = csd_at_qrtr_merge %>%
                                             filter(site == 'C7TCH'),
                                           grp_vars = c('concept_code'),
                                           time_var = 'time_start',
                                           var_col = 'prop_concept')
  
  output_tbl(csd_at_qrtr_merge, 'csd_exp_at_qrtr', file = TRUE)
  output_tbl(csd_ms_at_qrtr_eucl, 'csd_ms_anom_at_qrtr', file = TRUE)
  output_tbl(csd_ss_anom_qrtr, 'csd_ss_anom_at_qrtr', file = TRUE)
  
  ### No Time
  csd_nt_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                             name_string = 'csd_ss_exp_nt_')
  
  csd_ms_nt_anom_int <- compute_dist_anomalies(df_tbl = csd_nt_merge,
                                               grp_vars = c('variable', 'concept_code'), 
                                               var_col = 'prop_concept') 
  
  csd_ms_nt_anom_final <- detect_outliers(df_tbl = csd_ms_nt_anom_int,
                                          tail_input = 'both',
                                          p_input = 0.9,
                                          column_analysis = 'prop_concept',
                                          column_variable = 'concept_code')
  
  output_tbl(csd_nt_merge, 'csd_exp_nt', file = TRUE)
  output_tbl(csd_ms_nt_anom_final, 'csd_ms_anom_nt', file = TRUE)
  
  ## PES
  pes_nt1_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                              name_string = 'pes_ss_exp_nt1_')
  
  output_tbl(pes_nt1_merge, 'pes_exp_nt1', file = TRUE)
  
  pes_nt2_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                              name_string = 'pes_ss_exp_nt2_')
  
  output_tbl(pes_nt2_merge, 'pes_exp_nt2', file = TRUE)
  
  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()
  
  message('Done.')
  
  invisible(rslt)
  
}
