
## CSV Merge Function

merge_csvs <- function(output_directory,
                       name_string){
  
  csv_list <- list.files(output_directory, pattern = paste0('^',name_string),
                         full.names = TRUE)  #character list all files
  
  df_list <- list()
  
  for(i in 1:length(csv_list)){
    
    file <- read_csv(csv_list[i])
    
    df_list[[i]] <- file
    
  }
  
  df_merge <- purrr::reduce(.x = df_list,
                            .f = dplyr::union)
  
  return(df_merge)
  
}

## Runtime Computation Function

compute_runtime <- function(merged_attrition_tbl,
                            merged_runtime_tbl){
  
  all_times <- merged_attrition_tbl %>%
    select(qry_site, stamp) %>%
    union(select(merged_runtime_tbl, qry_site, stamp))
  
  step_runtime_attrition <- merged_attrition_tbl %>%
    select(qry_site, stamp, step_number, attrition_step) %>%
    arrange(qry_site, desc(stamp)) %>%
    group_by(qry_site) %>%
    mutate(step_runtime = stamp - lead(stamp))
    
  step_runtime_check <- merged_runtime_tbl %>%
    select(qry_site, stamp, check, check_app) %>%
    arrange(qry_site, desc(stamp)) %>%
    group_by(qry_site) %>%
    mutate(app_runtime = stamp - lead(stamp)) %>%
    group_by(qry_site, check) %>%
    mutate(check_runtime = sum(app_runtime))
  
  site_runtime <- all_times %>%
    group_by(qry_site) %>%
    summarise(start_time = min(stamp),
              end_time = max(stamp)) %>%
    mutate(site_runtime = end_time - start_time)
  
  final <- list('per_check' = step_runtime_check,
                'per_attrition' = step_runtime_attrition,
                'per_site' = site_runtime)
  
  return(final)
  
}


## Cohort Attrition

attrition_files <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                              name_string = 'diabetes_attrition_')

ca_output_step0 <- ca_process(attrition_tbl = attrition_files,
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'exploratory',
                              start_step_num = 0)

ca_output_step3 <- ca_process(attrition_tbl = attrition_files,
                              multi_or_single_site = 'multi',
                              anomaly_or_exploratory = 'exploratory',
                              start_step_num = 3)

## Runtime computation

runtime_files <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                            name_string = 'check_runtime_')

runtime_comps <- compute_runtime(merged_attrition_tbl = attrition_files,
                                 merged_runtime_tbl = runtime_files)

## Table 1s

table1_files <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                           name_string = 'demographic_')

## PF

### Across time
pf_at_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                          name_string = 'pf_ss_exp_at_')

### No time
pf_nt_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                          name_string = 'pf_ss_exp_nt_')

## EVP

### Across time
evp_at_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                           name_string = 'evp_ss_exp_at_')

evp_ms_at_eucl <- ms_anom_euclidean(fot_input_tbl = evp_at_merge,
                                    grp_vars = c('site', 'variable'),
                                    var_col = 'prop_pt_variable')

evp_ss_at_edit <- evp_at_merge %>% mutate(site == 'combined') ##### need to sum / combine rows here????? or should we run each site one at a time?

evp_ss_at_anom <- anomalize_ss_anom_at(fot_input_tbl = evp_ss_at_edit,
                                       time_var = 'time_start',
                                       grp_vars = 'variable',
                                       var_col = 'prop_pt_variable')

### No Time 
evp_nt_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                           name_string = 'evp_ss_exp_nt_')

## CSD

### Across time
csd_at_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                           name_string = 'csd_ss_exp_at_')

csd_ms_at_eucl <- ms_anom_euclidean(fot_input_tbl = csd_at_merge %>% filter(time_increment == 'year'),
                                    grp_vars = c('site', 'concept_code'),
                                    var_col = 'prop_concept')

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

## PES
pes_nt1_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                            name_string = 'pes_ss_exp_nt1_')

pes_nt2_merge <- merge_csvs(output_directory = paste0(base_dir, '/results/'),
                            name_string = 'pes_ss_exp_nt2_')
