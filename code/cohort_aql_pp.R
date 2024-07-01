
## CSV Merge Function

merge_csvs <- function(output_directory,
                       name_string){
  
  csv_list <- list.files(output_directory, pattern = paste0('^',name_string),
                         full.names = TRUE)  #character list all files
  
  df_list <- list()
  
  for(i in 1:length(csv_list)){
    
    file <- read_csv(csv_list[i])
    
    if('dmid' %in% names(file)){file <- file %>% rename('site' = dmid)}
    
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