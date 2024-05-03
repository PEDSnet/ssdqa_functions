

ca_process <- function(attrition_tbl,
                       multi_or_single_site,
                       anomaly_or_exploratory,
                       start_step_num = 0,
                       var_col = 'num_pts',
                       p_value = 0.9){
  
  site_filter <- check_site_type(cohort = attrition_tbl,
                                 multi_or_single_site = multi_or_single_site)
  attrition_tbl <- site_filter$cohort
  site_col <- site_filter$grouped_list
  
  
  attrition_process <- compute_attrition_diff(attrition_tbl = attrition_tbl,
                                              start_step_num = start_step_num,
                                              site_col = site_col)
  
  if(multi_or_single_site == 'multi' && anomaly_or_exploratory == 'anomaly'){
    
    att_int <- compute_dist_anomalies(df_tbl = attrition_process,
                                      grp_vars = c('step_number', 'attrition_step'),
                                      var_col = var_col)
    
    att_final <- detect_outliers(df_tbl = att_int,
                                 p_input = p_value,
                                 column_analysis = var_col,
                                 column_variable = c('step_number', 'attrition_step'))
    
  }else{att_final <- attrition_process}
  
  att_final %>%
    replace_site_col() %>%
    output_tbl(file = TRUE)
  
}