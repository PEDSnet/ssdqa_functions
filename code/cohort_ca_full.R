

#' Cohort Attrition 
#'
#' @param attrition_tbl table with attrition information for each site needed for analysis
#'                      should have at least the following columns:
#'                      - @site
#'                      - @step_number
#'                      - @attrition_step
#'                      - @num_pts
#'                      
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#'                               - @single - run the function for a single site
#'                               - @multi - run the function for multiple sites
#' @param anomaly_or_exploratory Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                               level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                               analyses are specialized to identify outliers within the cohort.
#' @param start_step_num the @step_number that should be considered the "start" for the analysis
#' @param var_col the column that should be used to conduct the analysis for multi-site anomaly detection. options are:
#'                - @num_pts (raw patient count), 
#'                - @prop_retained_start (proportion patients retained from starting step),
#'                - @prop_retained_prior (proportion patients retained from prior step),
#'                - @prop_diff_prior (proportion difference between each step and the prior step)
#' @param p_value the p value to be used as a threshold in the multi-site anomaly detection analysis
#'
#' @return a CSV file with all the original attrition information, plus columns examining the difference between each step and others.
#'         
#'         if a multi-site anomaly detection analysis is run, this output will also include some descriptive statistics about the chosen
#'         `var_col` and an indication of which sites are outliers at each attrition step
#' 
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


#' Cohort Attrition Output
#'
#' @param process_output the output of the `ca_process` function
#' @param output_function the name of the output function that should be used provided in the `parameter_summary` csv 
#'                        file that is output to the provided results folder after running the `ca_process` function 
#' @param log_scale a logical indicating whether the results should be shown in a log scale
#' @param var_col the column of the output variable of interest. options are:
#'                `num_pts` -- the raw count of patients meeting the requirements for each step
#'                `prop_retained_start` -- the proportion of patients retained at each step compared to the user-selected starting step
#'                `prop_retained_prior` -- the proportion of patients retained at each step compared to the prior step
#'                `prop_diff_prior` -- the proportion difference between each step and the step prior
#'
#' @return for ss_exp_nt & ms_exp_nt, a line graph displaying the var_col of interest at each
#'         attrition step is returned, along with a table with the descriptors for each step
#'         
#'         for ms_anom_nt, a dot plot is returned where anomalous values are shown as stars.
#'         the size of the dot represents the mean value, while the color represents the value
#'         of the output column
#' 
ca_output <- function(process_output,
                      output_function,
                      log_scale = FALSE,
                      var_col = 'num_pts'){
  
  if(output_function == 'ca_ss_exp_nt'){
    ca_output <- ca_ss_exp_nt(process_output = process_output,
                              log_scale = log_scale,
                              output = var_col)
  }else if(output_function == 'ca_ms_exp_nt'){
    ca_output <- ca_ms_exp_nt(process_output = process_output,
                              log_scale = log_scale,
                              output = var_col)
  }else if(output_function == 'ca_ms_anom_nt'){
    ca_output <- ca_ms_anom_nt1(process_output = process_output,
                                output = var_col)
  }else(cli::cli_abort('Please enter a valid output_function for this check'))
  
  return(ca_output)
  
}