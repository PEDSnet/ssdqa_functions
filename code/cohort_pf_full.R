
#' Clinical Facts per Patient
#' 
#' This is a completeness check that will compute the number of facts per years of follow-up for each patient in 
#' a cohort. The user will provide the domains of interest in a provided CSV file. Results can optionally be stratified
#' by site, age group, visit type, codeset utilization, and/or time.
#'
#' @param cohort - A dataframe with the cohort of patients for your study. Should include the columns:
#'                       - @person_id
#'                       - @start_date
#'                       - @end_date
#'                       - @site
#' @param site_list - A list of sites for which you would like to examine clinical facts. Can be one site (single-site) or multiple
#'                    (multi-site)
#' @param study_name - A custom string label with the name of your study 
#' @param intermediate_tbl - optional parameter to return an intermediate results table in addition to the standard summary results
#'                           table. the options for the method of return of this table are:
#'                           - @r_dataframe: will return a dataframe within your local R environment
#'                           - @database_table: will write the intermediate table to a database based on the provided connection
#'                                              information
#'                           - @csv: will write the table to a CSV file and save it to the `results` folder provided in the package
#'                           - @NULL: the function will not save an intermediate results table
#' @param visit_types - A list of visit types by which the output should be stratified. Options for visit types can be found or adjusted
#'                      in the provided `pf_visit_types.csv` file. If you would not like to stratify your results by visit type, please
#'                      set the visit_types argument equal to `all`
#' @param multi_or_single_site - Option to run the function on a single vs multiple sites
#'                               - @single - run the function for a single site
#'                               - @multi - run the function for multiple sites
#' @param time - a logical that tells the function whether you would like to look at the output over time
#' @param time_span - when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
#'                    formatted as c(start date, end date) in yyyy-mm-dd date format.
#' @param time_period - when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
#'                      to `year`, but other time periods such as `month` or `week` are also acceptable
#' @param age_groups - If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - @min_age: the minimum age for the group (i.e. 10)
#'                     - @max_age: the maximum age for the group (i.e. 20)
#'                     - @group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'                     
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'                     
#'                     If you would not like to stratify by age group, leave the argument as NULL
#'                     
#' @param codeset - If you would like to stratify the results by codeset utilization, first supply your codeset and add a column called
#'                  `flag` to the codeset. This column will be used to identify groups for each of the codes. You can either use one flag
#'                  label for the whole codeset, or identify subgroups within the codeset with different flag labels. Once your codeset
#'                  as been annotated, fill out the provided `codeset_metadata.csv` file with the following information:
#'                  - @table: the CDM table where the codes can be found (i.e. condition_occurrence)
#'                  - @field: the field within the CDM table where the codes can be found (i.e. condition_concept_id)
#'                  - @file_name: the name of the codeset file with the added flag column you provided
#'                  
#'                  
#'                  Then supply this csv file as the codeset argument (i.e. read.csv('path/to/codeset_metadata.csv'))
#'                     
#'                  If you would not like to stratify by codeset, leave the argument as NULL
#'                  
#' @param anomaly_or_exploratory - Option to conduct an exploratory or anomaly detection analysis. Exploratory analyses give a high
#'                                 level summary of the data to examine the fact representation within the cohort. Anomaly detection
#'                                 analyses are specialized to identify outliers within the cohort.
#' @param domain_tbl - a csv file that defines the domains where facts should be identified. defaults to the provided
#'                     `pf_domains.csv` file, which contains the following fields:
#'                     - @domain: a string label for the domain being examined (i.e. prescription drugs)
#'                     - @default_tbl: the CDM table where information for this domain can be found (i.e. drug_exposure)
#'                     - @field_name: if filtering is required, which field should be used to filter the default_tbl (i.e. drug_type_concept_id)
#'                     - @field_filter: if filtering is required, a string with codes that will filter the default_tbl to the
#'                                      domain of interest (i.e. "38000177")
#'                                      
#'                    This CSV can be altered to fit the users needs, or another csv with the same columns and formatting can be supplied.
#' @param visit_type_table - a csv file that defines available visit types that are called in @visit_types. defaults to the provided
#'                           `pf_visit_types.csv` file, which contains the following fields:
#'                           - @visit_concept_id: the visit_concept_id that represents the visit type of interest (i.e. 9201)
#'                           - @visit_type: the string label to describe the visit type; this label can be used multiple times
#'                                          within the file if multiple visit_concept_ids represent the visit type
#'                          
#'                           This CSV can be altered to fit the users needs, or another csv with the same columns and formatting can be supplied.
#'
#' @return if intermediate_tbl is not NULL, an intermediate results table is returned in the user-defined format
#' @return a dataframe with summary results (i.e. medians) that can be used as the input for `pf_output_gen` to generate graphical output
#' @return a CSV file with a summary of the parameters used to configure the function and recommendations for how to configure the
#'         `pf_output_gen` function parameters; written to the provided results folder
#'         
 
pf_process <- function(cohort = cohort,
                       site_list = c('seattle','cchmc'),
                       study_name = 'glom',
                       intermediate_tbl = 'r_dataframe',
                       visit_types = c('outpatient','inpatient'),
                       multi_or_single_site = 'multi',
                       time = FALSE,
                       time_span = c('2014-01-01', '2023-01-01'),
                       time_period = 'year',
                       age_groups = NULL,
                       codeset = NULL,
                       anomaly_or_exploratory='anomaly',
                       domain_tbl=read_codeset('pf_domains_short','cccc'),
                       visit_type_table=read_codeset('pf_visit_types','ic')){
  
  ## Step 0: Set cohort name for table output
  config('cohort', study_name)
  
  ## Step 1: Check Sites
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site,
                                 site_list = site_list)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  
  ## Step 2: Prep cohort
  
  cohort_prep <- prepare_cohort(cohort = cohort_filter, age_groups = age_groups, codeset = codeset)
  
  ## Step 3: Run Function
  
  grouped_list <- grouped_list %>% append(c('person_id','start_date','end_date','fu'))
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  if(is.data.frame(codeset)){grouped_list <- grouped_list %>% append('flag')}
  
  if(time){
    
    grouped_list <- grouped_list[! grouped_list %in% 'fu']
    
    pf_final <- compute_fot(cohort = cohort_prep,
                            reduce_id = 'visit_type',
                            time_period = time_period,
                            time_span = time_span,
                            site_list = site_list_adj,
                            check_func = function(dat){
                              loop_through_visits(cohort_tbl = dat,
                                                  site_col = site_col,
                                                  time = TRUE,
                                                  visit_type_tbl=visit_type_table,
                                                  site_list=site_list_adj,
                                                  visit_list=visit_types,
                                                  grouped_list=grouped_list,
                                                  domain_tbl=domain_tbl)
                            })
    
  } else {
    pf_tbl <- loop_through_visits(
      cohort_tbl=cohort_prep,
      site_col = site_col, 
      time = FALSE,
      site_list=site_list_adj,
      visit_list=visit_types,
      visit_type_tbl=visit_type_table,
      grouped_list=grouped_list,
      domain_tbl = domain_tbl
    )
    
    ### NEED TO MAKE SURE THAT CREATING LONG TABLE IS A GOOD DECISION FOR REPRODUCIBILITY 
    pf_final <- combine_study_facts(pf_tbl=pf_tbl,
                                    domain_list = domain_tbl,
                                    study_abbr = study_name, 
                                    time = time, 
                                    visit_type_list = visit_types) %>% collect()}
  
  # Output intermediate results if requested
  if(!is.null(intermediate_tbl)){
    if(intermediate_tbl == 'r_dataframe'){
      pf_intermediate_results <<- pf_final
    }else if(intermediate_tbl == 'database_table'){
        output_tbl(pf_final, 'pf_intermediate_results')
    }else if(intermediate_tbl == 'csv'){
      output_tbl(pf_final, 'pf_intermediate_results', file = TRUE)}
  }else{pf_intermediate_results <- NULL}
  
  param_csv_summary(site_list = site_list,
                    visit_list = visit_types,
                    ms_site = multi_or_single_site,
                    anom_exp = anomaly_or_exploratory,
                    time = time,
                    time_span = time_span,
                    age = age_groups,
                    cs = codeset) %>% output_tbl('parameter_summary', file = TRUE)
  
  ## Step 4: Summarise (Medians, SD)
  if(!time) {
    if(anomaly_or_exploratory=='anomaly' && multi_or_single_site=='single') {
      pf_output <- compute_dist_mean(pf_final,
                                     agegrp= age_groups,
                                     codeset = codeset)
    } else {pf_output <- compute_pf_medians(data_input=pf_final,
                                            agegrp = age_groups,
                                            codeset=codeset)}
    
  }else{pf_output <- pf_final}
  
}


#' Clinical Facet per Patient -- Output Generation
#'
#' @param pf_process - the summary dataframe output by the `pf_process` function. 
#' 
#'                     Note any intermediate table generated is not intended to be used with this function.
#' @param output_function - the name of the output function that should be used provided in the `parameter_summary` csv 
#'                          file that is output to the provided results folder after running the `pf_process` function 
#' @param output - which output variable you would like to use in the graphs. available options based on check 
#'                 configuration are provided in the `parameter_summary` csv file
#' @param facet - the variables by which you would like to facet the graph. available and/or recommended options for
#'                faceting variables are provided in the `parameter_summary` csv file
#' @param color - option to automatically generate a color palette or manually create one for both domains and sites
#'                       - @auto: this option will automatically create one color palette each for the distinct domains
#'                                and sites available in the *pf_process* table
#'                       - @manual: choose this option to create your own custom color palette. to do this, fill out
#'                                  the provided *domain_color_config* and *site_color_config* csv files with the name
#'                                  of the domain/site as it appears in the *pf_process* table and a corresponding hex
#'                                  code for the color assignment
#' @param time_span - the length of time that should be displayed for relevant over-time graphs. this time period should
#'                    either be the same as or a subset of the time period used in the `pf_process` function
#' @param date_breaks_str - only for single-site, exploratory, over time; a string that informs the program how the
#'                          time period should be divided (i.e. '1 year', '3 months', etc). Defaults to 1 year.
#' @param kmeans_clusters - the number of clusters that should be used in the kmeans cluster graph; defaults to 2
#' 
#'                          if the function outputs an error, try adjusting this number as a first step for troubleshooting.
#'                          the kmeans functionality will not work if there are not enough groups in the data to satisfy the 
#'                          provided number of clusters.
#' @param save_as_png - logical to tell the program if you would like to save the graphical output locally as a png file
#' @param file_path - if save_as_png = TRUE, the file path that leads to the location where the png files should be saved
#'
#' @return "raw" output that can be called within the R environment to generate the graph in the Viewer window
#' @return if save_as_png = TRUE, a png file with the graphical output
#' 

pf_output_gen <- function(pf_process,
                          output_function,
                          output,
                          facet,
                          color = 'auto',
                          time_span = c('2012-01-01', '2023-01-01'),
                          date_breaks_str = '1 year',
                          kmeans_clusters = 2,
                          save_as_png = FALSE,
                          file_path = NULL){
  
  ## Create color palettes for sites and domains
  
  site_list <- pf_process %>% select(site) %>% distinct() %>% pull()
  domain_list <- pf_process %>% select(domain) %>% distinct() %>% pull()
  
  create_color_scheme(type = color,
                      site_list = site_list,
                      domain_list = domain_list)
  
  ## Run output functions
  if(output_function == 'pf_ms_anom_nt'){
    pf_output <- pf_ms_anom_nt(data_tbl = pf_process,
                               output = output,
                               facet = facet,
                               kmeans_clusters = kmeans_clusters)
  }else if(output_function == 'pf_ss_anom_nt'){
    pf_output <- pf_ss_anom_nt(data_tbl = pf_process,
                               output = output,
                               facet = facet)
  }else if(output_function == 'pf_ms_exp_nt'){
    pf_output <- pf_ms_exp_nt(data_tbl = pf_process,
                              output = output,
                              facet = facet)
  }else if(output_function == 'pf_ss_exp_nt'){
    pf_output <- pf_ss_exp_nt(data_tbl = pf_process,
                              output = output,
                              facet = facet)
  }else if(output_function == 'pf_ms_anom_at'){
    pf_output <- pf_ms_anom_at(data_tbl = pf_process,
                               output = output,
                               facet = facet)
  }else if(output_function == 'pf_ss_anom_at'){
    pf_output <- pf_ss_anom_at(data_tbl = pf_process,
                               output = output,
                               facet = facet,
                               time_span = time_span)
  }else if(output_function == 'pf_ms_exp_at'){
    pf_output <- pf_ms_exp_at(data_tbl = pf_process,
                              output = output,
                              facet = facet,
                              time_span = time_span)
  }else if(output_function == 'pf_ss_exp_at'){
    pf_output <- pf_ss_exp_at(data_tbl = pf_process,
                              output = output,
                              facet = facet,
                              date_breaks_str = date_breaks_str)
  }
  
  ## Saving PNG images of graphical output
  if(save_as_png){
    
    int_labels <- c("pf_ms_exp_nt","pf_ms_exp_at")
    
    if(output_function == 'pf_ms_anom_nt'){
      
      for(plot in 1:length(pf_output)){
        num <- plot
        ggsave(plot = pf_output[[plot]],
               height = 15,
               width = 25,
               units = c('cm'),
               filename = paste0(file_path, '/', output_function, '_', num, '.png'))
        }
    
      }else{
        if(output_function %in% int_labels){
          
          htmltools::save_html(html = pf_output, file = paste0(file_path, '/', output_function, '.html'))
          
          }else{
            ggsave(plot = pf_output,
                   height = 15,
                   width = 25,
                   units = c('cm'),
                   filename = paste0(file_path, '/', output_function, '.png'))
      }
    }
  }
  
  ## Return "raw" output in place of or in addition to .png files
  return(pf_output)
}


# pf_output_gen <- function(pf_process,
#                           site_list,
#                           colors = 'auto',
#                           visit_types = c('all'),
#                           multi_or_single_site = 'single',
#                           time = FALSE,
#                           time_span = c('2012-01-01', '2023-01-01'),
#                           age_groups = FALSE,
#                           codeset = FALSE,
#                           anomaly_or_exploratory = 'exploratory',
#                           domain_tbl=read_codeset('pf_domains_short','cccc'),
#                           save_as_png = FALSE,
#                           file_path = NULL){
#   
#   ## Create Brewer color palettes for sites and domains
#   
#   create_color_scheme(type = colors)
#   
#   ## Generate appropriate output based on selections
#   if(time){
#     if(multi_or_single_site == 'single'){
#       if(anomaly_or_exploratory == 'anomaly'){
#         if(age_groups){
#           output <- pf_ss_anom_at(data_tbl = pf_output,
#                                   site_list = site_list,
#                                   age_groups = TRUE)
#           label <- "pf_ss_age_anom_at"
#         }else if(codeset){
#           output <- pf_ss_anom_at(data_tbl = pf_output,
#                                   site_list = site_list,
#                                   codeset = TRUE)
#           label <- "pf_ss_code_anom_at"
#         }else if(length(visit_types) > 1){
#           output <- pf_ss_anom_at(data_tbl = pf_output,
#                                   site_list = site_list,
#                                   visit_types = TRUE)
#           label <- "pf_ss_visit_anom_at"
#         }else{
#           output <- pf_ss_anom_at(data_tbl = pf_output,
#                                   site_list = site_list)
#           label <- "pf_ss_nostrat_anom_at"
#       }
#         } else {
#         if(age_groups){
#           output <- pf_ss_exp_at(data_tbl = pf_output,
#                                  site_list = site_list,
#                                  domain_colors = domain_colors,
#                                  age_groups = TRUE)
#           label <- "pf_ss_age_exp_at"
#         }else if(codeset){
#           output <- pf_ss_exp_at(data_tbl = pf_output,
#                                  site_list = site_list,
#                                  domain_colors = domain_colors,
#                                  codeset = TRUE)
#           label <- "pf_ss_code_exp_at"
#         }else if(length(visit_types) > 1){
#           output <- pf_ss_exp_at(data_tbl = pf_output,
#                                  site_list = site_list,
#                                  domain_colors = domain_colors,
#                                  visit_types = TRUE)
#           label <- "pf_ss_visit_exp_at"
#         }else{
#           output <- pf_ss_exp_at(data_tbl = pf_output,
#                                  domain_colors = domain_colors,
#                                  site_list = site_list)
#           label <- "pf_ss_nostrat_exp_at"
#     }
#           }
#       } else {
#       if(anomaly_or_exploratory == 'anomaly'){
#         if(age_groups){
#           output <- pf_ms_anom_at(data_tbl = pf_output,
#                                   domain_list = domain_tbl,
#                                   age_groups = TRUE)
#           label <- "pf_ms_age_anom_at"
#         }else if(codeset){
#           output <- pf_ms_anom_at(data_tbl = pf_output,
#                                   domain_list = domain_tbl,
#                                   codeset = TRUE)
#           label <- "pf_ms_code_anom_at"
#         }else if(length(visit_types) > 1){
#           output <- pf_ms_anom_at(data_tbl = pf_output,
#                                   domain_list = domain_tbl,
#                                   visit_types = TRUE)
#           label <- "pf_ms_visit_anom_at"
#         }else{
#           output <- pf_ms_anom_at(data_tbl = pf_output,
#                                   domain_list = domain_tbl)
#           label <- "pf_ms_nostrat_anom_at"
#         }
#       } else {
#         if(age_groups){
#           output <- pf_ms_exp_at(data_tbl = pf_output,
#                                  site_colors = site_colors,
#                                  domain_list = domain_tbl,
#                                  time_span = time_span,
#                                  age_groups = TRUE)
#           label <- "pf_ms_age_exp_at"
#         }else if(codeset){
#           output <- pf_ms_exp_at(data_tbl = pf_output,
#                                  site_colors = site_colors,
#                                  domain_list = domain_tbl,
#                                  time_span = time_span,
#                                  codeset = TRUE)
#           label <- "pf_ms_code_exp_at"
#         }else if(length(visit_types) > 1){
#           output <- pf_ms_exp_at(data_tbl = pf_output,
#                                  site_colors = site_colors,
#                                  domain_list = domain_tbl,
#                                  time_span = time_span,
#                                  visit_types = TRUE)
#           label <- "pf_ms_visit_exp_at"
#         }else{
#           output <- pf_ms_exp_at(data_tbl = pf_output,
#                                  site_colors = site_colors,
#                                  domain_list = domain_tbl,
#                                  time_span = time_span)
#           label <- "pf_ms_nostrat_exp_at"
#         }
#       }
#     }
#   } else {
#     if(multi_or_single_site == 'single'){
#       if(anomaly_or_exploratory == 'anomaly'){
#         if(age_groups){
#           output <- pf_ss_anom_nt(data_tbl = pf_output,
#                                   site_list = site_list,
#                                   domain_colors = domain_colors,
#                                   age_groups = TRUE)
#           label <- "pf_ss_age_anom_nt"
#         }else if(codeset){
#           output <- pf_ss_anom_nt(data_tbl = pf_output,
#                                   site_list = site_list,
#                                   domain_colors = domain_colors,
#                                   codeset = TRUE)
#           label <- "pf_ss_code_anom_nt"
#         }else if(length(visit_types) > 1){
#           output <- pf_ss_anom_nt(data_tbl = pf_output,
#                                   site_list = site_list,
#                                   domain_colors = domain_colors,
#                                   visit_types = TRUE)
#           label <- "pf_ss_visit_anom_nt"
#         }else{
#           output <- pf_ss_anom_nt(data_tbl = pf_output,
#                                   domain_colors = domain_colors,
#                                   site_list = site_list)
#           label <- "pf_ss_nostrat_anom_nt"
#         }
#       } else {
#         if(age_groups){
#           output <- pf_ss_exp_nt(data_tbl = pf_output,
#                                  site_list = site_list,
#                                  site_colors = site_colors,
#                                  age_groups = TRUE)
#           label <- "pf_ss_age_exp_nt"
#         }else if(codeset){
#           output <- pf_ss_exp_nt(data_tbl = pf_output,
#                                  site_list = site_list,
#                                  site_colors = site_colors,
#                                  codeset = TRUE)
#           label <- "pf_ss_code_exp_nt"
#         }else if(length(visit_types) > 1){
#           output <- pf_ss_exp_nt(data_tbl = pf_output,
#                                  site_list = site_list,
#                                  site_colors = site_colors,
#                                  visit_types = TRUE)
#           label <- "pf_ss_visit_exp_nt"
#         }else{
#           output <- pf_ss_exp_nt(data_tbl = pf_output,
#                                  site_list = site_list,
#                                  site_colors = site_colors)
#           label <- "pf_ss_nostrat_exp_nt"
#         }
#       }
#     } else {
#       if(anomaly_or_exploratory == 'anomaly'){
#         if(age_groups){
#           output <- pf_ms_anom_nt(data_tbl = pf_output,
#                                   age_groups = TRUE)
#           label <- "pf_ms_age_anom_nt"
#         }else if(codeset){
#           output <- pf_ms_anom_nt(data_tbl = pf_output,
#                                   codeset = TRUE)
#           label <- "pf_ms_code_anom_nt"
#         }else if(length(visit_types) > 1){
#           output <- pf_ms_anom_nt(data_tbl = pf_output,
#                                   visit_types = TRUE)
#           label <- "pf_ms_visit_anom_nt"
#         }else{
#           output <- pf_ms_anom_nt(data_tbl = pf_output)
#           label <- "pf_ms_nostrat_anom_nt"
#         }
#       } else{
#         if(age_groups){
#           output <- pf_ms_exp_nt(data_tbl = pf_output,
#                                  age_groups = TRUE)
#           label <- "pf_ms_age_exp_nt"
#         }else if(codeset){
#           output <- pf_ms_exp_nt(data_tbl = pf_output,
#                                  codeset = TRUE)
#           label <- "pf_ms_code_exp_nt"
#         }else if(length(visit_types) > 1){
#           output <- pf_ms_exp_nt(data_tbl = pf_output,
#                                  visit_types = TRUE)
#           label <- "pf_ms_visit_exp_nt"
#         }else{
#           output <- pf_ms_exp_nt(data_tbl = pf_output)
#           label <- "pf_ms_nostrat_exp_nt"
#         }
#       }
#     }
#   }
#   
#   ## ggsave graphs as .png if requested
#   if(save_as_png){
#     
#     single_op <- c('pf_ms_age_anom_at', 'pf_ms_code_anom_at','pf_ms_visit_anom_at','pf_ms_nostrat_anom_at',
#                    'pf_ms_age_exp_nt', 'pf_ms_code_exp_nt','pf_ms_visit_exp_nt','pf_ms_nostrat_exp_nt')
#     
#     int_labels <- c("pf_ms_age_exp_nt","pf_ms_code_exp_nt","pf_ms_visit_exp_nt",
#                     "pf_ms_nostrat_exp_nt","pf_ms_age_exp_at","pf_ms_code_exp_at",
#                     "pf_ms_visit_exp_at","pf_ms_nostrat_exp_at")
#     
#     if(label %in% single_op){
#       
#       if(label %in% int_labels){
#         
#         htmltools::save_html(html = output, file = paste0(file_path, '/', label, '.html'))
#         
#       }else{
#       
#       ggsave(plot = output,
#              height = 15,
#              width = 25,
#              units = c('cm'),
#              filename = paste0(file_path, '/', label, '.png'))
#         }
#     }else{
#       
#       for(plot in 1:length(output)){
#         
#         if(!(label %in% int_labels)){
#         
#         num <- plot
#         
#         ggsave(plot = output[[plot]],
#                height = 15,
#                width = 25,
#                units = c('cm'),
#                filename = paste0(file_path, '/', label, '_', num, '.png'))
#         }else{
#           num <- plot
#           htmltools::save_html(html = output[[plot]], file = paste0(file_path, '/', label, '_', num, '.html'))}
#       }
#       }
#   }
#   
#   ## Return "raw" output in place of or in addition to .png files
#   return(output)
# }
#   