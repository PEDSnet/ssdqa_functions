# Vector of additional packages to load before executing the request
config_append('extra_packages', c('lubridate','tidyr','ggplot2','RColorBrewer','timetk','stringr','plotly',
                                  'scales'))

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
  
  cohort <- results_tbl('jspa_cohort') 
  concepts <-  read_codeset('csd_codesets','iccccc') %>% 
                  filter(variable %in% c('ibd', 'spondyloarthritis', 'infliximab'))
  time_span_var <- c('2014-01-01', '2021-12-31')
  time_period_var <- 'month'
  
  ss_exp_nt <- csd_process(cohort = results_tbl('jspa_cohort'),
                           domain_tbl=read_codeset('scv_domains', 'cccc'),
                           concept_set = concepts,
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory='exploratory',
                           num_concept_combined = FALSE,
                           num_concept_1 = 30,
                           num_concept_2 = 30,
                           age_groups = FALSE, #read_codeset('age_group_definitions'),
                           time = FALSE,
                           time_span = time_span_var,
                           time_period = time_period_var)%>%
    collect()
    output_tbl(ss_exp_nt %>% relocate(site),
             name='csd_ss_exp_nt')
  
  ss_anom_nt <- csd_process(cohort = results_tbl('jspa_cohort'),
                            domain_tbl=read_codeset('scv_domains', 'cccc'),
                            concept_set = concepts,
                            multi_or_single_site = 'single',
                            anomaly_or_exploratory='anomaly',
                            num_concept_combined = FALSE,
                            num_concept_1 = 30,
                            num_concept_2 = 30,
                            age_groups = FALSE, #read_codeset('age_group_definitions'),
                            time = FALSE,
                            time_span = time_span_var,
                            time_period = time_period_var)%>%
    relocate(site)
    output_tbl(ss_anom_nt,
               name='csd_ss_anom_nt')
  
  ss_at <- csd_process(cohort = results_tbl('jspa_cohort'),
                                domain_tbl=read_codeset('scv_domains', 'cccc'),
                                concept_set = concepts,
                                multi_or_single_site = 'single',
                                anomaly_or_exploratory='anomaly',
                                num_concept_combined = FALSE,
                                num_concept_1 = 30,
                                num_concept_2 = 30,
                                age_groups = FALSE, #read_codeset('age_group_definitions'),
                                time = TRUE,
                                time_span = time_span_var,
                                time_period = time_period_var)%>%
    collect()
    output_tbl(ss_at,
              name='csd_ss_at')
    
  ms_nt <- csd_process(cohort = results_tbl('jspa_cohort'),
                       domain_tbl=read_codeset('scv_domains', 'cccc'),
                       concept_set = concepts,
                       multi_or_single_site = 'multi',
                       anomaly_or_exploratory='anomaly',
                       num_concept_combined = FALSE,
                       num_concept_1 = 30,
                       num_concept_2 = 30,
                       age_groups = FALSE, #read_codeset('age_group_definitions'),
                       time = FALSE,
                       time_span = time_span_var,
                       time_period = time_period_var)%>%
      collect()
      output_tbl(ms_nt,
                 name='csd_ms_nt')
      
   ms_at <- csd_process(cohort = results_tbl('jspa_cohort'),
                        domain_tbl=read_codeset('scv_domains', 'cccc'),
                        concept_set = concepts,
                        multi_or_single_site = 'multi',
                        anomaly_or_exploratory='exploratory',
                        num_concept_combined = FALSE,
                        num_concept_1 = 30,
                        num_concept_2 = 30,
                        age_groups = FALSE, #read_codeset('age_group_definitions'),
                        time = TRUE,
                        time_span = time_span_var,
                        time_period = time_period_var)%>%
     collect()
     output_tbl(ms_at,
                name='csd_ms_at_month')
  
  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()
  
  message('Done.')
  
  invisible(rslt)
  
}
