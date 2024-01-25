# Vector of additional packages to load before executing the request
config_append('extra_packages', c('lubridate','tidyr','ggplot2','RColorBrewer','timetk','stringr','plotly'))

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

  cohort <- results_tbl('cohort_jspa') %>% compute_new()
  
  compute_conc_full <- conc_process(cohort,
                                    grouped_list=c('site'),
                                    codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                                    care_site=TRUE,
                                    provider=TRUE,
                                    visit_type_tbl=read_codeset('conc_visit_types', col_type='ic'))%>%
    collect()
  output_tbl(compute_conc_full,
             name='conc_jspa_visit_cluster')
  
  compute_conc_time <- conc_process(cohort,
                                    grouped_list=c('site'),
                                    codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                                    care_site=TRUE,
                                    provider=TRUE,
                                    visit_type_tbl=read_codeset('conc_visit_types', col_type='ic'),
                                    time=TRUE,
                                    time_span=c('2010-01-01', '2023-01-01'),
                                    time_period='year',
                                    site_list=list('cchmc',
                                                   'chop',
                                                   'colorado',
                                                   'lurie',
                                                   'nationwide',
                                                   'nemours',
                                                   'seattle',
                                                   'stanford'))%>%
    collect()
  output_tbl(compute_conc_time,
             name='conc_jspa_visit_cluster_time')
  
  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()

  message('Done.')

  invisible(rslt)

}
