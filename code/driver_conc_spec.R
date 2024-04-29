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
  
  cohort <- results_tbl('jspa_cohort') %>% compute_new()
  
  message('single site, no time')
  cnc_sp_ss_nt <- conc_process(cohort=cohort,
                               multi_or_single_site='single',
                               care_site=TRUE,
                               provider=TRUE,
                               codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                               vocab_tbl = vocabulary_tbl('concept'),
                               visit_type_tbl=read_codeset('conc_visit_types', col_type='ic'))
  output_tbl(cnc_sp_ss_nt,
             name='cnc_sp_ss_nt')
  
  message('multi site, no time')
  cnc_sp_ms_nt <- conc_process(cohort=cohort,
                               multi_or_single_site='multi',
                               care_site=TRUE,
                               provider=TRUE,
                               codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                               visit_type_tbl=read_codeset('conc_visit_types', col_type='ic'))
  output_tbl(cnc_sp_ms_nt,
             name='cnc_sp_ms_nt')
  
  message('site site, over time')
  # by year
  cnc_sp_ss_at <- conc_process(cohort=cohort,
                               multi_or_single_site='single',
                               care_site=TRUE,
                               provider=TRUE,
                               codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                               visit_type_tbl=read_codeset('conc_visit_types', col_type='ic'),
                               time=TRUE,
                               time_span=c('2012-01-01', '2022-01-01'),
                               time_period='year')
  output_tbl(cnc_sp_ss_at,
             name='cnc_sp_ss_at')
  # by month
  cnc_sp_ss_at_month <- conc_process(cohort=cohort,
                                     multi_or_single_site='single',
                                     care_site=TRUE,
                                     provider=TRUE,
                                     codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                                     visit_type_tbl=read_codeset('conc_visit_types', col_type='ic'),
                                     time=TRUE,
                                     time_span=c('2012-01-01', '2022-01-01'),
                                     time_period='month')
  output_tbl(cnc_sp_ss_at_month,
             name='cnc_sp_ss_at_month')
  
  message('multi site, over time')
  cnc_sp_ms_at <- conc_process(cohort=cohort,
                               multi_or_single_site='multi',
                               care_site=TRUE,
                               provider=TRUE,
                               codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                               visit_type_tbl=read_codeset('conc_visit_types', col_type='ic'),
                               time=TRUE,
                               time_span=c('2012-01-01', '2022-01-01'),
                               time_period='year')
  output_tbl(cnc_sp_ms_at,
             name='cnc_sp_ms_at')
  
  
  
  message('Done.')
  
  
}
