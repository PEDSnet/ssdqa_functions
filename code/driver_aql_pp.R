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
  rslt$cnc_sp_vs<-read_codeset('pcornet_cnc_sp_vs', col_types='ccc') %>%
    mutate(vs_text=gsub(".*=","",valueset_item_descriptor))
  rslt$cnc_output_names<-results_tbl('cnc_sp_specialty_names_loc_mugjx', results_tag = FALSE)%>%collect()%>%
    left_join(rslt$cnc_sp_vs, by = c('specialty_concept_id'='valueset_item'))%>%
    output_tbl(name='cnc_sp_vs_names',
               db=FALSE,
               file=TRUE)
  
  
  
  # Write step summary log to CSV and/or database,
  # as determined by configuration
  output_sum()
  
  message('Done.')
  
  invisible(rslt)
  
}
