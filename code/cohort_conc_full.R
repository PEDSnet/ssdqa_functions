
conc_process <- function(cohort = cohort,
                         site_list = c('seattle','cchmc'),
                         study_name = 'glom',
                         intermediate_tbl = 'r_dataframe',
                         visit_types = c('outpatient','inpatient'),
                         multi_or_single_site = 'multi',
                         collapse_sites = FALSE,
                         time = FALSE,
                         time_span = c('2014-01-01', '2023-01-01'),
                         age_groups = NULL,
                         codeset_fact = read_codeset("conc_codesets", col_types = 'cccc'),
                         #codeset_spec = NULL,
                         anomaly_or_exploratory='anomaly',
                         domain_name, # idea here is to have them provide a string that we then match to a cdm_tbl
                         #domain_tbl=read_codeset('pf_domains_short','cccc'),
                         visit_type_table=read_codeset('pf_visit_types','ic')){
  ## Step 0: Set cohort name for table output
  config('cohort', study_name)
  
  ## Step 1: Prepare cohort
  # cohort_prep <- prepare_pf(cohort = cohort, age_groups = age_groups, codeset = codeset)
  
  ## Step 2: Run function
  grouped_list <- c('site')
  if(is.data.frame(age_groups)){grouped_list<-grouped_list%>%append('age_grp')}
  if(is.data.frame(codeset)){grouped_list<-grouped_list%>%append('flag')}
  
  if(time){
    grouped_list <- grouped_list[!grouped_list%in% 'fu']
    
    conc_final <- compute_fot_conc(cohort=cohort,
                                   time_period='year',
                                   time_span=time_span,
                                   collapse_sites,
                                   visit_type_tbl=visit_type_table,
                                   site_list=site_list,
                                   visit_list=visit_types,
                                   domain_tbl=domain_tbl)
  }
  else{
    # doesn't involve looping through visits yet
    conc_final <- compute_conc(cohort=cohort,
                               grouped_list=grouped_list,
                               codeset_tbl=codeset_fact)
    
  }
  
  
}