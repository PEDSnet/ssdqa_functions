#' Concordance: Clinical Events and Specialties
#' 
#' This is a check that will assess quality of specialty data in a study sample
#' The user will provide clinical codesets of interest with associated domains
#' and will be able to stratify results by:
#'                age group (user will provide groupings)
#'                visit type (user will provide groupings)
#'                time (by year)
#' @param cohort - A dataframe with the cohort of patients for your study. Should include the columns:
#'          `site` | `person_id` | `start_date` | `end_date`
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#'                      - 'single': run on a single site, or treat all of the sites as one
#'                      - 'multi': run on a group of sites, treating each site separately
#'@param age_groups- If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - min_age: the minimum age for the group (i.e. 10)
#'                     - max_age: the maximum age for the group (i.e. 20)
#'                     - group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'                     
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'                     If you do not wish to stratify by age, keep as NULL
#' @param codeset_tbl table in the specs directory with the columns:
#'                    - domain: name of the domain
#'                    - default_tbl: name of the cdm_tbl
#'                    - field_name: column name in the default_tbl for which to search the codeset concept_ids
#'                    - codeset_name: name of a codeset in the specs directory
#' @param care_site TRUE if want to look at care_site specialty
#'                  FALSE if do not want to look at care_site specialty
#' @param provider TRUE if want to look at provider specialty
#'                  FALSE if do not want to look at provider specialty
#'                  IF both `provider` and `care_site` are both TRUE,
#'                        provider specialty will be prioritized if provider and care_site are discordant for the visit
#' @param visit_type_tbl - a csv file that defines available visit types that are called in @visit_types. defaults to the provided
#'                           `conc_visit_types.csv` file, which contains the following fields:
#'                           - visit_concept_id: the visit_concept_id that represents the visit type of interest (i.e. 9201)
#'                           - visit_type: the string label to describe the visit type; this label can be used multiple times
#'                                          within the file if multiple visit_concept_ids represent the visit type
#'                          
#'                           This CSV can be altered to fit the users needs, or another csv with the same columns and formatting can be supplied.
#' @param time TRUE if results should be over time. Defaults to FALSE
#' @param time_period if time=TRUE, indicates time period (e.g. 'year', 'month') over which to measure
#' @param time_span if time=TRUE, vector containing minimum and maximum dates over which to measure
#' @param vocab_tbl location of vocabulary table containing concept_id to concept_name mapping. If a vocabulary table is not available, will default to NULL
#'        
conc_process <- function(cohort,
                         multi_or_single_site='multi',
                         age_groups=NULL,
                         codeset_tbl=NULL,
                         care_site,
                         provider,
                         visit_type_tbl=NULL,
                         time=FALSE,
                         time_span=c('2012-01-01', '2020-01-01'),
                         time_period='year',
                         vocab_tbl=NULL){
 
  message('Preparing cohort')
  ## Step 0: Site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  
  ## Step 1: Prepare cohort

  ## Include age groups, if desired
  if(is.data.frame(age_groups)){
    grouped_list_prep<-grouped_list%>%
      append('age_grp')
  }else{grouped_list_prep<-grouped_list}
  
  ### Include visit types, if desired
  if(is.data.frame(visit_type_tbl)){
    grouped_list_prep<-grouped_list_prep%>%
      append('visit_concept_id')
  }
  
  ## Step 2: Run function
  message('Computing specialty concordance')
  site_output<-list()
  # not over time
  if(!time){
    for(k in 1:length(site_list_adj)){
      site_list_thisrnd <- site_list_adj[[k]]
      # filters by site
      cohort_site <- cohort_filter %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))

      conc_site <- compute_conc(cohort=cohort_site,
                                 grouped_list=grouped_list_prep,
                                 codeset_tbl=codeset_tbl,
                                 care_site=care_site,
                                 provider=provider,
                                 visit_type_tbl=visit_type_tbl,
                                 age_gp_tbl=age_groups)
      site_output[[k]]<-conc_site%>%mutate(site=site_list_thisrnd)
    }
    conc_tbl<-reduce(.x=site_output,
                       .f=dplyr::union)

  }
  else{
    # over time
    conc_tbl<-compute_fot(cohort=cohort_filter,
                            site_list=site_list_adj,
                            site_col=site_col,
                            time_span=time_span,
                            time_period=time_period,
                            reduce_id=NULL,
                            check_func=function(dat){
                              compute_conc(cohort=dat,
                                           grouped_list=grouped_list_prep,
                                           codeset_tbl=codeset_tbl,
                                           care_site=care_site,
                                           provider=provider,
                                           visit_type_tbl=visit_type_tbl,
                                           age_gp_tbl=age_groups,
                                           time=TRUE)
                            })
  }

  message('Outputting specialty names to specs directory')
  spec_names<-join_to_vocabulary(tbl=conc_tbl,
                                 vocab_tbl=vocab_tbl,
                                 col='specialty_concept_id')%>%
    distinct(specialty_concept_id, concept_name)%>%
    rename(specialty_concept_name=concept_name)
  output_tbl(spec_names,
             name=paste0('specialty_concept_names_',multi_or_single_site,"_",format(Sys.time(), '%Y%m%d')),
             db=FALSE,
             file=TRUE)

  final_conc_tbl<-replace_site_col(conc_tbl)
  return(final_conc_tbl)
}


#' Concordance: Clinical Events and Specialties -- 
#' @param conc_process_output output from the `conc_process` function
#' @param conc_process_names classified names from the output from `find_distinct_concepts`
#'                            with specialties grouped based on a `specialty_name` column,
#'                            which can be generated by assigning groupings to `specialty_concept_names` or by renaming the `specialty_concept_names` column to `specialty_name` if no grouping is required
#' @param multi_or_single_site string indicating whether to generate output for 'multi' or 'single' site
#' @param anomaly_or_exploratory string indicating whether to generate output for 'anomaly' or 'exploratory' analsis
#' @param time_dimension TRUE if should have a time dimension
#' @param facet_vars vector of variable names to facet by
#' @param color_var variable in conc_process_output to color/fill by
#' @param top_n integer value whose meaning depends on plot context:
#'          for anomaly detection, will display the top n with values furthest from the mean. Defaults to the number of rows in the output, which will be the maximum number of rows regardless of groupings
#' @param n_mad number of MAD from the median for which to flag anomalies
#'                defaults to 3
#' @param alt a temporary parameter to specify whether to display a different type of plot, for testing
#'                    
#' suggestions:
#' if single_site, exploratory: 
#'        facet_vars: codeset_name and/or age_grp
#'        color_var: specialty_name
#' if single_site, anomaly:
#'        facet_vars: codeset_name
#'        color_var: specialty_name
#' if multi_site, exploratory:
#'        facet_vars: codeset_name and/or age_grp and/or visit_type
#'        color_var: keep NULL (i.e. do not pass anything in)
#' if multi_site, anomaly:
#'        facet_vars: codeset_name and/or age_grp and/or visit_type
#'        color_var: site           
conc_output_gen <- function(conc_process_output,
                            conc_process_names,
                            multi_or_single_site,
                            anomaly_or_exploratory,
                            time_dimension,
                            facet_vars,
                            color_var=NULL,
                            top_n=nrow(conc_process_output),
                            n_mad=3L,
                            alt=FALSE,
                            specialty_filter){
  message('Preparing data for visualization')
  if('cluster'%in%facet_vars&'visit_type'%in%facet_vars){
    stop("Can only stratify by visit_type or cluster, not both")
  }
  if(length(facet_vars)>1){
    vars_no_cs<-facet_vars[!facet_vars=='codeset_name']
    gp_vars <- c('codeset_name')%>%append(vars_no_cs)
  }else{
    gp_vars<-c('codeset_name')
  }
  if(time_dimension){
    gp_vars <- gp_vars %>%append(c('time_start','time_increment'))
  }
  if(multi_or_single_site=='multi'){
    gp_vars <- gp_vars %>% append('site')
  }
  if(multi_or_single_site=='single'&anomaly_or_exploratory=='anomaly'){
    gp_vars <- gp_vars %>% append('cluster')
  }
  
  spec_gp_vars <- gp_vars %>% append(c('specialty_name'))
  # compute proportions
  conc_output_denom <- conc_process_output %>%
    inner_join(conc_process_names, by = 'specialty_concept_id')%>%
    group_by(!!!syms(gp_vars))%>%
    summarise(total=sum(num_visits, na.rm=TRUE))%>%
    ungroup()
  
  conc_output_pp <- conc_process_output %>%
    inner_join(conc_process_names, by = 'specialty_concept_id')%>%
    group_by(!!!syms(spec_gp_vars))%>%
    summarise(n=sum(num_visits, na.rm = TRUE))%>%
    ungroup()%>%
    inner_join(conc_output_denom)%>%
    mutate(prop=n/total)
  
  if('concept_id'%in%colnames(conc_output_pp)){
    conc_output_pp <- select(vocabulary_tbl('concept'),c(concept_id, concept_name))%>%
      inner_join(conc_output_pp,by='concept_id', copy=TRUE) %>%
      collect()
  }
  
  # compute Euclidean distance for MS anomaly AT
  if(anomaly_or_exploratory=='anomaly'&multi_or_single_site=='multi'){
    if(time_dimension){
    conc_output_pp<-ms_anom_euclidean(fot_input_tbl=conc_output_pp,
                                      grp_vars=c('site','specialty_name'),
                                      var_col='prop')
    }else{
      gp_vars_no_site<-spec_gp_vars[!spec_gp_vars=='site']
      message('Computing median and distance to median')
      conc_output_pp <- compute_dist_median_conc(tbl=conc_output_pp,
                                                 grp_vars=gp_vars_no_site,
                                                 var_col='prop',
                                                 num_mad=n_mad)

      message('Flagging the anomalies')
      # only select the specialties where at least one site is an anomaly
      conc_output_pp <- flag_anomaly(tbl=conc_output_pp,
                                      facet_vars=facet_vars,
                                      distinct_vars=c('codeset_name', 'specialty_name'))
    }


  }
  if(anomaly_or_exploratory=='anomaly'&multi_or_single_site=='single'&!time_dimension){
    gp_vars_no_site<-spec_gp_vars[!spec_gp_vars=='site'&!spec_gp_vars=='cluster']
    message('Computing mean and distance to mean')
    conc_output_pp <- compute_dist_median_conc(conc_output_pp,
                                               grp_vars=gp_vars_no_site,
                                               var_col='prop',
                                               num_mad=n_mad)
    
    message('Flagging the anomalies')
    # only select the specialties where at least one specialty is an anomaly
    conc_output_pp <- flag_anomaly(tbl=conc_output_pp,
                                           facet_vars=facet_vars,
                                           distinct_vars=c('codeset_name','cluster')) #%>%
      # insert_top_n_indicator(.,
      #                        gp_cols=c("cluster"),
      #                        val_col="n_mad",
      #                        n=top_n)%>%
      # filter(top_n_indicator)
  }
  
  # generate color palette for color variable
  if(!is.null(color_var)){
    color_list <- (conc_output_pp%>%distinct(!!sym(color_var)))%>%pull()
    conc_colors <- generate_color_pal(color_list)
  }
  
  message('Building visualization')
  ## SINGLE SITE, EXPLORATORY
  if(multi_or_single_site=='single'&anomaly_or_exploratory=='exploratory'){
    # over time
    if(time_dimension){
      conc_output_plot <- plot_cnc_sp_ss_exp_at(data_tbl=conc_output_pp,
                                              facet=facet_vars,
                                              pal_map=conc_colors)
      
    }else{
      # not over time
      conc_output_pp <- insert_top_n_indicator(dat=conc_output_pp,
                                               gp_cols=facet_vars,
                                               val_col="prop",
                                               n=top_n)%>%
        filter(top_n_indicator)
      conc_output_plot <- plot_cnc_sp_ss_exp_nt(data_tbl=conc_output_pp,
                                         facet=facet_vars,
                                         x_var='specialty_name',
                                         y_var='prop',
                                         fill_var=color_var,
                                         pal_map=conc_colors,
                                         top_n=top_n)
    }
    
  }else if(multi_or_single_site=='single'&anomaly_or_exploratory=='anomaly'){
    ## SINGLE SITE, ANOMALY
    if(time_dimension){
      # over time
      conc_output_pp <- insert_top_n_indicator(dat=conc_output_pp,
                                               gp_cols=c('cluster'),
                                               val_col="total",
                                               n=top_n,
                                               sum_first=TRUE)%>%
        filter(top_n_indicator)
      
      conc_output_plot<-plot_cnc_sp_ss_an_at(process_output=conc_output_pp,
                                             filt_list=list(specialty_name=specialty_filter),
                                             ct_col='n',
                                             id_col='cluster',
                                             denom_col='total',
                                             name_col='specialty_name',
                                             facet=c('specialty_name', 'cluster'),
                                             plot_title_text=paste0('Visits with specialties'))
    }else{
      # not over time
      conc_output_plot <- plot_cnc_sp_ss_an_nt(data_tbl=conc_output_pp)
    }
    
  }else if(multi_or_single_site=='multi'&anomaly_or_exploratory=='exploratory'){
    ## MULTI SITE, EXPLORATORY
    if(time_dimension){
      # over time
      conc_output_pp<-insert_top_n_indicator(conc_output_pp,
                                             gp_cols=c('specialty_name'),
                                             val_col = "n",
                                             n=top_n,
                                             sum_first=TRUE)%>%
        filter(top_n_indicator)
      facet_vars <- facet_vars %>% append(c('specialty_name'))
      conc_output_plot <- plot_cnc_sp_ms_exp_at(data_tbl=conc_output_pp,
                                              facet=facet_vars,
                                              pal_map=conc_colors)
    }else{
      # not over time
      conc_output_plot <- plot_cnc_sp_ms_exp_nt(data_tbl=conc_output_pp,
                                                    pal_map=conc_colors)
    }
    ## MULTI SITE, ANOMALY
  }else if(multi_or_single_site=='multi'&anomaly_or_exploratory=='anomaly'){
    if(time_dimension){
      # over time
      conc_output_plot <- plot_cnc_sp_ms_an_at(process_output=conc_output_pp,
                                               grp_vars=c('specialty_name', 'time_start', 
                                                          'mean_allsiteprop'),
                                               filt_list=list(specialty_name=specialty_filter))
                                               
    }else{
      # not over time
      conc_output_plot <- plot_cnc_sp_ms_an_nt(data_tbl=conc_output_pp)
    }
    
  }
  return(conc_output_plot)
  #return(conc_output_pp)
}
