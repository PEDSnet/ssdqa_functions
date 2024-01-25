#' Concordance: Clinical Events and Specialties
#' 
#' This is a check that will assess quality of specialty data in a study sample
#' The user will provide clinical codesets of interest with associated domains
#' and will be able to stratify results by:
#'                age group (user will provide groupings)
#'                visit type (user will provide groupings)
#'                time (by year)
#' @param cohort - A dataframe with the cohort of patients for your study. Should include the columns:
#'                       - @person_id
#' @param grouped_list a list containing the variables to group by:
#'                can contain:
#'                  `site` if want to stratify by site (and there is a `site` column in the input data)
#'@param age_groups- If you would like to stratify the results by age group, fill out the provided `age_group_definitions.csv` file
#'                     with the following information:
#'                     - @min_age: the minimum age for the group (i.e. 10)
#'                     - @max_age: the maximum age for the group (i.e. 20)
#'                     - @group: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'                     
#'                     Then supply this csv file as the age_groups argument (i.e. read.csv('path/to/age_group_definitions.csv'))
#'                     If you do not wish to stratify by age, keep as NULL
#' @param codeset_tbl table in the specs directory with the columns:
#'                        domain: name of the domain
#'                        default_tbl: name of the cdm_tbl
#'                        field_name: column name in the default_tbl for which to search the codeset concept_ids
#'                        codeset_name: name of a codeset in the specs directory
#' @param care_site TRUE if want to look at care_site specialty
#'                  FALSE if do not want to look at care_site specialty
#' @param provider TRUE if want to look at provider specialty
#'                  FALSE if do not want to look at provider specialty
#'                  IF both `provider` and `care_site` are both TRUE,
#'                        provider specialty will be prioritized if provider and care_site are discordant for the visit
#' @param visit_type_tbl - a csv file that defines available visit types that are called in @visit_types. defaults to the provided
#'                           `conc_visit_types.csv` file, which contains the following fields:
#'                           - @visit_concept_id: the visit_concept_id that represents the visit type of interest (i.e. 9201)
#'                           - @visit_type: the string label to describe the visit type; this label can be used multiple times
#'                                          within the file if multiple visit_concept_ids represent the visit type
#'                          
#'                           This CSV can be altered to fit the users needs, or another csv with the same columns and formatting can be supplied.
#' @param time TRUE if results should be over time. Defaults to FALSE
#' @param time_period if time=TRUE, indicates time period (e.g. 'year', 'month') over which to measure
#' @param time_span if time=TRUE, vector containing minimum and maximum dates over which to measure
#' @param site_list if supplied, sites to filter the results to
#'        
conc_process <- function(cohort,
                         grouped_list=c('site'),
                         age_groups=NULL,
                         codeset_tbl=NULL,
                         care_site,
                         provider,
                         visit_type_tbl=NULL,
                         time=FALSE,
                         time_span=c('2012-01-01', '2020-01-01'),
                         time_period='year',
                         site_list=NULL){
  ## Step 0: Set cohort name for table output
  #config('cohort', study_name)
  message('Preparing cohort')
  ## Step 1: Prepare cohort
  
  ### Include time component, if desired
  # if(time){
  #   grouped_list <- grouped_list%>%append('year')
  # }
  ### Include age groups, if desired
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
  if(!time){
    site_list_v <- unlist(site_list)
    cohort<-cohort%>%filter(site%in%site_list_v)
    conc_final <- compute_conc(cohort=cohort,
                               grouped_list=grouped_list_prep,
                               codeset_tbl=codeset_tbl,
                               care_site=care_site,
                               provider=provider,
                               visit_type_tbl=visit_type_tbl,
                               age_gp_tbl=age_groups)
  }
  else{
    conc_final<-compute_fot(cohort=cohort,
                            site_list=site_list,
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
  spec_concept_names <- find_distinct_concepts(conc_final)
  output_tbl(spec_concept_names,
             name='specialty_concept_names',
             db=FALSE,
             file=TRUE)
  return(conc_final)
}


#' Concordance: Clinical Events and Specialties -- 
#' @param conc_process_output output from the `conc_process` function
#' @param conc_process_names classified names from the output from `find_distinct_concepts`
#'                            with specialties grouped based on a `specialty_name` column,
#'                            which can be generated by assigning groupings to `specialty_concept_names` or by renaming the `specialty_concept_names` column to `specialty_name` if no grouping is required
#' @param single_site TRUE if should be a single site analysis
#' @param multi_site TRUE if should be multi site analysis
#' @param exploratory TRUE if should be exploratory analysis
#' @param anomaly TRUE if should be anomoly detection
#' @param time_dimension TRUE if should have a time dimension
#' @param facet_vars vector of variable names to facet by
#' @param color_var variable in conc_process_output to color/fill by
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
                            single_site,
                            multi_site,
                            exploratory,
                            anomaly,
                            time_dimension,
                            facet_vars,
                            color_var=NULL){
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
    gp_vars <- gp_vars %>%append('year')
  }
  if(multi_site){
    gp_vars <- gp_vars %>% append('site')
  }
  if(single_site&anomaly){
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
  
  # compute means and sd for anomaly detection
  if(anomaly&multi_site){
    gp_vars_no_site<-spec_gp_vars[!spec_gp_vars=='site']
    message('Computing mean and distance to mean')
    conc_output_pp <- compute_dist_mean_conc(conc_output_pp,
                                             grp_vars=gp_vars_no_site,
                                             var_col='prop',
                                             num_sd=2L)
    
    message('Flagging the anomalies')
    # only select the specialties where at least one site is an anomaly
    conc_output_pp <- flag_anomaly(tbl=conc_output_pp,
                                   facet_vars=facet_vars,
                                   distinct_vars=c('codeset_name', 'specialty_name'))
  }
  if(anomaly&single_site){
    gp_vars_no_site<-spec_gp_vars[!spec_gp_vars=='site'&!spec_gp_vars=='cluster']
    message('Computing mean and distance to mean')
    conc_output_pp <- compute_dist_mean_conc(conc_output_pp,
                                             grp_vars=gp_vars_no_site,
                                             var_col='prop',
                                             num_sd=2L)
    
    message('Flagging the anomalies')
    # only select the specialties where at least one specialty is an anomaly
    # conc_output_pp <- flag_anomaly(tbl=conc_output_pp,
    #                                        facet_vars=facet_vars,
    #                                        distinct_vars=c('codeset_name','cluster'))
  }
  
  # generate color palette for color variable
  if(!is.null(color_var)){
    color_list <- (conc_output_pp%>%distinct(!!sym(color_var)))%>%pull()
    conc_colors <- generate_color_pal(color_list)
  }
  
  message('Building visualization')
  if(single_site&exploratory){
    if(time_dimension){
      # single site, exploratory, over time
      conc_output_plot <- plot_ss_exp_ot(data_tbl=conc_output_pp,
                                         facet=facet_vars,
                                         color_var=color_var,
                                         pal_map=conc_colors)
      
    }else{
      # single site, exploratory, no time
      conc_output_plot <- conc_ss_exp_nt(data_tbl=conc_output_pp,
                                         facet=facet_vars,
                                         x_var='specialty_name',
                                         y_var='prop',
                                         fill_var=color_var,
                                         pal_map=conc_colors)
    }
    
  }else if(single_site&anomaly){
    if(time_dimension){
      
    }else{
      # single site, anomaly, no time
      # conc_output_plot <- plot_an_nt(data_tbl=conc_output_pp,
      #                                   x_var='cluster',
      #                                   y_var='prop',
      #                                   fill_var=color_var,
      #                                   facet=facet_vars,
      #                                   pal_map=conc_colors)
      conc_output_plot <- plot_ss_an_nt_conc(data_tbl=conc_output_pp)
    }
    
  }else if(multi_site&exploratory){
    if(time_dimension){
      
    }else{
      # multi-site, exploratory, no time
      conc_output_plot <- plot_ms_exp_nt(data_tbl=conc_output_pp,
                                         x_var='site',
                                         y_var='specialty_name',
                                         fill_var='prop',
                                         facet=facet_vars)
      # conc_output_plot <- plot_conc_ms_exp_dotplot(data_tbl=conc_output_pp,
      #                                              pal_map=conc_colors)
    }
    
  }else if(multi_site&anomaly){
    if(time_dimension){
      
    }else{
      # multi-site, anomaly, no time
      conc_output_plot <- plot_an_nt(data_tbl=conc_output_pp,
                                     x_var='specialty_name',
                                     y_var='prop',
                                     fill_var=color_var,
                                     facet=facet_vars,
                                     pal_map=conc_colors)
    }
    
  }
  return(conc_output_plot)
  #return(conc_output_pp)
}
