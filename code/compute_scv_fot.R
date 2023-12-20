

compute_fot_scv <- function(cohort,
                            code_type,
                            concept_set,
                            code_domain,
                            time_period='year',
                            time_span= c('2012-01-01','2022-12-31'),
                            site_list=list('stanford',
                                           'colorado',
                                           'chop'),
                           domain_tbl=read_codeset('scv_domains','cccc')
) {
  
  if(!is.vector(concept_set)){stop('For an over time output, please select 1-5 codes from your
                                   concept set and include them as a vector in the concept_set argument.')}
  if(is.vector(concept_set) && length(concept_set) > 5){stop('For an over time output, please select 1-5 
                                                              codes from your concept set and include them as
                                                             a vector in the concept_set argument.')}
  
  site_list_v <- unlist(site_list)
  
  final_results <- list()
  
  t1 <- seq(from=ymd(time_span[[1]]),to=ymd(time_span[[2]]),by=time_period)
  t2 <- ceiling_date(t1, time_period) - 1
  
  
  # narrows the visit time to cohort_entry and end date
  for(k in 1:length(t1)) {
    
    message(paste0('Starting ',t1[[k]]))
    
    target <- ymd(t1[[k]])
    
    baseline_start_date <- target
    baseline_end_date <- ceiling_date(target, time_period) - 1
    
    cohort_narrowed <- cohort %>% 
      mutate(start_date = as_date(baseline_start_date),
             end_date = as_date(baseline_end_date))
    
    
    cohort_narrow_prepped <- cohort_narrowed %>%
      filter(site %in% site_list_v) %>% mutate(time_period=start_date,
                                               time_increment=time_period)
    
    concept_set_prep <- as.data.frame(concept_set) %>% rename('concept_id' = concept_set) %>%
      mutate(concept_id = as.integer(concept_id))
    concept_set_prep <- copy_to_new(df = concept_set_prep)
    
    scv <- check_code_dist(cohort = cohort_narrow_prepped,
                           concept_set = concept_set_prep,
                           code_type = code_type,
                           code_domain = code_domain,
                           time = TRUE) %>% mutate(start_date = target,
                                                   end_date = baseline_end_date,
                                                   time_increment=time_period)
    
    # scv_reduced <- dplyr::bind_rows(scv, .id='visit_type') %>% 
    #   mutate(time_increment=time_period)
    
    final_results[[k]] <- scv
    
  }
  
  # final_results[[paste0(check_string,'_meta')]] <- meta
  output = reduce(.x=final_results,
                  .f=dplyr::union)
  
  return(output)
  
}


## single site exploratory (use as multi site and just facet by site?)
scv_ss_exp_at <- function(scv_process,
                          output,
                          facet){
  
  if(output == 'source_prop'){
    facet <- facet %>% append('source_concept_id')
    color <- 'concept_id'
  }else if(output == 'concept_prop'){
    facet <- facet %>% append('concept_id')
    color <- 'source_concept_id'
  }else{stop(paste0('invalid output selected: column ', output, ' does not exist'))}
  
  p <- scv_process %>%
    mutate(concept_id = as.character(concept_id),
           source_concept_id = as.character(source_concept_id)) %>%
    ggplot(aes(y = !!sym(output), x = start_date, color = !!sym(color))) +
    geom_line() +
    facet_wrap((facet)) +
    labs(title = 'Code Mapping Pairs Over Time')
  
  ggplotly(p)
  
}


## need to figure out how to plot multi site anomaly
## need to figure out what to do for single site anomaly
produce_multisite_mad_scv <- function(multisite_tbl,
                                      code_type,
                                      facet_var = NULL,
                                      mad_dev) {
  if(code_type == 'source'){
    concept_col <- 'source_concept_id'
  }else(concept_col <- 'concept_id')
  
  
  if(is.null(facet_var)){
    grp1 <- c('start_date', 'centroid', 'source_concept_id', 'concept_id')
    grp2 <- c('site', concept_col)
  }else{
    grp1 <- c('start_date', 'centroid', 'source_concept_id', 'concept_id', facet_var)
    grp2 <- c('site', concept_col, facet_var)
  }
  
  mad_computation <- 
    multisite_tbl %>% 
    group_by(!!!syms(grp1)) %>% 
    summarise(mad_pt=mad(check, center=centroid)) %>% 
    ungroup() %>% 
    mutate(lower_mad = mad_pt - (abs(mad_pt*mad_dev)),
           upper_mad = mad_pt + (abs(mad_pt*mad_dev)))
  
  full_tbl_outliers <- 
    multisite_tbl %>% ungroup() %>% 
    inner_join(mad_computation) %>% 
    mutate(
      outlier=case_when((distance < lower_mad) | (distance > upper_mad) ~ 1,
                        TRUE ~ 0)
    ) %>% filter(! site=='all')
  
  sites_grp_outliers <- 
    full_tbl_outliers %>% 
    group_by(!!!syms(grp2)) %>% 
    filter(outlier==1) %>% 
    summarise(grp_outlier_num=n()) %>%  ungroup() 
  
  sites_grp_ct_total <- 
    full_tbl_outliers %>% 
    group_by(!!!syms(grp2)) %>% 
    summarise(grp_total_num=n()) %>% ungroup()
  
  sites_grp_total <- 
    sites_grp_ct_total %>% 
    left_join(sites_grp_outliers) %>% 
    mutate(grp_outlier_prop = round(grp_outlier_num/grp_total_num,2))
  
  sites_total <- 
    sites_grp_total %>% ungroup() %>% 
    group_by(site) %>% 
    mutate(site_total_num=sum(grp_total_num, na.rm = TRUE),
           site_total_outlier=sum(grp_outlier_num, na.rm = TRUE)) %>% 
    mutate(site_outlier_prop=round(site_total_outlier/site_total_num,2)) 
  
}

scv_ms_anom_at <- function(scv_process,
                           output,
                           facet,
                           mad_dev){
  
  facet <- facet %>% append(c('concept_id', 'source_concept_id'))
  
  if(output == 'source_prop'){
    y_col <- 'source_concept_id'
    code_type <- 'source'
  }else{y_col <- 'concept_id'
  code_type <- 'cdm'}
  
  fot <- fot_check(tblx = scv_process %>% ungroup(),
                   target_col = output,
                   facet_var = !!syms(facet))
  
  fot2 <- check_fot_all_dist(fot_check_output = fot$fot_heuristic)
  
  mad <- produce_multisite_mad_scv(multisite_tbl = fot2,
                                   code_type = code_type,
                                   mad_dev = mad_dev)
  
  r <- ggplot(mad, aes(x=site, y=as.character(!!sym(y_col)), fill=grp_outlier_prop)) +
    geom_tile() +
    facet_wrap((facet)) +
    theme_classic() +
    coord_flip() +
    labs(title = 'Codes with Representative Proportions of Mappings +/- 2 MAD away from Median',
         y = 'Code')
}


scv_ss_anom_at <- function(scv_process,
                           output,
                           facet){
  
  if(output == 'source'){
    col <- 'source_concept_id'
  }else{(col <- 'concept_id')}
  
  facet <- facet %>% append(col)
  
  n_mappings_yr <- scv_process_test %>%
    group_by(!!!syms(facet), start_date) %>%
    summarise(n_mappings = n())
  
  code_list <- n_mappings_yr %>% ungroup() %>% distinct(!!sym(col)) %>% pull()
  

  qicharts2::qic(x = start_date,
                 y = n_mappings,
                 data = n_mappings_yr,
                 chart = 'c',
                 facets = ~ concept_id)
  
  ggplot(n_mappings_yr, aes(x = start_date, y = n_mappings)) +  
    geom_point() + geom_line() + 
    facet_wrap((facet)) + 
    ggQC::stat_QC(method = 'c',
                  color.qc_limits = '#ad131d',
                  color.qc_center = '#19753f') + 
    theme_bw()
  
}
