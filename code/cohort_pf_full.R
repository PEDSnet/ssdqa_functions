

pf_process <- function(cohort = cohort,
                       site_list = c('seattle','cchmc'),
                       study_name = 'glom',
                       visit_types = c('outpatient','inpatient'),
                       multi_or_single_site = 'single',
                       collapse_sites = FALSE,
                       #grouped_list = c('site', 'person_id', 'start_date', 
                                        #'end_date', 'fu'),
                       time = FALSE,
                       time_span = c('2015-01-01', '2023-01-01'),
                       age_groups = read_codeset('age_group_definitions','iic'),
                       codeset = NULL,
                       anomaly_or_exploratory='exploratory',
                       domain_tbl=read_codeset('pf_domains_short','cccc'),
                       visit_type_table=read_codeset('pf_visit_types','ic')){
  
  ## Step 0: Set cohort name for table output
  config('cohort', study_name)
  
  ## Step 1: Prepare cohort
  
  cohort_prep <- prepare_pf(cohort = cohort, age_groups = age_groups, codeset = codeset)
  
  ## Step 2: Run Function
  #### NEED TO FIGURE OUT WHETHER WE WANT TO REQUIRE A SITE COLUMN
  #grouped_list <- c('site', 'person_id','person_id','start_date','end_date','fu')
  
  cols <- colnames(cohort)
  
  if(! 'site' %in% cols){cohort <- cohort %>% mutate(site = 'all')}
  
  grouped_list <- c('site','person_id','start_date','end_date','fu')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  if(is.data.frame(codeset)){grouped_list <- grouped_list %>% append('flag')}
  
  if(time){
    
    grouped_list <- grouped_list[! grouped_list %in% 'fu']
    
    pf_final <- compute_fot_pf(cohort = cohort_prep,
                             grouped_list=grouped_list,
                             time_period='year',
                             time_span= time_span,
                             collapse_sites = collapse_sites,
                             visit_type_tbl=visit_type_table,
                             site_list=site_list,
                             visit_list=visit_types,
                             domain_tbl=domain_tbl)
    
    #output_tbl(pf_tbl, 'pf_fot')
    
    #pf_final <- pf_tbl
    
  } else {
    pf_tbl <- loop_through_visits(
      cohort_tbl=cohort_prep,
      time = FALSE,
      collapse_sites=collapse_sites,
      site_list=site_list,
      visit_list=visit_types,
      visit_type_tbl=visit_type_table,
      grouped_list=grouped_list,
      domain_tbl = domain_tbl
    )
    
    ### NEED DOCUMENTATION SAYING THAT THE VISIT LABEL WILL ALSO BE THE OUTPUT OF THE TABLE HERE
    ### SHOULD WE MAKE ALL THESE TABLES TEMPORARY? PROVIDE AN OPTION TO USERS? WHY ARE WE EVEN OUTPUTTING THIS TABLE? 
    ### SHOULD THE OUTPUT OF THE TABLE BE AN OPTION?
    #output_list_to_db(pf_tbl)
    
    
    ### NEED TO MAKE SURE THAT CREATING LONG TABLE IS A GOOD DECISION FOR REPRODUCIBILITY 
    pf_final <- combine_study_facts(pf_tbl=pf_tbl,
                                    domain_list = domain_tbl,
                                    study_abbr = study_name, 
                                    time = time, 
                                    visit_type_list = visit_types) %>% collect()
  }
  
  ## Step 3: Summarise (Medians, SD) --- KALEIGH TO CHECK TO MAKE SURE THE CODE BELOW COVERS THIS

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




pf_output_gen <- function(pf_process,
                          site_list,
                          colors = 'auto',
                          visit_types = c('all'),
                          multi_or_single_site = 'single',
                          time = FALSE,
                          time_span = c('2012-01-01', '2023-01-01'),
                          age_groups = FALSE,
                          codeset = FALSE,
                          anomaly_or_exploratory = 'exploratory',
                          domain_tbl=read_codeset('pf_domains_short','cccc'),
                          save_as_png = FALSE,
                          file_path = NULL){
  
  ## Create Brewer color palettes for sites and domains
  if(colors == 'auto'){
    site_names <- site_list
    site_palette_base <- colorRampPalette(brewer.pal(8, "Dark2"))
    site_color <- setNames(site_palette_base(length(site_names)), site_names)
    as.data.frame(site_color) %>% rownames_to_column('site') %>%
      write.csv(file.path(base_dir, 'specs/site_color_config.csv'), row.names = FALSE)
    
    domain_names <- domain_tbl %>% select(domain) %>% pull()
    domain_palette_base <- colorRampPalette(brewer.pal(8, "Dark2"))
    domain_color <- setNames(domain_palette_base(length(domain_names)), domain_names)
    as.data.frame(domain_color) %>% rownames_to_column('site') %>%
      write.csv(file.path(base_dir, 'specs/domain_color_config.csv'), row.names = FALSE)
  }else{
    site_color <- read_codeset('site_color_custom', 'cc')
    write.csv(site_color, file.path(base_dir, 'specs/site_color_config.csv'), row.names = FALSE)
    
    domain_color <- read_codeset('domain_color_custom', 'cc')
    write.csv(domain_color, file.path(base_dir, 'specs/domain_color_config.csv'), row.names = FALSE)
  }
  
  ## Generate appropriate output based on selections
  if(time){
    if(multi_or_single_site == 'single'){
      if(anomaly_or_exploratory == 'anomaly'){
        if(age_groups){
          output <- pf_ss_anom_at(data_tbl = pf_output,
                                  site_list = site_list,
                                  age_groups = TRUE)
          label <- "pf_ss_age_anom_at"
        }else if(codeset){
          output <- pf_ss_anom_at(data_tbl = pf_output,
                                  site_list = site_list,
                                  codeset = TRUE)
          label <- "pf_ss_code_anom_at"
        }else if(length(visit_types) > 1){
          output <- pf_ss_anom_at(data_tbl = pf_output,
                                  site_list = site_list,
                                  visit_types = TRUE)
          label <- "pf_ss_visit_anom_at"
        }else{
          output <- pf_ss_anom_at(data_tbl = pf_output,
                                  site_list = site_list)
          label <- "pf_ss_nostrat_anom_at"
      }
        } else {
        if(age_groups){
          output <- pf_ss_exp_at(data_tbl = pf_output,
                                 site_list = site_list,
                                 domain_colors = domain_colors,
                                 age_groups = TRUE)
          label <- "pf_ss_age_exp_at"
        }else if(codeset){
          output <- pf_ss_exp_at(data_tbl = pf_output,
                                 site_list = site_list,
                                 domain_colors = domain_colors,
                                 codeset = TRUE)
          label <- "pf_ss_code_exp_at"
        }else if(length(visit_types) > 1){
          output <- pf_ss_exp_at(data_tbl = pf_output,
                                 site_list = site_list,
                                 domain_colors = domain_colors,
                                 visit_types = TRUE)
          label <- "pf_ss_visit_exp_at"
        }else{
          output <- pf_ss_exp_at(data_tbl = pf_output,
                                 domain_colors = domain_colors,
                                 site_list = site_list)
          label <- "pf_ss_nostrat_exp_at"
    }
          }
      } else {
      if(anomaly_or_exploratory == 'anomaly'){
        if(age_groups){
          output <- pf_ms_anom_at(data_tbl = pf_output,
                                  domain_list = domain_tbl,
                                  age_groups = TRUE)
          label <- "pf_ms_age_anom_at"
        }else if(codeset){
          output <- pf_ms_anom_at(data_tbl = pf_output,
                                  domain_list = domain_tbl,
                                  codeset = TRUE)
          label <- "pf_ms_code_anom_at"
        }else if(length(visit_types) > 1){
          output <- pf_ms_anom_at(data_tbl = pf_output,
                                  domain_list = domain_tbl,
                                  visit_types = TRUE)
          label <- "pf_ms_visit_anom_at"
        }else{
          output <- pf_ms_anom_at(data_tbl = pf_output,
                                  domain_list = domain_tbl)
          label <- "pf_ms_nostrat_anom_at"
        }
      } else {
        if(age_groups){
          output <- pf_ms_exp_at(data_tbl = pf_output,
                                 site_colors = site_colors,
                                 domain_list = domain_tbl,
                                 time_span = time_span,
                                 age_groups = TRUE)
          label <- "pf_ms_age_exp_at"
        }else if(codeset){
          output <- pf_ms_exp_at(data_tbl = pf_output,
                                 site_colors = site_colors,
                                 domain_list = domain_tbl,
                                 time_span = time_span,
                                 codeset = TRUE)
          label <- "pf_ms_code_exp_at"
        }else if(length(visit_types) > 1){
          output <- pf_ms_exp_at(data_tbl = pf_output,
                                 site_colors = site_colors,
                                 domain_list = domain_tbl,
                                 time_span = time_span,
                                 visit_types = TRUE)
          label <- "pf_ms_visit_exp_at"
        }else{
          output <- pf_ms_exp_at(data_tbl = pf_output,
                                 site_colors = site_colors,
                                 domain_list = domain_tbl,
                                 time_span = time_span)
          label <- "pf_ms_nostrat_exp_at"
        }
      }
    }
  } else {
    if(multi_or_single_site == 'single'){
      if(anomaly_or_exploratory == 'anomaly'){
        if(age_groups){
          output <- pf_ss_anom_nt(data_tbl = pf_output,
                                  site_list = site_list,
                                  domain_colors = domain_colors,
                                  age_groups = TRUE)
          label <- "pf_ss_age_anom_nt"
        }else if(codeset){
          output <- pf_ss_anom_nt(data_tbl = pf_output,
                                  site_list = site_list,
                                  domain_colors = domain_colors,
                                  codeset = TRUE)
          label <- "pf_ss_code_anom_nt"
        }else if(length(visit_types) > 1){
          output <- pf_ss_anom_nt(data_tbl = pf_output,
                                  site_list = site_list,
                                  domain_colors = domain_colors,
                                  visit_types = TRUE)
          label <- "pf_ss_visit_anom_nt"
        }else{
          output <- pf_ss_anom_nt(data_tbl = pf_output,
                                  domain_colors = domain_colors,
                                  site_list = site_list)
          label <- "pf_ss_nostrat_anom_nt"
        }
      } else {
        if(age_groups){
          output <- pf_ss_exp_nt(data_tbl = pf_output,
                                 site_list = site_list,
                                 site_colors = site_colors,
                                 age_groups = TRUE)
          label <- "pf_ss_age_exp_nt"
        }else if(codeset){
          output <- pf_ss_exp_nt(data_tbl = pf_output,
                                 site_list = site_list,
                                 site_colors = site_colors,
                                 codeset = TRUE)
          label <- "pf_ss_code_exp_nt"
        }else if(length(visit_types) > 1){
          output <- pf_ss_exp_nt(data_tbl = pf_output,
                                 site_list = site_list,
                                 site_colors = site_colors,
                                 visit_types = TRUE)
          label <- "pf_ss_visit_exp_nt"
        }else{
          output <- pf_ss_exp_nt(data_tbl = pf_output,
                                 site_list = site_list,
                                 site_colors = site_colors)
          label <- "pf_ss_nostrat_exp_nt"
        }
      }
    } else {
      if(anomaly_or_exploratory == 'anomaly'){
        if(age_groups){
          output <- pf_ms_anom_nt(data_tbl = pf_output,
                                  age_groups = TRUE)
          label <- "pf_ms_age_anom_nt"
        }else if(codeset){
          output <- pf_ms_anom_nt(data_tbl = pf_output,
                                  codeset = TRUE)
          label <- "pf_ms_code_anom_nt"
        }else if(length(visit_types) > 1){
          output <- pf_ms_anom_nt(data_tbl = pf_output,
                                  visit_types = TRUE)
          label <- "pf_ms_visit_anom_nt"
        }else{
          output <- pf_ms_anom_nt(data_tbl = pf_output)
          label <- "pf_ms_nostrat_anom_nt"
        }
      } else{
        if(age_groups){
          output <- pf_ms_exp_nt(data_tbl = pf_output,
                                 age_groups = TRUE)
          label <- "pf_ms_age_exp_nt"
        }else if(codeset){
          output <- pf_ms_exp_nt(data_tbl = pf_output,
                                 codeset = TRUE)
          label <- "pf_ms_code_exp_nt"
        }else if(length(visit_types) > 1){
          output <- pf_ms_exp_nt(data_tbl = pf_output,
                                 visit_types = TRUE)
          label <- "pf_ms_visit_exp_nt"
        }else{
          output <- pf_ms_exp_nt(data_tbl = pf_output)
          label <- "pf_ms_nostrat_exp_nt"
        }
      }
    }
  }
  
  ## ggsave graphs as .png if requested
  if(save_as_png){
    
    single_op <- c('pf_ms_age_anom_at', 'pf_ms_code_anom_at','pf_ms_visit_anom_at','pf_ms_nostrat_anom_at',
                   'pf_ms_age_exp_nt', 'pf_ms_code_exp_nt','pf_ms_visit_exp_nt','pf_ms_nostrat_exp_nt')
    
    int_labels <- c("pf_ms_age_exp_nt","pf_ms_code_exp_nt","pf_ms_visit_exp_nt",
                    "pf_ms_nostrat_exp_nt","pf_ms_age_exp_at","pf_ms_code_exp_at",
                    "pf_ms_visit_exp_at","pf_ms_nostrat_exp_at")
    
    if(label %in% single_op){
      
      if(label %in% int_labels){
        
        htmltools::save_html(html = output, file = paste0(file_path, '/', label, '.html'))
        
      }else{
      
      ggsave(plot = output,
             height = 15,
             width = 25,
             units = c('cm'),
             filename = paste0(file_path, '/', label, '.png'))
        }
    }else{
      
      for(plot in 1:length(output)){
        
        if(!(label %in% int_labels)){
        
        num <- plot
        
        ggsave(plot = output[[plot]],
               height = 15,
               width = 25,
               units = c('cm'),
               filename = paste0(file_path, '/', label, '_', num, '.png'))
        }else{
          num <- plot
          htmltools::save_html(html = output[[plot]], file = paste0(file_path, '/', label, '_', num, '.html'))}
      }
      }
  }
  
  ## Return "raw" output in place of or in addition to .png files
  return(output)
}
  