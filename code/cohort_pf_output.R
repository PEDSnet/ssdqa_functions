
#' **Single Site, Anomaly, Over Time**
pf_ss_anom_at <- function(data_tbl,
                          domain_list,
                          age_groups = FALSE,
                          codeset = FALSE,
                          visit_type = FALSE){
  
  domain_list <- domain_list %>% select(domain) %>% pull()
  
  output_list <- list()
  
  for(i in 1:length(domain_list)){
    
    data_filter <- data_tbl %>% filter(domain == domain_list[[i]])
    
    if(age_groups){
      r <- plot_anomaly_diagnostics(.data=data_filter %>% filter(visit_type == 'all'), 
                                    .facet_vars = c(site, age_grp), 
                                    .date_var = start_date, 
                                    .value=median_fact_ct, 
                                    .alpha=0.10, 
                                    .legend_show = FALSE,
                                    .max_anomalies = 0.5, 
                                    .title = paste0('Median ', domain_list[[i]], ' Across Time: 
                               Anomaly Detection Across Sites'),
                                    .facet_ncol = 3, 
                                    .facet_scales = 'free_x', 
                                    .facet_collapse = TRUE, 
                                    .interactive = FALSE)
    }else if(codeset){
      r <- plot_anomaly_diagnostics(.data=data_filter %>% filter(visit_type == 'all'), 
                                    .facet_vars = c(site, flag), 
                                    .date_var = start_date, 
                                    .value=median_fact_ct, 
                                    .alpha=0.10, 
                                    .legend_show = FALSE,
                                    .max_anomalies = 0.5, 
                                    .title = paste0('Median ', domain_list[[i]], ' Across Time: 
                               Anomaly Detection Across Sites'),
                                    .facet_ncol = 3, 
                                    .facet_scales = 'free_x', 
                                    .facet_collapse = TRUE, 
                                    .interactive = FALSE)
    }else if(visit_type){
      r <- plot_anomaly_diagnostics(.data=data_filter, 
                                    .facet_vars = c(site, visit_type), 
                                    .date_var = start_date, 
                                    .value=median_fact_ct, 
                                    .alpha=0.10, 
                                    .legend_show = FALSE,
                                    .max_anomalies = 0.5, 
                                    .title = paste0('Median ', domain_list[[i]], ' Across Time: 
                               Anomaly Detection Across Sites'),
                                    .facet_ncol = 3, 
                                    .facet_scales = 'free_x', 
                                    .facet_collapse = TRUE, 
                                    .interactive = FALSE)
    }else{
      r <- plot_anomaly_diagnostics(.data=data_filter %>% filter(visit_type == 'all'), 
                                    .facet_vars = c(site), 
                                    .date_var = start_date, 
                                    .value=median_fact_ct, 
                                    .alpha=0.10, 
                                    .legend_show = FALSE,
                                    .max_anomalies = 0.5, 
                                    .title = paste0('Median ', domain_list[[i]], ' Across Time: 
                               Anomaly Detection Across Sites'),
                                    .facet_ncol = 3, 
                                    .facet_scales = 'free_x', 
                                    .facet_collapse = TRUE, 
                                    .interactive = FALSE)
    }
    
    output_list[[i]] <- r
  }
  
  output_list
  
}



#' **Single Site, Exploratory, Over Time**

pf_ss_exp_at <- function(data_tbl,
                         site_list,
                         age_groups = FALSE,
                         codeset = FALSE,
                         visit_types = FALSE,
                         x_axis = 'start_date',
                         y_axis = 'median_fact_ct',
                         date_breaks_str = '1 year'){
  
  site_list <- site_list
  
  output_list <- list()
  
  for(i in 1:length(site_list)){
    
    data_filter <- data_tbl %>% filter(site == site_list[[i]])
    
    if(age_groups){
      r <- ggplot(data_filter %>% filter(visit_type == 'all'), 
                  aes(x=!! sym(x_axis), y=!! sym(y_axis), group=domain, fill=domain)) +
        geom_point(aes(color=domain)) +
        geom_smooth(method='loess',formula=y~x, size=0.5) +
        facet_wrap(~age_grp, scales = 'free_y') +
        scale_fill_manual(values=domain_colors) +
        scale_color_manual(values=domain_colors) + 
        theme_classic() +
        scale_x_date(date_breaks=date_breaks_str) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle(paste0(site_list[[i]], ', age groups : patient facts per domain')) +
        labs(x='Time Period', y='Median Facts Per Patient')
    }else if(codeset){
      r <- ggplot(data_filter %>% filter(visit_type == 'all'), 
                  aes(x=!! sym(x_axis), y=!! sym(y_axis), group=domain, fill=domain)) +
        geom_point(aes(color=domain)) +
        geom_smooth(method='loess',formula=y~x, size=0.5) +
        facet_wrap(~flag, scales = 'free_y') +
        scale_fill_manual(values=domain_colors) +
        scale_color_manual(values=domain_colors) + 
        theme_classic() +
        scale_x_date(date_breaks=date_breaks_str) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle(paste0(site_list[[i]], ', codeset utilization : patient facts per domain')) +
        labs(x='Time Period', y='Median Facts Per Patient')
    }else if(visit_types){
      r <- ggplot(data_filter, aes(x=!! sym(x_axis), y=!! sym(y_axis), group=domain, fill=domain)) +
        geom_point(aes(color=domain)) +
        geom_smooth(method='loess',formula=y~x, size=0.5) +
        facet_wrap(~visit_type, scales = 'free_y') +
        scale_fill_manual(values=domain_colors) +
        scale_color_manual(values=domain_colors) + 
        theme_classic() +
        scale_x_date(date_breaks=date_breaks_str) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle(paste0(site_list[[i]], ', visit types : patient facts per domain')) +
        labs(x='Time Period', y='Median Facts Per Patient')
    }else{
      r <- ggplot(data_filter %>% filter(visit_type == 'all'), 
                  aes(x=!! sym(x_axis), y=!! sym(y_axis), group=domain, fill=domain)) +
        geom_point(aes(color=domain)) +
        geom_smooth(method='loess',formula=y~x, size=0.5) +
        scale_fill_manual(values=domain_colors) +
        scale_color_manual(values=domain_colors) + 
        theme_classic() +
        scale_x_date(date_breaks=date_breaks_str) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle(paste0(site_list[[i]], ' : patient facts per domain')) +
        labs(x='Time Period', y='Median Facts Per Patient')
    }
    
    output_list[[i]] <- r
  }
  
  output_list
}


#' **Multi-Site, Anomaly, Over Time**

pf_ms_anom_at <- function(data_tbl,
                          domain_list,
                          outcome_var = 'grp_outlier_prop',
                          age_groups = FALSE,
                          codeset = FALSE,
                          visit_types = FALSE){
  
  domain_list <- domain_list %>% select(domain) %>% pull()
  
  if(age_groups){
    
    prep_fot <- check_fot_multisite(tblx = data_tbl,
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = 'age_grp')
    prep_mad <- produce_multisite_mad(multisite_tbl = prep_fot,
                                      facet_var = 'age_grp',
                                      mad_dev=2)
    r <- ggplot(prep_mad, aes(x=site, y=grp, fill=!!sym(outcome_var))) +
      geom_tile() +
      facet_wrap(~age_grp) +
      theme_classic() +
      coord_flip() +
      labs(fill = 'Proportion of Anomalous Measures',
           y = 'Domain',
           title = 'MAD Across Sites over Time: Multi-Site Anomaly Detection')
    
  }else if(codeset){
    prep_fot <- check_fot_multisite(tblx = data_tbl,
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = 'flag')
    prep_mad <- produce_multisite_mad(multisite_tbl = prep_fot,
                                      facet_var = 'flag',
                                      mad_dev=2)
    r <- ggplot(prep_mad, aes(x=site, y=grp, fill=!!sym(outcome_var))) +
      geom_tile() +
      facet_wrap(~flag) +
      theme_classic() +
      coord_flip() +
      labs(fill = 'Proportion of Anomalous Measures',
           y = 'Domain',
           title = 'MAD Across Sites over Time: Multi-Site Anomaly Detection')
  }else if(visit_types){
    prep_fot <- check_fot_multisite(tblx = data_tbl,
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = 'visit_type')
    prep_mad <- produce_multisite_mad(multisite_tbl = prep_fot,
                                      facet_var = 'visit_type',
                                      mad_dev=2)
    r <- ggplot(prep_mad, aes(x=site, y=grp, fill=!!sym(outcome_var))) +
      geom_tile() +
      facet_wrap(~visit_type) +
      theme_classic() +
      coord_flip() +
      labs(fill = 'Proportion of Anomalous Measures',
           y = 'Domain',
           title = 'MAD Across Sites over Time: Multi-Site Anomaly Detection')
  }else{
    prep_fot <- check_fot_multisite(tblx = data_tbl,
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = NULL)
    prep_mad <- produce_multisite_mad(multisite_tbl = prep_fot,
                                      facet_var = NULL,
                                      mad_dev=2)
    r <- ggplot(prep_mad, aes(x=site, y=grp, fill=!!sym(outcome_var))) +
      geom_tile() +
      theme_classic() +
      coord_flip() +
      labs(fill = 'Proportion of Anomalous Measures',
           y = 'Domain',
           title = 'MAD Across Sites over Time: Multi-Site Anomaly Detection')
  }
  return(r)
}


#' **Multi-Site, Exploratory, Across Time**

pf_ms_exp_at <- function(data_tbl,
                         domain_list,
                         time_span,
                         age_groups = FALSE,
                         codeset = FALSE,
                         visit_types = FALSE){
  
  domain_list <- domain_list %>% select(domain) %>% pull()
  
  if(age_groups){
    prep_fot <- check_fot_multisite(tblx = data_tbl,
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = 'age_grp')
    output <- create_multisite_exp(multisite_tbl = prep_fot,
                                   date_breaks_str = '1 year',
                                   time_span = time_span,
                                   facet_var = 'age_grp')
  }else if(codeset){
    prep_fot <- check_fot_multisite(tblx = data_tbl,
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = 'flag')
    output <- create_multisite_exp(multisite_tbl = prep_fot,
                                   date_breaks_str = '1 year',
                                   time_span = time_span,
                                   facet_var = 'flag')
  }else if(visit_types){
    prep_fot <- check_fot_multisite(tblx = data_tbl,
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = 'visit_type')
    output <- create_multisite_exp(multisite_tbl = prep_fot,
                                   date_breaks_str = '1 year',
                                   time_span = time_span,
                                   facet_var = 'visit_type')
  }else{
    prep_fot <- check_fot_multisite(tblx = data_tbl,
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = NULL)
    output <- create_multisite_exp(multisite_tbl = prep_fot,
                                   date_breaks_str = '1 year',
                                   time_span = time_span,
                                   facet_var = NULL)
  }
  
  return(output)
}


#' **Single Site, Anomaly Detection, No Time**

pf_ss_anom_nt <- function(data_tbl,
                          site_list,
                          age_groups = FALSE,
                          codeset = FALSE,
                          visit_types = FALSE){
  
  site_list <- site_list
  
  output_list <- list()
  
  for(i in 1:length(site_list)){
    
    data_filter <- data_tbl %>% filter(site == site_list[[i]])
    
    if(age_groups){
      r <- ggplot(data_filter %>% filter(visit_type == 'all') %>% 
                    select(site, age_group, var_name, prop_outlier_site_fact) %>% 
                    distinct(),
                  aes(x = prop_outlier_site_fact, y = var_name, fill = var_name)) +
        geom_col() +
        facet_wrap(~age_grp) +
        scale_fill_manual(values = domain_colors) +
        labs(title = paste0(site_list[[i]], ' Age Group Anomaly Detection: \nProportion of Patients +/- 2 SD away from Mean')) +
        theme_classic()
    }else if(codeset){
      r <- ggplot(data_filter %>% filter(visit_type == 'all') %>% 
                    select(site, flag, var_name, prop_outlier_site_fact) %>% 
                    distinct(),
                  aes(x = prop_outlier_site_fact, y = var_name, fill = var_name)) +
        geom_col() +
        facet_wrap(~flag) +
        scale_fill_manual(values = domain_colors) +
        labs(title = paste0(site_list[[i]], ' Codeset Anomaly Detection: \nProportion of Patients +/- 2 SD away from Mean')) +
        theme_classic()
    }else if(visit_types){
      r <- ggplot(data_filter %>% 
                  select(site, visit_type, var_name, prop_outlier_site_fact) %>% 
                  distinct(),
                  aes(x = prop_outlier_site_fact, y = var_name, fill = var_name)) +
        geom_col() +
        facet_wrap(~visit_type) +
        scale_fill_manual(values = domain_colors) +
        labs(title = paste0(site_list[[i]], ' Visit Type Anomaly Detection: \nProportion of Patients +/- 2 SD away from Mean')) +
        theme_classic()
    }else{
      r <- ggplot(data_filter %>% filter(visit_type == 'all') %>% 
                    select(site, var_name, prop_outlier_site_fact) %>% 
                    distinct(),
                  aes(x = prop_outlier_site_fact, y = var_name, fill = var_name)) +
        geom_col() +
        scale_fill_manual(values = domain_colors) +
        labs(title = paste0(site_list[[i]], ' Anomaly Detection: \nProportion of Patients +/- 2 SD away from Mean')) +
        theme_classic()
    }
    
    output_list[[i]] <- r
  }
  
  output_list
  
}


#' **Single Site, Exploratory, No Time**

pf_ss_exp_nt <- function(data_tbl,
                         site_list,
                         age_groups = FALSE,
                         codeset = FALSE,
                         visit_types = FALSE){
  
  pf_format <- data_tbl %>%
    mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
           var_name = paste0(var_name, '\n(N = ', n_w_fact, ')'))
  
  if(age_groups){
    
    output_prep <- create_list_input_facet_sepvarname(data_tbl= pf_format %>% filter(visit_type == 'all'),
                                                      facet_var_nm='age_grp',
                                                      site_name_list=site_list)
    
    output <- create_facet_graphs_byvarname(list_name=output_prep)
    
  }else if(codeset){
    
    output_prep <- create_list_input_facet_sepvarname(data_tbl= pf_format %>% filter(visit_type == 'all'),
                                                      facet_var_nm='flag',
                                                      site_name_list=site_list)
    
    output <- create_facet_graphs_byvarname(list_name=output_prep)
    
  }else if(visit_types){
    
    output_prep <- create_list_input_facet_sepvarname(data_tbl= pf_format,
                                                      facet_var_nm='visit_type',
                                                      site_name_list=site_list)
    
    output <- create_facet_graphs_byvarname(list_name=output_prep)
    
  }else{
    
    pf_format <- data_tbl %>%
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             var_name_lab = paste0(var_name, '\n(N = ', n_w_fact, ')'))
    
    output_prep <- create_list_input_sepsites(data_tbl= pf_format %>% filter(visit_type == 'all'),
                                              outcome_var = 'median_site_without0s')
    
    output <- create_sepsite_output(output_prep)
  }
  
  return(output)
}

#' **Multi-Site, Anomaly Detection, No Time**

pf_ms_anom_nt <- function(data_tbl,
                          age_groups = FALSE,
                          codeset = FALSE,
                          visit_types = FALSE){
  
  if(age_groups){
    output_prep <- prep_kmeans(dat = data_tbl %>% filter(visit_type == 'all' & age_grp != 'None'), 
                               facet_var = 'age_grp')
    
    output <- produce_kmeans_output(output_prep, centers = 2)
  }else if(codeset){
    output_prep <- prep_kmeans(dat = data_tbl %>% filter(visit_type == 'all'), 
                               facet_var = 'flag')
    
    output <- produce_kmeans_output(output_prep, centers = 2)
  }else if(visit_types){
    output_prep <- prep_kmeans(dat = data_tbl, 
                               facet_var = 'visit_type')
    
    output <- produce_kmeans_output(output_prep, centers = 2)
  }else{
    output_prep <- prep_kmeans(dat = data_tbl %>% filter(visit_type == 'all'), 
                               facet_var = NULL)
    
    output <- produce_kmeans_output(output_prep, centers = 2, facet = FALSE)
  }
  
  return(output)
}

#' **Multi-Site, Exploratory, No Time**

pf_ms_exp_nt <- function(data_tbl,
                         age_groups = FALSE,
                         codeset = FALSE,
                         visit_types = FALSE){
  
  if(age_groups){
    data_format <- data_tbl %>% filter(visit_type == 'all') %>%
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             site_lab = paste0(site, ' (N = ', n_w_fact, ')'))
    
    r <- ggplot(data_format, aes(x=var_name,y=median_site_without0s, colour=site))+
      geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
      geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
      facet_wrap(~age_grp, scales="free_x", ncol=2)+
      theme_bw()+
      labs(title = 'Median Facts per Patient Across Sites: \nAge Group Stratification') +
      coord_flip()
    
    g <- girafe(r)
  }else if(codeset){
    data_format <- data_tbl %>% filter(visit_type == 'all') %>%
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             site_lab = paste0(site, ' (N = ', n_w_fact, ')'))
    
    r <- ggplot(data_format, aes(x=var_name,y=median_site_without0s, colour=site))+
      geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
      geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
      facet_wrap(~flag, scales="free_x", ncol=2)+
      theme_bw()+
      labs(title = 'Median Facts per Patient Across Sites: \nCodeset Utilization') +
      coord_flip()
    
    g <- girafe(r)
  }else if(visit_types){
    data_format <- data_tbl %>% 
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             site_lab = paste0(site, ' (N = ', n_w_fact, ')'))
    
    r <- ggplot(data_format, aes(x=var_name,y=median_site_without0s, colour=site))+
      geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
      geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
      facet_wrap(~visit_type, scales="free_x", ncol=2)+
      theme_bw()+
      labs(title = 'Median Facts per Patient Across Sites: \nVisit Type Stratification') +
      coord_flip()
    
    g <- girafe(r)
  }else{
    data_format <- data_tbl %>% filter(visit_type == 'all') %>%
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             site_lab = paste0(site, ' (N = ', n_w_fact, ')'))
    
    r <- ggplot(data_format, aes(x=var_name,y=median_site_without0s, colour=site))+
      geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
      geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
      theme_bw()+
      labs(title = 'Median Facts per Patient Across Sites') +
      coord_flip()
    
    g <- girafe(r)
  }
  
  return(g)
}
