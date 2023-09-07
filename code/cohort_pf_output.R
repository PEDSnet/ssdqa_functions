
#' **Single Site, Anomaly, Over Time**
pf_ss_anom_at <- function(data_tbl,
                          site_list,
                          age_groups = FALSE,
                          codeset = FALSE,
                          visit_types = FALSE){
  
  site_list <- site_list
  
  output_list <- list()
  
  for(i in 1:length(site_list)){
    
    data_filter <- data_tbl %>% filter(site == site_list[[i]])
    
    if(age_groups){
      r <- plot_anomaly_diagnostics(.data=data_filter %>% filter(visit_type == 'all' & !is.na(age_grp) & fact_ct_denom > 5), 
                                    .facet_vars = c(domain, age_grp), 
                                    .date_var = start_date, 
                                    .value=median_fact_ct, 
                                    .alpha=0.10, 
                                    .legend_show = FALSE,
                                    .max_anomalies = 0.5, 
                                    .title = paste0(site_list[[i]], ' Median Facts Across Time: 
                               Anomaly Detection Across Sites'),
                                    .facet_ncol = 3, 
                                    .facet_scales = 'free_y', 
                                    .facet_collapse = TRUE, 
                                    .interactive = FALSE)
    }else if(codeset){
      r <- plot_anomaly_diagnostics(.data=data_filter %>% filter(visit_type == 'all' & fact_ct_denom > 5), 
                                    .facet_vars = c(domain, flag), 
                                    .date_var = start_date, 
                                    .value=median_fact_ct, 
                                    .alpha=0.10, 
                                    .legend_show = FALSE,
                                    .max_anomalies = 0.5, 
                                    .title = paste0(site_list[[i]], ' Median Facts Across Time: 
                               Anomaly Detection Across Sites'),
                                    .facet_ncol = 3, 
                                    .facet_scales = 'free', 
                                    .facet_collapse = TRUE, 
                                    .interactive = FALSE)
    }else if(visit_types){
      r <- plot_anomaly_diagnostics(.data=data_filter %>% filter(fact_ct_denom > 5), 
                                    .facet_vars = c(domain, visit_type), 
                                    .date_var = start_date, 
                                    .value=median_fact_ct, 
                                    .alpha=0.10, 
                                    .legend_show = FALSE,
                                    .max_anomalies = 0.5, 
                                    .title = paste0(site_list[[i]], ' Median Facts Across Time: 
                               Anomaly Detection Across Sites'),
                                    .facet_ncol = 3, 
                                    .facet_scales = 'free_y', 
                                    .facet_collapse = TRUE, 
                                    .interactive = FALSE)
    }else{
      r <- plot_anomaly_diagnostics(.data=data_filter %>% filter(visit_type == 'all' & fact_ct_denom > 5), 
                                    .facet_vars = c(domain), 
                                    .date_var = start_date, 
                                    .value=median_fact_ct, 
                                    .alpha=0.10, 
                                    .legend_show = FALSE,
                                    .max_anomalies = 0.5, 
                                    .title = paste0(site_list[[i]], ' Median Facts Across Time: 
                               Anomaly Detection Across Sites'),
                                    .facet_ncol = 3, 
                                    .facet_scales = 'free_y', 
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
                         domain_colors,
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
      r <- ggplot(data_filter %>% filter(visit_type == 'all' & !is.na(age_grp)), 
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
    
    prep_fot <- check_fot_multisite(tblx = data_tbl %>% filter(visit_type == 'all' & !is.na(age_grp)),
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
      labs(fill = 'Proportion of \nAnomalous Measures',
           y = 'Domain',
           title = 'MAD Across Sites over Time: Multi-Site Anomaly Detection')
    
  }else if(codeset){
    prep_fot <- check_fot_multisite(tblx = data_tbl %>% filter(visit_type == 'all'),
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
      labs(fill = 'Proportion of \nAnomalous Measures',
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
      labs(fill = 'Proportion of \nAnomalous Measures',
           y = 'Domain',
           title = 'MAD Across Sites over Time: Multi-Site Anomaly Detection')
  }else{
    prep_fot <- check_fot_multisite(tblx = data_tbl %>% filter(visit_type == 'all'),
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
      labs(fill = 'Proportion of \nAnomalous Measures',
           y = 'Domain',
           title = 'MAD Across Sites over Time: Multi-Site Anomaly Detection')
  }
  return(r)
}


#' **Multi-Site, Exploratory, Across Time**

pf_ms_exp_at <- function(data_tbl,
                         domain_list,
                         site_colors,
                         time_span,
                         age_groups = FALSE,
                         codeset = FALSE,
                         visit_types = FALSE){
  
  domain_list <- domain_list %>% select(domain) %>% pull()
  
  if(age_groups){
    prep_fot <- check_fot_multisite(tblx = data_tbl %>% filter(visit_type == 'all' & !is.na(age_grp)),
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = 'age_grp')
    output <- create_multisite_exp(multisite_tbl = prep_fot,
                                   date_breaks_str = '1 year',
                                   time_span = time_span,
                                   site_colors_v = site_colors,
                                   facet_var = 'age_grp')
  }else if(codeset){
    prep_fot <- check_fot_multisite(tblx = data_tbl %>% filter(visit_type == 'all'),
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = 'flag')
    output <- create_multisite_exp(multisite_tbl = prep_fot,
                                   date_breaks_str = '1 year',
                                   time_span = time_span,
                                   site_colors_v = site_colors,
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
                                   site_colors_v = site_colors,
                                   facet_var = 'visit_type')
  }else{
    prep_fot <- check_fot_multisite(tblx = data_tbl %>% filter(visit_type == 'all'),
                                    target_col = 'median_fact_ct',
                                    site_col = 'site',
                                    time_col = 'start_date',
                                    domain_list = domain_list,
                                    facet_var = NULL)
    output <- create_multisite_exp(multisite_tbl = prep_fot,
                                   date_breaks_str = '1 year',
                                   time_span = time_span,
                                   site_colors_v = site_colors,
                                   facet_var = NULL)
  }
  
  return(output)
}


#' **Single Site, Anomaly Detection, No Time**

pf_ss_anom_nt <- function(data_tbl,
                          site_list,
                          domain_colors,
                          age_groups = FALSE,
                          codeset = FALSE,
                          visit_types = FALSE){
  
  site_list <- site_list
  
  output_list <- list()
  
  for(i in 1:length(site_list)){
    
    data_filter <- data_tbl %>% filter(site == site_list[[i]])
    
    if(age_groups){
      r <- ggplot(data_filter %>% filter(visit_type == 'all' & age_grp != 'None') %>% 
                    select(site, age_grp, var_name, prop_outlier_site_fact) %>% 
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
                         site_colors,
                         age_groups = FALSE,
                         codeset = FALSE,
                         visit_types = FALSE){
  
  pf_format <- data_tbl %>%
    mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
           var_name = paste0(var_name, '\n(N = # of patients)'))
  
  if(age_groups){
    
    output_prep <- create_list_input_facet_sepvarname(data_tbl= pf_format %>% filter(visit_type == 'all' & age_grp != 'None'),
                                                      facet_var_nm='age_grp',
                                                      site_colors = site_colors,
                                                      site_name_list=site_list)
    
    output <- create_facet_graphs_byvarname(list_name=output_prep)
    
  }else if(codeset){
    
    output_prep <- create_list_input_facet_sepvarname(data_tbl= pf_format %>% filter(visit_type == 'all'),
                                                      facet_var_nm='flag',
                                                      site_colors = site_colors,
                                                      site_name_list=site_list)
    
    output <- create_facet_graphs_byvarname(list_name=output_prep)
    
  }else if(visit_types){
    
    output_prep <- create_list_input_facet_sepvarname(data_tbl= pf_format,
                                                      facet_var_nm='visit_type',
                                                      site_colors = site_colors,
                                                      site_name_list=site_list)
    
    output <- create_facet_graphs_byvarname(list_name=output_prep)
    
  }else{
    
    pf_format <- data_tbl %>%
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             var_name_lab = paste0(var_name, '\n(N = # of patients)'))
    
    
    grps <- c('var_name')
    
    if('age_grp' %in% pf_output) {grps <- append(grps, 'age_grp')}
    if('flag' %in% pf_output) {grps <- append(grps, 'flag')}
    
    output_prep <- create_list_input_sepsites(data_tbl= pf_format,
                                              site_colors = site_colors,
                                              outcome_var = 'median_site_without0s')
    
    output <- create_sepsite_output(output_prep)
  }
  
  return(output)
}



#' **Single-Site, Exploratory, No Time**
#' 
#' This chart will produce output for each domain. The data frame 
#' that is listed as a parameter of the function should contain a single 
#' output for each domain. User can facet by site, age category, 
#' or other stratifications. Multiple graphs can also be produced (e.g., 
#' one graph faceting by site, or creaeting separate output for each site)
#' 
#' @param data_tbl output from previous function; 
#' requires input for one site, one visit type; can create multiple graphs
#' for different grouped variables
#' @param output desired output - have 3 options:
#' 1) `median_site_with0s`: specific site median, including patients with no evidence of patient fact
#' (e.g., if domain = labs, includes in the median all patients with and without any labs)
#' 2) `median_site_without0s`: specific site median, not including patients without evidence of patient fact
#' (e.g., if domain = labs, only includes median for patients with evidence of a lab)
#' 3) `prop_all_w_fact`: proportion of patients with the patient fact (e.g., proportion of patients with lab)
#' @param facet variables to facet (e.g., `var_name`); vector of strings
#' 
#' 

pf_ss_exp_nt <- function(data_tbl,
                         output,
                         facet) {
  
  if(output=='median_site_with0s') {y_title='Median for All Patients'}
  if(output=='median_site_without0s') {y_title='Median for Patients with Fact'}
  if(output=='prop_all_w_fact') {y_title='Proportion of Patients with Fact'}
  
  domain_deframe <- 
    data_tbl %>% rename(domain=var_name) %>% distinct(domain) %>% 
    inner_join(read_codeset('domain_color_config','cc')) %>% 
    deframe()
  
  var_name_setup <- 
    data_tbl %>% rename(domain=var_name)
  
  ggplot(var_name_setup,
         aes(x=domain, y=!! sym(output), fill=domain)) +
    geom_bar(stat='identity') + 
    facet_wrap((facet)) + 
    labs(y=y_title,
         x='Domain') +
    scale_fill_manual(values=domain_deframe) +
    coord_flip() 
  
  
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
    data_format <- data_tbl %>% filter(visit_type == 'all' & age_grp != 'None') %>%
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             site_lab = paste0(site, ' (N = # of patients)'))
    
    r <- ggplot(data_format, aes(x=var_name,y=median_site_without0s, colour=site))+
      geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
      geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
      facet_wrap(~age_grp, scales="free_x", ncol=2)+
      theme_bw()+
      labs(title = 'Median Facts per Patient Across Sites: \nAge Group Stratification') +
      coord_flip()
    
    g <- girafe(ggobj = r)
  }else if(codeset){
    data_format <- data_tbl %>% filter(visit_type == 'all') %>%
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             site_lab = paste0(site, ' (N = # of patients)'))
    
    r <- ggplot(data_format, aes(x=var_name,y=median_site_without0s, colour=site))+
      geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
      geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
      facet_wrap(~flag, scales="free_x", ncol=2)+
      theme_bw()+
      labs(title = 'Median Facts per Patient Across Sites: \nCodeset Utilization') +
      coord_flip()
    
    g <- girafe(ggobj = r)
  }else if(visit_types){
    data_format <- data_tbl %>% 
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             site_lab = paste0(site, ' (N = # of patients)'))
    
    r <- ggplot(data_format, aes(x=var_name,y=median_site_without0s, colour=site))+
      geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
      geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
      facet_wrap(~visit_type, scales="free_x", ncol=2)+
      theme_bw()+
      labs(title = 'Median Facts per Patient Across Sites: \nVisit Type Stratification') +
      coord_flip()
    
    g <- girafe(ggobj = r)
  }else{
    data_format <- data_tbl %>% filter(visit_type == 'all') %>%
      mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
             site_lab = paste0(site, ' (N = # of patients)'))
    
    r <- ggplot(data_format, aes(x=var_name,y=median_site_without0s, colour=site))+
      geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
      geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
      theme_bw()+
      labs(title = 'Median Facts per Patient Across Sites') +
      coord_flip()
    
    g <- girafe(ggobj = r)
  }
  
  return(g)
}
