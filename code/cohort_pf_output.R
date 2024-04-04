
#### NEED TO GO BACK AND REFINE DOCUMENTATION ####


#' **Single Site, Anomaly, Over Time**
#' 
#' This chart will produce output for each domain. The data frame 
#' that is listed as a parameter of the function should minimally contain a single 
#' output for each domain. User can facet by site, age category, 
#' or other stratifications. Multiple graphs can also be produced (e.g., 
#' one graph faceting by site, or creating separate output for each site)
#' 
#' @param data_tbl output from previous function; 
#' requires input for one site; can create multiple graphs
#' for different grouped variables
#' 
#' @param output desired output - have 2 options:
#' 1) `median_fact_ct`: the median number of facts for each domain during the specified time period
#' 2) `sum_fact_ct`: the sum of facts for each domain during the specified time period
#' 
#' @param facet variables to facet (e.g., `domain`); vector of strings

pf_ss_anom_at <- function(data_tbl,
                          output,
                          facet,
                          time_span){
  
  if(output=='median_fact_ct'){
    title='Median Facts Across Time'
  }else if(output=='sum_fact_ct'){
    title='Sum of Facts Across Time'
  }else(stop('Please select a valid output - `median_fact_ct` or `sum_fact_ct`'))
  # if(output=='fact_ct_denom') {title='What does this represent'}
  
  facet <- facet %>% append('domain')
  
  data_tbl_pad <- data_tbl %>% group_by(across(all_of(facet))) %>%
    pad_by_time(.date_var = start_date,
                .by = 'year',
                .start_date = time_span[1],
                .end_date = time_span[2],
                .pad_value = 0)
  
  plot_anomaly_diagnostics(.data=data_tbl_pad,
                           .facet_vars = facet, 
                           .date_var = start_date, 
                           .value=!! sym(output), 
                           .alpha=0.10, 
                           .legend_show = FALSE,
                           .max_anomalies = 0.5, 
                           .title = title,
                           .facet_ncol = 3, 
                           .facet_scales = 'free_y', 
                           .facet_collapse = TRUE, 
                           .interactive = FALSE)
}

#' **Single Site, Exploratory, Over Time**
#' 
#' This chart will produce output for each domain. The data frame 
#' that is listed as a parameter of the function should minimally contain a single 
#' output for each domain. User can facet by site, age category, 
#' or other stratifications. Multiple graphs can also be produced (e.g., 
#' one graph faceting by site, or creating separate output for each site)
#' 
#' @param data_tbl output from previous function; 
#' requires input for one site; can create multiple graphs
#' for different grouped variables
#' 
#' @param output desired output - have 2 options:
#' 1) `median_fact_ct`: the median number of facts for each domain during the specified time period
#' 2) `sum_fact_ct`: the sum of facts for each domain during the specified time period
#' 
#' @param facet variables to facet (e.g., `domain`); vector of strings
#' 
#' @param date_breaks_str string to denote how time should be broken up in the chart (e.g. year, month)

pf_ss_exp_at <- function(data_tbl,
                         output,
                         facet,
                         date_breaks_str = '1 year'){
  
  if(output=='median_fact_ct'){
    y_title='Median Fact Count'
  }else if(output=='sum_fact_ct'){
      y_title='Sum of Facts'
  }else(stop('Please select a valid output - `median_fact_ct` or `sum_fact_ct`'))
  # if(output=='fact_ct_denom') {y_title='What does this represent'}
  
  domain_deframe <- 
    data_tbl %>% distinct(domain) %>% 
    inner_join(read_codeset('domain_color_config','cc')) %>% 
    deframe()
  
  p <- ggplot(data_tbl, aes(x=start_date, y=!! sym(output), group=domain, fill=domain)) +
    geom_point(aes(color=domain)) +
    geom_smooth(method='loess',formula=y~x, size=0.5) +
    facet_wrap((facet), scales = 'free_y') +
    scale_fill_manual(values=domain_deframe) +
    scale_color_manual(values=domain_deframe) + 
    theme_classic() +
    scale_x_date(date_breaks=date_breaks_str) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x='Time Period', 
         y=y_title)
  
  ggplotly(p)
}

#' **Multi-Site, Anomaly, Over Time**
#' 
#' Proportion of patients with fact & Euclidean distance summary
#' 
#' @param process_output output from previous function; 
#' requires input for multiple sites; can create multiple graphs
#' for different grouped variables
#' 
#' @param domain_filter one of the user provided domains in the process_output table to be used
#'                      to filter down the output
#' @param visit_filter one of the user provided visit types in the process_output table to be used
#'                     to filter down the output                    
#' 
pf_ms_anom_at <- function(process_output,
                          domain_filter,
                          visit_filter) {
  
  
  filt_op <- process_output %>% filter(visit_type == visit_filter,
                                       domain == domain_filter)
  
  allsites <- 
    filt_op %>% 
    select(time_start,visit_type,domain,mean_allsiteprop) %>% distinct() %>% 
    rename(prop=mean_allsiteprop) %>% 
    mutate(site='all site average') %>% 
    mutate(text_smooth=paste0("Site: ", site,
                              #"\n","Proportion: ",prop_concept,
                              "\n","Proportion Patients with Fact: ",prop),
           text_raw=paste0("Site: ", site,
                           #"\n","Proportion: ",prop_concept,
                           "\n","Proportion Patients with Fact: ",prop)) 
  
  dat_to_plot <- 
    filt_op %>% 
    mutate(text_smooth=paste0("Site: ", site,
                              #"\n","Site Proportion: ",prop_concept,
                              #"\n","Proportion: ",prop_concept,
                              #"\n","Site Smoothed Proportion: ",site_loess,
                              #"\n","All-Site Mean: ",mean_allsiteprop,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop,
                           #"\n","Proportion: ",prop_concept,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           #"\n","All-Site Mean: ",mean_allsiteprop,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean)) 
  
  p <- dat_to_plot %>%
    ggplot(aes(y = prop, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(y = 'Proportion \n(Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion Patients with ', visit_filter, ' ', domain_filter, ' Across Time'))
  
  q <- dat_to_plot %>%
    ggplot(aes(y = prop, x = time_start, color = site,
               group=site, text=text_raw)) +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion Patients with ', visit_filter, ' ', domain_filter, ' Across Time'))
  
  t <- dat_to_plot %>% 
    distinct(site, dist_eucl_mean, site_loess) %>% 
    group_by(site, dist_eucl_mean) %>% 
    summarise(mean_site_loess = mean(site_loess)) %>%
    mutate(tcol = ifelse(mean_site_loess >= 0.8 | mean_site_loess <= 0.2, 'group1', 'group2')) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess)) + 
    geom_col() + 
    geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
              show.legend = FALSE, color = 'white') +
    coord_radial(r_axis_inside = FALSE, rotate_angle = TRUE) + 
    guides(theta = guide_axis_theta(angle = 0)) +
    #scale_y_continuous(limits = c(-1,ylim_max)) + 
    theme_minimal() + 
    scale_fill_viridis_c(option = 'turbo', limits = c(0, 1), oob = scales::squish) +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(face = 'bold')) + 
    labs(fill = 'Avg. Proportion \n(Loess)', 
         y ='Euclidean Distance', 
         x = '', 
         title = paste0('Euclidean Distance for ', visit_filter, ' ', domain_filter))
  
  plotly_p <- ggplotly(p,tooltip="text")
  plotly_q <- ggplotly(q,tooltip="text")
  
  output <- list(plotly_p,
                 plotly_q,
                 t)
  
  return(output)
  
}

#' **Multi-Site, Exploratory, Across Time**
#' 
#' This chart will produce output for each site & domain. The data frame 
#' that is listed as a parameter of the function should minimally contain a single 
#' output for each site & domain combination. User can facet by visit type, age category, 
#' or other stratifications. Multiple graphs can also be produced with varying facets.
#' 
#' @param data_tbl output from previous function; 
#' requires input for multiple sites; can create multiple graphs
#' for different grouped variables
#' 
#' @param output desired output - have 2 options:
#' 1) `median_fact_ct`: the median number of facts for each domain during the specified time period
#' 2) `sum_fact_ct`: the sum of facts for each domain during the specified time period
#' 
#' @param facet variables to facet (e.g., `domain`); vector of strings
#' 
#' @param time_span the desired time span to be examined in the output; can be the same as the 
#'                  previous function or can be changed to a subset of the time_span from the
#'                  previous function

pf_ms_exp_at <- function(data_tbl,
                         time_span,
                         output,
                         facet){
  
  if(output=='median_fact_ct'){
    title <- 'Median Fact Count Across Time'
  }else if(output=='sum_fact_ct'){
    title <- 'Total Fact Count Across Time'
  }else(stop('Please select a valid output - `median_fact_ct` or `sum_fact_ct`'))
  
  facet <- facet %>% append('domain') %>% unique()
  
  p <- data_tbl %>%
    filter(start_date >= time_span[1] &
             end_date <= time_span[2]) %>%
    ggplot(aes(x = start_date, y = !!sym(output), fill = site, color = site)) +
    geom_line() +
    facet_wrap((facet)) +
    labs(title = title,
         x = 'Time')
  
  ggplotly(p)
}

#' **Single Site, Anomaly Detection, No Time**
#' 
#' This chart will produce output for each domain. The data frame 
#' that is listed as a parameter of the function should minimally contain a single 
#' output for each domain. User can facet by site, age category, 
#' or other stratifications. Multiple graphs can also be produced (e.g., 
#' one graph faceting by site, or creating separate output for each site)
#' 
#' @param data_tbl output from previous function; 
#' requires input for one site; can create multiple graphs
#' for different grouped variables
#' 
#' @param output desired output - have 4 options:
#' 1) `outlier_fact`: the number of facts overall (i.e. not grouped by site) that fall 2 SD away from the mean
#' 2) `prop_outlier_fact`: the proportion of facts overall that fall 2 SD away from the mean
#' 3) `outlier_site_fact`: the number of facts per site that fall 2 SD away from the mean
#' 4) `prop_outlier_site_fact`: the proportion of facts per site that fall 2 SD away from the mean
#' 
#' @param facet variables to facet (e.g., `domain`); vector of strings

pf_ss_anom_nt <- function(data_tbl,
                          output,
                          facet=c('domain')){
  
  if(output=='outlier_fact'){
    y_title = 'Number of Overall Patients +/- 2 SD Away from Mean'
  }else if(output=='prop_outlier_fact'){
    y_title = 'Proportion of Overall Patients +/- 2 SD Away from Mean'
  }else if(output=='outlier_site_fact'){
    y_title = 'Number of Site Patients +/- 2 SD Away from Mean'
  }else if(output=='prop_outlier_site_fact'){
      y_title = 'Proportion of Site Patients +/- 2 SD Away from Mean'
  }else(stop('Please select a valid output - `outlier_fact`, `prop_outlier_fact`, `outlier_site_fact`, or 
             `prop_outlier_site_fact`'))
  
  domain_deframe <- 
    data_tbl %>% rename(domain=domain) %>% distinct(domain) %>% 
    inner_join(read_codeset('domain_color_config','cc')) %>% 
    deframe()
  
  ggplot(data_tbl,
         aes(x = !!sym(output), y = domain, fill = domain)) +
    geom_col() +
    facet_wrap((facet)) +
    scale_fill_manual(values = domain_deframe) +
    labs(title = y_title,
         y = 'Domain') +
    theme_classic() +
    theme(panel.grid.major = element_line(size=0.4, linetype = 'solid'),
          panel.grid.minor = element_line(size=0.2, linetype = 'dashed'))
  
}


#' **Single-Site, Exploratory, No Time**
#' 
#' This chart will produce output for each domain. The data frame 
#' that is listed as a parameter of the function should minimally contain a single 
#' output for each domain. User can facet by site, age category, 
#' or other stratifications. Multiple graphs can also be produced (e.g., 
#' one graph faceting by site, or creaeting separate output for each site)
#' 
#' @param data_tbl output from previous function; 
#' requires input for one site; can create multiple graphs
#' for different grouped variables
#' 
#' @param output desired output - have 2 options:
#' 1) `median_site_with0s`: specific site median, including patients with no evidence of patient fact
#' (e.g., if domain = labs, includes in the median all patients with and without any labs)
#' 2) `median_site_without0s`: specific site median, not including patients without evidence of patient fact
#' (e.g., if domain = labs, only includes median for patients with evidence of a lab)
#' 
#' @param facet variables to facet (e.g., `domain`); vector of strings
#' 
#' 

pf_ss_exp_nt <- function(data_tbl,
                         output,
                         facet=c('domain')) {
  
  if(output=='median_site_with0s'){
    y_title='Median for All Patients'
  }else if(output=='median_site_without0s'){
      y_title='Median for Patients with Fact'
  }else(stop('Please select a valid output - `median_site_with0s` or `median_site_without0s`'))
  
  domain_deframe <- 
    data_tbl %>% distinct(domain) %>% 
    inner_join(read_codeset('domain_color_config','cc')) %>% 
    deframe()
  
  # domain_setup <- 
  #   data_tbl %>% rename(domain=domain)
  
  ggplot(data_tbl,
         aes(x=domain, y=!! sym(output), fill=domain)) +
    geom_bar(stat='identity') + 
    facet_wrap((facet)) + 
    labs(y=y_title,
         x='Domain') +
    scale_fill_manual(values=domain_deframe) +
    coord_flip() +
    theme_classic() +
    theme(panel.grid.major = element_line(size=0.4, linetype = 'solid'),
          panel.grid.minor = element_line(size=0.2, linetype = 'dashed'))
  
  
}



#' **Multi-Site, Anomaly Detection, No Time**
#' 
#' This chart will produce a cluster analysis based on each site & domain. The data frame 
#' that is listed as a parameter of the function should minimally contain a single 
#' output for each domain and site combination. User can facet by visit type, age category, 
#' or other stratifications. Multiple graphs can also be produced with varying facets.
#' 
#' @param data_tbl output from previous function; 
#' requires input for multiple sites; can create multiple graphs
#' for different grouped variables
#' 
#' @param output desired output - have 2 options:
#' 1) `median_site_with0s`: specific site median, including patients with no evidence of patient fact
#' (e.g., if domain = labs, includes in the median all patients with and without any labs)
#' 2) `median_site_without0s`: specific site median, not including patients without evidence of patient fact
#' (e.g., if domain = labs, only includes median for patients with evidence of a lab)
#' 
#' @param facet variables to facet (e.g., `domain`); vector of strings
#' 
#' 

pf_ms_anom_nt <- function(data_tbl,
                          output,
                          facet,
                          kmeans_clusters){
  
  if(output=='median_site_with0s'){
    pass <- TRUE
  }else if(output=='median_site_without0s'){
    pass <- TRUE
  }else(stop('Please select a valid output - `median_site_with0s` or `median_site_without0s`'))
  
  output_prep <- prep_kmeans(dat = data_tbl, 
                             output = output,
                             facet_vars = facet)
  
  output <- produce_kmeans_output(kmeans_list = output_prep,
                                  centers = kmeans_clusters)
  
}

#' #' **Multi-Site, Exploratory, No Time**
#' 
#' This chart will produce output for each site & domain. The data frame 
#' that is listed as a parameter of the function should minimally contain a single 
#' output for each domain and site combination. User can facet by visit type, age category, 
#' or other stratifications. Multiple graphs can also be produced with varying facets.
#' 
#' @param data_tbl output from previous function; 
#' requires input for multiple sites; can create multiple graphs
#' for different grouped variables
#' 
#' @param output desired output - have 2 options:
#' 1) `median_site_with0s`: specific site median, including patients with no evidence of patient fact
#' (e.g., if domain = labs, includes in the median all patients with and without any labs)
#' 2) `median_site_without0s`: specific site median, not including patients without evidence of patient fact
#' (e.g., if domain = labs, only includes median for patients with evidence of a lab)
#' 
#' @param facet variables to facet (e.g., `domain`); vector of strings
#' 

pf_ms_exp_nt <- function(data_tbl,
                         output,
                         facet){
  
  if(output=='median_site_with0s'){
    y_title='Median for All Patients Across Sites'
    comp_var = 'median_all_with0s'
  }else if(output=='median_site_without0s'){
      y_title='Median for Patients with Fact Across Sites'
      comp_var = 'median_all_without0s'
  }else(stop('Please select a valid output - `median_site_with0s` or `median_site_without0s`'))
  
  site_deframe <- 
    data_tbl %>% distinct(site) %>% 
    inner_join(read_codeset('site_color_config','cc')) %>% 
    deframe()
  
  data_format <- data_tbl %>%
    mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
           site_lab = paste0(site, ' (N = ', n_w_fact, ')'))
  
  r <- ggplot(data_format, 
              aes(x=domain,y=!! sym(output), colour=site))+
    geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
    geom_point(aes(x=domain, y=!! sym(comp_var)), shape=8, size=3, color="black")+
    scale_color_manual(values=site_deframe)+
    facet_wrap((facet), scales="free_x", ncol=2)+
    theme_bw()+
    labs(y = y_title,
         x = 'Domain') +
    coord_flip()
  
  girafe(ggobj = r)
}
