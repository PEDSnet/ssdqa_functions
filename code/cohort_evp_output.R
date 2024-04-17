

#' *Single Site, Exporatory, No Time*
#'
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#' @param facet columns the user would like to facet by
#'
#' @return a bar graph displaying the proportion of patients/rows that meet criteria for each
#'         of the variables found in process_output
#' 
evp_ss_exp_nt <- function(process_output,
                          output_level,
                          facet){
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  process_output %>%
    ggplot(aes(y = variable, x = !!sym(prop), fill = variable)) +
    geom_col(show.legend = FALSE) +
    facet_wrap((facet)) +
    scale_fill_brewer(palette = 'Set2') +
    theme_minimal() +
    labs(x = paste0('Proportion ', title),
         y = 'Variable',
         title = paste0('Proportion of ', title, ' per Variable'))
  
  
}

#' * Multi Site, Exploratory, No Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#' @param facet columns the user would like to facet by
#'
#' @return a heat map displaying the proportion of patients/rows that meet criteria for each
#'         of the variables found in process_output at each of site
#' 

evp_ms_exp_nt <- function(process_output,
                          output_level,
                          facet){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(cli::cli_abort('Please choose an acceptable output level: {.code patient} or {.code row}'))
  
  process_output %>%
    mutate(colors = ifelse(!!sym(prop) < 0.2 | !!sym(prop) > 0.8, 'group1', 'group2')) %>%
    ggplot(aes(y = site, x = variable, fill = !!sym(prop))) +
    geom_tile() +
    geom_text(aes(label = !!sym(prop), color = colors), #size = 2, 
              show.legend = FALSE) +
    scale_color_manual(values = c('white', 'black')) +
    scale_fill_viridis_c(option = 'turbo') +
    theme_minimal() +
    labs(title = paste0('Proportion ', title, ' per Variable & Site'),
         y = 'Site',
         x = 'Variable', 
         fill = paste0('Proportion ', title))
  
}

#' * Single Site, Anomaly, No Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param facet columns the user would like to facet by
#'
#' @return a heat map displaying the Jaccard similarity index between each of the variables
#'         any variables without a relationship and any self-to-self relationships are dropped
#'         
#'         if the user hovers over the heatmap, the co-occurrence count, jaccard score for the pair,
#'         mean jaccard score for the variable, and variables will show.
#' 
evp_ss_anom_nt <- function(process_output,
                           facet){
  
  plot <- process_output %>%
    ggplot(aes(x = as.character(concept1), y = as.character(concept2), 
               fill = jaccard_index)) + 
    geom_tile_interactive(aes(tooltip = paste0('concept1 = ',concept1, '; n= ',concept1_ct,'\n','concept2 = ',concept2,'; n= ',concept2_ct,
                                               '\n', 'co-occurrence = ', cocount,
                                               '\n','jaccard sim = ',jaccard_index
                                               #'\n', 'mean = ',var_jaccard_mean,'\n','sd = ', var_jaccard_sd
                                               ))) + 
    scale_fill_viridis_c(option = 'turbo') + 
    facet_wrap((facet)) +
    labs(title = 'Co-Occurrence of Variables',
         x = 'variable1',
         y = 'variable2') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) 
  
  p <- girafe(ggobj=plot,
              width=10,
              height=10)
}

#' * Multi Site, Anomaly, No Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#' @param kmeans_centers the number of centers that should be used in the k-means computations
#'                       defaults to `2`
#' @param facet columns the user would like to facet by
#'
#' @return one cluster graph per facet grouping with sites comprising the cluster elements
#'         if facet = NULL, one cluster graph will be output
#' 
evp_ms_anom_nt <- function(process_output,
                           output_level,
                           kmeans_centers = 2, 
                           facet){
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  process_output_prep <- process_output %>%
    mutate(domain = variable)
  
  kmeans_prep <- prep_kmeans(dat = process_output_prep,
                             output = prop,
                             facet_vars = facet)
  
  kmeans_output <- produce_kmeans_output(kmeans_list = kmeans_prep,
                                         centers = kmeans_centers)
  
  return(kmeans_output)
}

#' * Single Site, Exploratory, Across Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#' @param facet columns the user would like to facet by
#'
#' @return a line graph displaying the proportion of patients/rows for each variable
#'         over the user-specified time period. On hover, the user can see the exact
#'         proportion for that variable and time point
#' 
evp_ss_exp_at <- function(process_output,
                          output_level,
                          facet){
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  p <- process_output %>%
    ggplot(aes(y = !!sym(prop), x = time_start, color = variable)) +
    geom_line() +
    scale_color_brewer(palette = 'Set2') +
    facet_wrap((facet)) +
    theme_minimal() +
    labs(title = paste0('Proportion ', title, ' Over Time'),
         color = 'Variable', 
         y = paste0('Proportion ', title),
         x = 'Time')
  
  plot <- ggplotly(p)
  
  
}


#' * Multi Site, Exploratory, Across Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#' @param facet columns the user would like to facet by
#'
#' @return a line graph displaying the proportion of patients/rows for each variable
#'         & site over the user-specified time period. On hover, the user can see the exact
#'         proportion for that site, variable, and time point
#' 
evp_ms_exp_at <- function(process_output,
                          output_level,
                          facet){
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  facet <- facet %>% append('variable') %>% unique()
  
  p <- process_output %>%
    ggplot(aes(y = !!sym(prop), x = time_start, color = site)) +
    geom_line() +
    scale_color_brewer(palette = 'Set2') +
    facet_wrap((facet)) +
    theme_minimal() +
    labs(title = paste0('Proportion ', title, ' Over Time'),
         color = 'Site', 
         y = paste0('Proportion ', title),
         x = 'Time')
  
  plot <- ggplotly(p)
  
  
}

#' Find anomalies for smaller time frames
#'
#' @param ss_input_tbl output of compute_at_cross_join where the input table uses
#'                     a time increment smaller than a year
#' @param filter_concept the concept id of interest for which the plot should be generated
#'
#' @return two plots - one with a time series with outliers highlighted in red dots, and
#'         another with 4 time series visualizing anomaly decomposition
#' 
evp_small_time_anom <- function(ss_input_tbl,
                                filter_variable,
                                val_col) {
  
  plt_tbl <- ss_input_tbl %>% filter(variable == filter_variable) %>%
    rename('prop' = !!val_col)
  
  anomalize_tbl <- 
    anomalize(plt_tbl,.date_var=time_start, 
              .value=prop)
  
  anomalies <- 
    plot_anomalies(.data=anomalize_tbl,
                   .date_var=time_start) %>% 
    layout(title = paste0('Anomalies for Variable ', filter_variable))
  
  decomp <- 
    plot_anomalies_decomp(.data=anomalize_tbl,
                          .date_var=time_start) %>% 
    layout(title = paste0('Anomalies for Variable ', filter_variable))
  
  final <- list(anomalies, decomp)
} 

#' * Single Site, Anomaly, Across Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#' @param facet columns the user would like to facet by
#'
#' @return a control chart displaying the proportion of patients/rows over the user
#'         specified time period; any orange dots along the line indicate an 
#'         anomalous data point
#'         
evp_ss_anom_at <- function(process_output,
                           output_level,
                           filter_variable,
                           facet){
  
  if(output_level == 'row'){
    ct <- 'variable_row_ct'
    denom <- 'total_row_ct'
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    ct <- 'variable_pt_ct'
    denom <- 'total_pt_ct'
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  time_inc <- process_output %>% distinct(time_increment) %>% pull()
  
  if(time_inc == 'year'){
  
  facet <- facet %>% append('variable') %>% unique()
  
  final <- process_output %>%
    unite(facet_col, !!!syms(facet), sep = '\n') %>%
    rename('ycol' = ct,
           'denom' = denom)
  
 pp_qi <-  qic(data = final, x = time_start, y = ycol, chart = 'pp', facet = ~facet_col,
      title = paste0('Control Chart: Proportion of ', title, ' per Variable'), 
      ylab = 'Proportion', xlab = 'Time',
      show.grid = TRUE, n = denom)
 
 op_dat <- pp_qi$data
 
 new_pp <- ggplot(op_dat,aes(x,y)) +
   geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "gray",alpha = 0.4) +
   geom_line(colour = "black", size = .5) + 
   geom_line(aes(x,cl)) +
   geom_point(colour = "black" , fill = "black", size = 1) +
   geom_point(data = subset(op_dat, y >= ucl), color = "red", size = 2) +
   geom_point(data = subset(op_dat, y <= lcl), color = "red", size = 2) +
   facet_wrap(~facet1) +
   ggtitle(label = paste0('Control Chart: Proportion of ', title, ' per Variable')) +
   labs(x = 'Time',
        y = 'Proportion')+
   theme_minimal()
 
 output <- ggplotly(new_pp)
  
  }else{
    
    anomalies <- 
      plot_anomalies(.data=process_output,
                     .date_var=time_start) %>% 
      layout(title = paste0('Anomalies for Variable ', filter_variable))
    
    decomp <- 
      plot_anomalies_decomp(.data=process_output,
                            .date_var=time_start) %>% 
      layout(title = paste0('Anomalies for Variable ', filter_variable))
    
    output <- list(anomalies, decomp)
    
  }
  
  return(output)
  
}


#' **Multi-Site Across Time Anomaly**
#' Produces graphs showing Euclidean Distanctes
#' 
#' @param process_output output from `evp_process`
#' @param filter_variable the variable that should be used to generate output
#' @return two graphs:
#'    1) line graph that shows the smoothed proportion of a 
#'    code across time computation with the Euclidean distance associated with each line
#'    2) line graph that shows the raw proportion of a 
#'    code across time computation with the Euclidean distance associated with each line
#'    3) a bar graph with the Euclidean distance value for each site, with the average
#'    proportion as the fill
#' 
#' THIS GRAPH SHOWS ONLY ONE VARIABLE AT A TIME!
#' 

evp_ms_anom_at <- function(process_output,
                           output_level,
                           filter_variable) {
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  filt_op <- process_output %>% filter(variable == filter_variable) %>%
    mutate(prop_col = !!sym(prop))
  
  allsites <- 
    filt_op %>% 
    select(time_start,variable,mean_allsiteprop) %>% distinct() %>% 
    rename(prop_col=mean_allsiteprop) %>% 
    mutate(site='all site average') %>% 
    mutate(text_smooth=paste0("Site: ", site,
                              #"\n","Proportion: ",prop_concept,
                              "\n","Proportion: ",prop_col),
           text_raw=paste0("Site: ", site,
                           #"\n","Proportion: ",prop_concept,
                           "\n","Proportion: ",prop_col)) 
  
  dat_to_plot <- 
    filt_op %>% 
    mutate(text_smooth=paste0("Site: ", site,
                              #"\n","Site Proportion: ",prop_concept,
                              #"\n","Proportion: ",prop_concept,
                              #"\n","Site Smoothed Proportion: ",site_loess,
                              #"\n","All-Site Mean: ",mean_allsiteprop,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop_col,
                           #"\n","Proportion: ",prop_concept,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           #"\n","All-Site Mean: ",mean_allsiteprop,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean)) 
  
  p <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion of ', filter_variable, ' Across Time'))
  
  q <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site,
               group=site, text=text_raw)) +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion of ', filter_variable, ' Across Time'))
  
  t <- dat_to_plot %>% 
    distinct(site, dist_eucl_mean, site_loess) %>% 
    group_by(site, dist_eucl_mean) %>% 
    summarise(mean_site_loess = mean(site_loess)) %>%
    mutate(tcol = ifelse(mean_site_loess >= 0.8 | mean_site_loess <= 0.2, 'group1', 'group2')) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess)) + 
    geom_col() + 
    geom_text(aes(label = dist_eucl_mean, color = tcol), vjust = 2, size = 3,
              show.legend = FALSE) +
    scale_color_manual(values = c('white', 'black')) +
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
         title = paste0('Euclidean Distance for ', filter_variable))
  
  plotly_p <- ggplotly(p,tooltip="text")
  plotly_q <- ggplotly(q,tooltip="text")
  
  output <- list(plotly_p,
                 plotly_q,
                 t)
  
  return(output)
  
}
