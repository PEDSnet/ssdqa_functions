

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
                          facet = NULL){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(cli::cli_abort('Please choose an acceptable output level: {.code patient} or {.code row}'))
  
  var_ct <- process_output %>%
    distinct(variable) %>%
    summarise(n()) %>% pull()
  
  if(var_ct > 20){cli::cli_alert_warning('Output has been limited to top 20 variables to improve visibility on y-axis.')}
  
  process_output %>%
    arrange(desc(!!sym(prop))) %>%
    slice(1:20) %>%
    ggplot(aes(y = variable, x = !!sym(prop), fill = variable)) +
    geom_col(show.legend = FALSE) +
    facet_wrap((facet)) +
    scale_fill_ssdqa() +
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
                          facet = NULL){
  
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
    ggplot(aes(x = site, y = variable, fill = !!sym(prop))) +
    geom_tile() +
    geom_text(aes(label = !!sym(prop), color = colors), #size = 6,
              show.legend = FALSE) +
    scale_color_manual(values = c('white', 'black')) +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
    labs(title = paste0('Proportion ', title, ' per Variable & Site'),
         x = 'Site',
         y = 'Variable', 
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
                           facet = NULL){
  
  var_ct <- process_output %>%
    select(concept1, concept2) %>% 
    pivot_longer(cols = c('concept1', 'concept2')) %>%
    distinct(value) %>% summarise(n()) %>% pull()
  
  if(var_ct > 20){cli::cli_alert_warning('Output has been limited to top 20 variables to improve visibility on axes.')}
  
  vars <- process_output %>%
    select(concept1, concept2, concept1_ct, concept2_ct) %>% 
    pivot_longer(cols = c('concept1', 'concept2')) %>% 
    rename(concept1 = concept1_ct, concept2 = concept2_ct) %>% 
    pivot_longer(cols = c(concept1, concept2), 
                 names_to = 'name2', values_to = 'value2') %>% 
    filter(name == name2) %>% 
    distinct(value, value2) %>% 
    arrange(desc(value2)) %>% slice(1:20) %>% pull(value)
  
  plot <- process_output %>%
    filter(concept1 %in% vars & concept2 %in% vars) %>%
    mutate(jaccard_index = round(jaccard_index, 3)) %>%
    ggplot(aes(x = as.character(concept1), y = as.character(concept2), 
               fill = jaccard_index)) + 
    geom_tile_interactive(aes(tooltip = paste0('concept1 = ',concept1, '; n= ',concept1_ct,'\n','concept2 = ',concept2,'; n= ',concept2_ct,
                                               '\n', 'co-occurrence = ', cocount,
                                               '\n','jaccard sim = ',jaccard_index
                                               #'\n', 'mean = ',var_jaccard_mean,'\n','sd = ', var_jaccard_sd
                                               ))) + 
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
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
#' @param text_wrapping_char integer value indicating the point at which axis
#'                           label text should begin to wrap
#'
#' @return a dot plot where the shape of the dot represents whether the point is
#'         anomalous, the color of the dot represents the proportion of rows/patients
#'         for a given variable, and the size of the dot represents the mean proportion
#'        across all sites
#'         
evp_ms_anom_nt<-function(process_output,
                         output_level,
                         text_wrapping_char = 60){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Row'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patient'
  }else(cli::cli_abort('Please choose an acceptable output level: {.code patient} or {.code row}'))
  
  comparison_col = prop
  
  dat_to_plot <- process_output %>%
    mutate(text=paste("Variable: ",variable,
                      "\nSite: ",site,
                      "\nProportion: ",round(!!sym(comparison_col),2),
                      "\nMean proportion:",round(mean_val,2),
                      '\nSD: ', round(sd_val,2),
                      "\nMedian proportion: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2)))
  
  
  #mid<-(max(dat_to_plot[[comparison_col]],na.rm=TRUE)+min(dat_to_plot[[comparison_col]],na.rm=TRUE))/2
  
  plt<-ggplot(dat_to_plot %>% filter(anomaly_yn != 'no outlier in group'),
              aes(x=site, y=variable, text=text, color=!!sym(comparison_col)))+
    geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
    geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'), 
                           aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c(19,8))+
    scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=60)) +
    labs(y = "Variable",
         size="",
         title=paste0('Anomalous Variables per ', title, ' by Site'),
         subtitle = 'Dot size is the mean proportion per variable') +
    guides(color = guide_colorbar(title = 'Proportion'),
           shape = guide_legend(title = 'Anomaly'),
           size = 'none')
  
  girafe(ggobj = plt)
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
                          facet = NULL){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(cli::cli_abort('Please choose an acceptable output level: {.code patient} or {.code row}'))
  
  p <- process_output %>%
    ggplot(aes(y = !!sym(prop), x = time_start, color = variable)) +
    geom_line() +
    scale_color_ssdqa() +
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
#' @param filter_variable the single variable the output should display
#' @param facet columns the user would like to facet by
#'
#' @return a line graph displaying the proportion of patients/rows for each variable
#'         & site over the user-specified time period. On hover, the user can see the exact
#'         proportion for that site, variable, and time point
#' 
evp_ms_exp_at <- function(process_output,
                          output_level,
                          filter_variable,
                          facet = NULL){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
    title <- 'Patients'
  }else(cli::cli_abort('Please choose an acceptable output level: {.code patient} or {.code row}'))
  
  facet <- facet %>% append('variable') %>% unique()
  
  time_inc <- process_output %>% filter(!is.na(time_increment)) %>% distinct(time_increment) %>% pull()
  
  if(time_inc == 'year' && length(filter_variable) > 3){cli::cli_abort('Please choose up to 3 variables for this output type')
    }else if(time_inc != 'year' && length(filter_variable) > 1){cli::cli_abort('Please choose 1 variable for this output type')}
  
  p <- process_output %>%
    filter(variable %in% filter_variable) %>%
    ggplot(aes(y = !!sym(prop), x = time_start, color = site)) +
    geom_line() +
    scale_color_ssdqa() +
    facet_wrap((facet)) +
    theme_minimal() +
    labs(title = paste0('Proportion ', title, ' Over Time'),
         color = 'Site', 
         y = paste0('Proportion ', title),
         x = 'Time')
  
  plot <- ggplotly(p)
  
  
}

#' * Single Site, Anomaly, Across Time *
#' 
#' @param process_output the output provided by the `evp_process` function
#' @param output_level the level of output to be displayed: `patient` or `row`
#' @param filter_variable the single variable the output should display
#' @param facet columns the user would like to facet by
#'
#' @return if analysis was executed by year or greater, a P Prime control chart
#'         is returned with outliers marked with orange dots
#'         
#'         if analysis was executed by month or smaller, an STL regression is 
#'         conducted and outliers are marked with red dots. the graphs representing
#'         the data removed in the regression are also returned
#'         
evp_ss_anom_at <- function(process_output,
                           output_level,
                           filter_variable,
                           facet = NULL){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
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
  }else(cli::cli_abort('Please choose an acceptable output level: {.code patient} or {.code row}'))
  
  time_inc <- process_output %>% distinct(time_increment) %>% pull()
  
  if(time_inc == 'year'){
  
    facet <- facet %>% append('variable') %>% unique()
    
    final <- process_output %>%
      filter(variable %in% filter_variable) %>%
      unite(facet_col, !!!syms(facet), sep = '\n') %>%
      rename('ycol' = ct,
             'denom' = denom)
    
    pp_qi <-  qic(data = final, x = time_start, y = ycol, chart = 'pp', facet = ~facet_col,
        title = paste0('Control Chart: Proportion of ', title, ' per Variable'), 
        ylab = 'Proportion', xlab = 'Time',
        show.grid = TRUE, n = denom)
   
    op_dat <- pp_qi$data
 
   new_pp <- ggplot(op_dat,aes(x,y)) +
     geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
     geom_line(colour = ssdqa_colors_standard[[12]], size = .5) +  
     geom_line(aes(x,cl)) +
     geom_point(colour = ssdqa_colors_standard[[6]] , fill = ssdqa_colors_standard[[6]], size = 1) +
     geom_point(data = subset(op_dat, y >= ucl), color = ssdqa_colors_standard[[3]], size = 2) +
     geom_point(data = subset(op_dat, y <= lcl), color = ssdqa_colors_standard[[3]], size = 2) +
     facet_wrap(~facet1, scales = 'free_y', ncol = 2) +
     ggtitle(label = paste0('Control Chart: Proportion of ', title, ' per Variable')) +
     labs(x = 'Time',
          y = 'Proportion')+
     theme_minimal()
   
   output <- ggplotly(new_pp)
  
  }else{
    
    anomalies <- 
      plot_anomalies(.data=process_output %>% filter(variable == filter_variable),
                     .date_var=time_start) %>% 
      layout(title = paste0('Anomalies for Variable ', filter_variable))
    
    decomp <- 
      plot_anomalies_decomp(.data=process_output %>% filter(variable == filter_variable),
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
#' @param output_level the level of output that should be shown (`person` or `row`)
#' @param filter_variable the variable that should be used to generate output
#' @return three graphs:
#'    1) line graph that shows the smoothed proportion of a 
#'    variable across time computation with the Euclidean distance associated with each line
#'    2) line graph that shows the raw proportion of a 
#'    variable across time computation with the Euclidean distance associated with each line
#'    3) a bar graph with the Euclidean distance value for each site, with the average
#'    proportion as the fill
#' 
#' THIS GRAPH SHOWS ONLY ONE VARIABLE AT A TIME!
#' 

evp_ms_anom_at <- function(process_output,
                           output_level,
                           filter_variable) {
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output_level == 'row'){
    prop <- 'prop_row_variable'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_variable'
  }else(cli::cli_abort('Please choose an acceptable output level: {.code patient} or {.code row}'))
  
  filt_op <- process_output %>% filter(variable == filter_variable) %>%
    mutate(prop_col = !!sym(prop))
  
  allsites <- 
    filt_op %>% 
    select(time_start,variable,mean_allsiteprop) %>% distinct() %>% 
    rename(prop_col=mean_allsiteprop) %>% 
    mutate(site='all site average') %>% 
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Proportion: ",prop_col),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",prop_col)) 
  
  dat_to_plot <- 
    filt_op %>% 
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop_col,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean)) 
  
  p <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    scale_color_ssdqa() +
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
    scale_color_ssdqa() +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion of ', filter_variable, ' Across Time'))
  
  t <- dat_to_plot %>% 
    distinct(site, dist_eucl_mean, site_loess) %>% 
    group_by(site, dist_eucl_mean) %>% 
    summarise(mean_site_loess = mean(site_loess)) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess)) + 
    geom_col() + 
    geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
              show.legend = FALSE) +
    coord_radial(r_axis_inside = FALSE, rotate_angle = TRUE) + 
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() + 
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme(legend.position = 'bottom',
          legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
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
