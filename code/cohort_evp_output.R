

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
    prop <- 'prop_row_concept'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_concept'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  process_output %>%
    ggplot(aes(y = concept_group, x = !!sym(prop), fill = concept_group)) +
    geom_col(show.legend = FALSE) +
    facet_wrap((facet)) +
    scale_fill_brewer(palette = 'Set2') +
    labs(x = paste0('Proportion ', title),
         y = 'Concept Group',
         title = paste0('Proportion of ', title, ' per Concept Group'))
  
  
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
  
  if(output_level == 'row'){
    prop <- 'prop_row_concept'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_concept'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  process_output %>%
    mutate(colors = ifelse(!!sym(prop) < 0.2 | !!sym(prop) > 0.8, 'group1', 'group2')) %>%
    ggplot(aes(y = site, x = concept_group, fill = !!sym(prop))) +
    geom_tile() +
    geom_text(aes(label = !!sym(prop), color = colors), #size = 2, 
              show.legend = FALSE) +
    scale_color_manual(values = c('white', 'black')) +
    scale_fill_viridis_c(option = 'turbo') +
    labs(title = paste0('Proportion ', title, ' per Concept Group & Site'),
         y = 'Site',
         x = 'Concept Group', 
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
    labs(title = 'Co-Occurrence of Concept Sets',
         x = 'concept1',
         y = 'concept2') +
    theme_bw() +
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
    prop <- 'prop_row_concept'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_concept'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  process_output_prep <- process_output %>%
    mutate(domain = concept_group)
  
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
    prop <- 'prop_row_concept'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_concept'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  p <- process_output %>%
    ggplot(aes(y = !!sym(prop), x = time_start, color = concept_group)) +
    geom_line() +
    scale_color_brewer(palette = 'Set2') +
    facet_wrap((facet)) +
    labs(title = paste0('Proportion ', title, ' Over Time'),
         color = 'Concept Group', 
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
    prop <- 'prop_row_concept'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_concept'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  facet <- facet %>% append('concept_group') %>% unique()
  
  p <- process_output %>%
    ggplot(aes(y = !!sym(prop), x = time_start, color = site)) +
    geom_line() +
    scale_color_brewer(palette = 'Set2') +
    facet_wrap((facet)) +
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
#' @param facet columns the user would like to facet by
#'
#' @return a control chart displaying the proportion of patients/rows over the user
#'         specified time period; any orange dots along the line indicate an 
#'         anomalous data point
#'         
evp_ss_anom_at <- function(process_output,
                           output_level,
                           facet){
  
  if(output_level == 'row'){
    ct <- 'concept_row_ct'
    denom <- 'total_row_ct'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    ct <- 'concept_pt_ct'
    denom <- 'total_pt_ct'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  facet <- facet %>% append('concept_group') %>% unique()
  
  final <- process_output %>%
    unite(facet_col, !!!syms(facet), sep = '\n') %>%
    rename('ycol' = ct,
           'denom' = denom)
  
  qic(data = final, x = time_start, y = ycol, chart = 'pp', facet = ~facet_col,
      title = paste0('Control Chart: Proportion of ', title, ' per Concept'), 
      ylab = 'Proportion', xlab = 'Time',
      show.grid = TRUE, n = denom)
  
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
    prop <- 'prop_row_concept'
  }else if(output_level == 'patient'){
    prop <- 'prop_pt_concept'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  filt_op <- process_output %>% filter(concept_group == filter_variable) %>%
    mutate(prop_col = !!sym(prop))
  
  allsites <- 
    filt_op %>% 
    select(time_start,concept_group,mean_allsiteprop) %>% distinct() %>% 
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






#' *Multi Site, Anomaly, Across Time*
#' 
#' codes where a mapping represents a proportion of all mappings for that code which
#' is +/- 2 MAD away from median. 
#' 
#' graph displays the proportion of mappings per code 
#' that are outliers.
#' 
#' @param process_output dataframe output by `evp_process`
#' @param code_type type of code to be used in analysis -- either `patient` or `row`
#' @param facet the variables by which you would like to facet the graph
#' @param mad_dev an integer to define the deviation that should be used to compute the upper and lower MAD limits
#' 
#' @return a heatmap that shows the proportion of mappings for each code that are unstable across
#'         time, meaning they frequently deviate from the all site centroid
#' 
evp_ms_anom_at_old <- function(process_output,
                           output_level,
                           facet,
                           mad_dev = 2){
  
  if(output_level == 'row'){
    ct <- 'concept_row_ct'
    title <- 'Rows'
  }else if(output_level == 'patient'){
    ct <- 'concept_pt_ct'
    title <- 'Patients'
  }else(stop('Please choose an acceptable output level: `patient` or `row`'))
  
  fot <- check_fot_multisite(tblx = process_output %>% ungroup() %>%
                               mutate(start_date = time_start, domain = concept_group),
                             target_col = ct,
                             facet_var = facet,
                             domain_list = process_output %>% distinct(concept_group) %>% pull())
  
  fot2 <- check_fot_all_dist(fot_check_output = fot$fot_heuristic)
  
  mad <- produce_multisite_mad(multisite_tbl = fot2 %>% rename('grp_check' = concept_group),
                               facet_var = facet,
                               mad_dev = mad_dev)
  
  #mad2 <- mad %>% left_join(process_output %>% distinct(site, !!sym(col), !!sym(denom), concept_name))
  
  final <- mad %>%
    mutate(tooltip = paste0('Prop. Outliers: ', grp_outlier_prop, '\nN Outliers: ', grp_outlier_num))
  
  r <- ggplot(final, aes(x=site, y=as.character(grp), fill=grp_outlier_prop)) +
    geom_tile_interactive(aes(tooltip = tooltip), color = 'black') +
    facet_wrap((facet)) +
    scale_fill_viridis_c(option = 'turbo') +
    theme_classic() +
    coord_flip() +
    labs(title = 'Stability of Concepts Over Time',
         y = 'Code',
         fill = 'Proportion Unstable \nConcepts')
  
  p <- girafe(ggobj = r)
  
  return(p)
}
