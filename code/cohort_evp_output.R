

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
#' Produces graphs showing AUCs
#' 
#' @param process_output_graph output from `evp_process`
#' @param filter_variable the variable that should be used to generate output
#' @return two graphs:
#'    1) line graph that shows the proportion of a 
#'    code across time computation with the AUC associated with each line
#'    2) bar graph with each site on the x-axis, and the y-axis the AUC value, 
#'    with a dotted line showing the all-site average
#' 
#' THIS GRAPH SHOWS ONLY ONE VARIABLE AT A TIME!
#' 

evp_ms_anom_at <- function(process_output_graph,
                           output_level,
                           filter_variable) {
  
  if(output_level == 'row'){
    var_col <- 'prop_row_concept'
  }else if(output_level == 'patient'){
    var_col <- 'prop_pt_concept'
  }else(stop('Please select a valid output_level: `patient` or `row`'))
  
  allsites <- 
    process_output_graph %>% 
    filter(concept_group == filter_variable) %>% 
    select(time_start,concept_group,mean_allsiteprop,auc_gold_standard) %>% distinct() %>% 
    rename_with(~var_col, mean_allsiteprop) %>% 
    mutate(site='all site average',
           auc_value=auc_gold_standard) %>% 
    mutate(text=paste0("Site: ", site,
                       #"\n","Proportion: ",prop_concept,
                       "\n","AUC Value: ",auc_value,
                       "\n","All-Site AUC: ",auc_gold_standard)) 
  
  #%>% 
  dat_to_plot <- 
    process_output_graph %>% 
    filter(concept_group == filter_variable) %>% 
    mutate(text=paste0("Site: ", site,
                       #"\n","Proportion: ",prop_concept,
                       "\n","AUC Value: ",auc_value,
                       "\n","All-Site AUC: ",auc_gold_standard)) 
  
  concept_var <- 
    dat_to_plot %>% select(concept_group) %>% distinct() %>% pull
  
  if(length(concept_var) > 1) {stop('Please input only one concept_id')}
  
  p <- dat_to_plot %>%
    #filter(concept_id == 81893) %>% 
    ggplot(aes(y = !!sym(var_col), x = time_start, color = site,group=site, text=text)) +
    geom_line(data= filter(allsites, concept_group==concept_var), linewidth = 1.1) +
    #stat_smooth(geom='line',alpha=0.7,se=TRUE) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    ggtitle(paste0('Proportion of ',concept_var,' Across Time',
                   '\n','All-Site in Red',
                   '\n','AUC value in Tooltip'))
  
  
  gold_standard <- allsites %>%
    filter(concept_group == filter_variable) %>% 
    select(auc_gold_standard) %>%
    distinct() %>% pull()
  
  p2 <- dat_to_plot %>% 
    #filter(concept_id == 81893) %>% 
    select(site,auc_value,auc_gold_standard) %>% 
    distinct() %>% 
    ggplot(aes(y = auc_value, x = site, fill=site)) +
    geom_bar(stat='identity') + 
    geom_hline(yintercept=gold_standard,
               linetype='dashed', color='red') +
    coord_flip() +
    guides(fill='none') + theme_minimal() +
    ggtitle(paste0('Site AUC Value for ',concept_var,
                   '\n','All-Site in Dashed Red'))
  
  
  plotly_p <- ggplotly(p,tooltip="text")
  plotly_p2 <- ggplotly(p2,tooltip='auc_value')
  
  output <- list(plotly_p,
                 plotly_p2)
  
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
