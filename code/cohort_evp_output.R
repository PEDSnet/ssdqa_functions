
#' * Single Site, Exploratory, No Time *

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

evp_ss_anom_at <- function(process_output,
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
  
  final <- process_output %>%
    unite(facet_col, !!!syms(facet), sep = '\n') %>%
    rename('ycol' = prop)
  
  qic(data = final, x = time_start, y = ycol, chart = 'c', facet = ~facet_col,
      title = paste0('Control Chart: Proportion ', title, ' per Concept'), 
      ylab = 'Proportion', xlab = 'Time',
      show.grid = TRUE)
  
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
evp_ms_anom_at <- function(process_output,
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
  
  fot <- fot_check(tblx = process_output %>% ungroup() %>%
                     mutate(start_date = time_start),
                   target_col = ct,
                   facet_var = facet %>% append('concept_group'))
  
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
