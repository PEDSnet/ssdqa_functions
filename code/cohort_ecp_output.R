
#' * Single Site, Exploratory, No Time *

ecp_ss_exp_nt <- function(process_output,
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

ecp_ms_exp_nt <- function(process_output,
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

#' * Multi Site, Anomaly, No Time *

ecp_ms_anom_nt <- function(process_output,
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

ecp_ss_exp_at <- function(process_output,
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

ecp_ms_exp_at <- function(process_output,
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

ecp_ss_anom_at <- function(process_output,
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
  
  process_output %>% 
    group_by(!!!syms(facet)) %>%
    group_modify(
      ~spc_calculate(
        data = .x, 
        x = time_start,
        y = !!sym(prop),
        chart = "c"
      )
    ) %>% 
    ungroup() %>%
    # plot
    spc_plot(engine = "ggplot") + 
    facet_wrap((facet)) + 
    theme(panel.background = element_rect("white", "grey80")) +
    labs(title = paste0('Control Chart: Proportion ', title, ' Over Time'))
  
}

#' * Multi Site, Anomaly, Across Time *


