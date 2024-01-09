
#' OUTPUT GENERATION

#' HR COMMENT: The parameter to this function `scv_process` is
#' the name of a function in the other file. I changed it in the
#' table below. 

#' *Single Site, Anomaly, Across Time*
#' 
#' Facets by main code (cdm or source) by default, with each line representing
#' a mapping code. using plotly so the legend is interactive and codes can be isolated
#' 
#' Use this same thing for multi site and just facet by site? or do we need another
#' visualization

#' HR COMMENT: For this, too, can we hover over codes to see what the concept
#' name is? Also, what is the anomaly here? I think this is exploratory, correct?
#' Should we create a caption that explains the content of this graph? 
#' 
scv_ss_ms_exp_at <- function(process_output,
                          output,
                          facet,
                          vocab_tbl = vocabulary_tbl('concept')){
  
  if(output == 'source'){
    facet <- facet %>% append('source_concept_id')
    color <- 'concept_id'
    prop <- 'source_prop'
  }else if(output == 'cdm'){
    facet <- facet %>% append('concept_id')
    color <- 'source_concept_id'
    prop <- 'concept_prop'
  }else{stop('Please select a valid output - `source` or `cdm`')}
  
  
  if(is.null(vocab_tbl)){
    
    p <- process_output %>%
      mutate(concept_id = as.character(concept_id),
             source_concept_id = as.character(source_concept_id)) %>%
      ggplot(aes(y = !!sym(prop), x = time_start, color = !!sym(color))) +
      geom_line() +
      facet_wrap((facet)) +
      labs(title = 'Code Mapping Pairs Over Time')

    ggplotly(p)
    
  }else{
    
    process_output_db <- copy_to_new(df = process_output)
    
    process_output_plot <- process_output_db %>%
      mutate(join_col = !!sym(color)) %>%
      left_join(select(vocab_tbl, concept_id, concept_name), 
                by = c('join_col' = 'concept_id'))
    
    p <- process_output_plot %>%
      mutate(concept_id = as.character(concept_id),
             source_concept_id = as.character(source_concept_id)) %>%
      ggplot(aes(y = !!sym(prop), x = time_start, color = !!sym(color),
                 text = paste('concept_name: ', concept_name))) +
      geom_line() +
      facet_wrap((facet)) +
      labs(title = 'Code Mapping Pairs Over Time',
           color = color)
    
    ggplotly(p)
  }
  
}


#' *Multi Site, Anomaly, Across Time*
#' 
#' Similar to PF -- 
#' 
#' codes where a mapping represents a proportion of all mappings for that code which
#' is +/- 2 MAD away from median. 
#' 
#' graph displays the proportion of mappings per code 
#' that are outliers.
#' 
#' 
#' 

produce_multisite_mad_scv <- function(multisite_tbl,
                                      code_type,
                                      facet_var = NULL,
                                      mad_dev) {
  if(code_type == 'source'){
    concept_col <- 'source_concept_id'
  }else if(code_type == 'cdm'){
    concept_col <- 'concept_id'
  }else{stop('Please select a valid code type')}
  
  
  if(is.null(facet_var)){
    grp1 <- c('start_date', 'centroid', 'source_concept_id', 'concept_id')
    grp2 <- c('site', concept_col)
  }else{
    grp1 <- c('start_date', 'centroid', 'source_concept_id', 'concept_id', facet_var)
    grp2 <- c('site', concept_col, facet_var)
  }
  
  mad_computation <- 
    multisite_tbl %>% 
    group_by(!!!syms(grp1)) %>% 
    summarise(mad_pt=mad(check, center=centroid)) %>% 
    ungroup() %>% 
    mutate(lower_mad = mad_pt - (abs(mad_pt*mad_dev)),
           upper_mad = mad_pt + (abs(mad_pt*mad_dev)))
  
  full_tbl_outliers <- 
    multisite_tbl %>% ungroup() %>% 
    inner_join(mad_computation) %>% 
    mutate(
      outlier=case_when((distance < lower_mad) | (distance > upper_mad) ~ 1,
                        TRUE ~ 0)
    ) %>% filter(! site=='all')
  
  sites_grp_outliers <- 
    full_tbl_outliers %>% 
    group_by(!!!syms(grp2)) %>% 
    filter(outlier==1) %>% 
    summarise(grp_outlier_num=n()) %>%  ungroup() 
  
  sites_grp_ct_total <- 
    full_tbl_outliers %>% 
    group_by(!!!syms(grp2)) %>% 
    summarise(grp_total_num=n()) %>% ungroup()
  
  sites_grp_total <- 
    sites_grp_ct_total %>% 
    left_join(sites_grp_outliers) %>% 
    mutate(grp_outlier_prop = round(grp_outlier_num/grp_total_num,2))
  
  sites_total <- 
    sites_grp_total %>% ungroup() %>% 
    group_by(site) %>% 
    mutate(site_total_num=sum(grp_total_num, na.rm = TRUE),
           site_total_outlier=sum(grp_outlier_num, na.rm = TRUE)) %>% 
    mutate(site_outlier_prop=round(site_total_outlier/site_total_num,2)) 
  
}

scv_ms_anom_at <- function(process_output,
                           output,
                           facet,
                           mad_dev = 2){
  
  facet <- facet %>% append(c('concept_id', 'source_concept_id'))
  
  if(output == 'source'){
    y_col <- 'source_concept_id'
    prop <- 'source_prop'
  }else if(output == 'cdm'){
    y_col <- 'concept_id'
    prop <- 'concept_prop'
  }else{stop('Please select a valid output - `source` or `cdm`')}
  
  fot <- fot_check(tblx = process_output %>% ungroup() %>%
                     mutate(start_date = time_start),
                   target_col = prop,
                   facet_var = facet)
  
  fot2 <- check_fot_all_dist(fot_check_output = fot$fot_heuristic)
  
  mad <- produce_multisite_mad_scv(multisite_tbl = fot2,
                                   code_type = output,
                                   mad_dev = mad_dev)
  
  r <- ggplot(mad, aes(x=site, y=as.character(!!sym(y_col)), fill=grp_outlier_prop)) +
    geom_tile() +
    facet_wrap((facet)) +
    scale_fill_viridis_c(option = 'turbo') +
    theme_classic() +
    coord_flip() +
    labs(title = 'Codes with Representative Proportions of Mappings +/- 2 MAD away from Median',
         y = 'Code')
}


#' *Single Site, Anomaly, Across Time*
#' 
#' Control chart looking at number of mappings over time
#' 
#' using the CHOP-developed package called `rocqi` 
#' 

scv_ss_anom_at <- function(process_output,
                           output,
                           facet){
  
  if(output == 'source'){
    col <- 'source_concept_id'
  }else if(output == 'cdm'){
    col <- 'concept_id'}
  
  facet <- facet %>% append(col)
  
  n_mappings_yr <- process_output %>%
    group_by(!!!syms(facet), start_date) %>%
    summarise(n_mappings = n())
  
  n_mappings_yr %>% 
    group_by(!!!syms(facet)) %>%
    group_modify(
      ~spc_calculate(
        data = .x, 
        x = start_date,
        y = n_mappings,
        chart = "c"
      )
    ) %>% 
    ungroup() %>%
    # plot
    spc_plot(engine = "ggplot") + 
    facet_wrap((facet)) + 
    theme(panel.background = element_rect("white", "grey80")) +
    labs(title = 'Control Chart: Number of Mappings per Code Over Time')
  
  
  
}
