
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
                          code_type,
                          facet,
                          vocab_tbl = vocabulary_tbl('concept')){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
    color <- 'concept_id'
    prop <- 'source_prop'
    denom <- 'denom_source_ct'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    color <- 'source_concept_id'
    prop <- 'concept_prop'
    denom <- 'denom_concept_ct'
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  facet <- facet %>% append(col)
  
  if(is.null(vocab_tbl)){
    
    p <- process_output %>%
      mutate(concept_id = as.character(concept_id),
             source_concept_id = as.character(source_concept_id)) %>%
      ggplot(aes(y = !!sym(prop), x = time_start, color = !!sym(color))) +
      geom_line() +
      facet_wrap((facet)) +
      labs(title = 'Code Mapping Pairs Over Time')

    plot <- ggplotly(p)
    
    ref_tbl <- generate_ref_table(tbl = process_output,
                                  col = col,
                                  denom = denom,
                                  vocab_tbl = vocab_tbl,
                                  time = TRUE)
    
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
    
    plot <- ggplotly(p)
    
    ref_tbl <- generate_ref_table(tbl = process_output_db,
                                  col = col,
                                  denom = denom,
                                  vocab_tbl = vocab_tbl,
                                  time = TRUE)
  }
  
  output <- list(plot, ref_tbl)
  
  return(output)
  
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
    grp1 <- c('start_date', 'centroid', 'source_concept_id', 'concept_id', facet_var) %>% unique()
    grp2 <- c('site', concept_col, facet_var) %>% unique()
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
                           code_type,
                           facet,
                           mad_dev = 2,
                           vocab_tbl = vocabulary_tbl('concept')){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
    denom <- 'denom_source_ct'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    denom <- 'denom_concept_ct'
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  fot <- fot_check(tblx = process_output %>% ungroup() %>%
                     mutate(start_date = time_start),
                   target_col = 'ct',
                   facet_var = facet %>% append(c('concept_id', 'source_concept_id')))
  
  fot2 <- check_fot_all_dist(fot_check_output = fot$fot_heuristic)
  
  mad <- produce_multisite_mad_scv(multisite_tbl = fot2,
                                   code_type = code_type,
                                   facet_var = facet,
                                   mad_dev = mad_dev)
  
  final <- mad %>% left_join(process_output %>% distinct(site, !!sym(col), !!sym(denom)))
  
  if(is.null(vocab_tbl)){
    
    final <- final %>% mutate(denom_fmt = format(!!sym(denom), big.mark = ','),
                              tooltip = paste0('Total Concept Rows: ', denom_fmt))
    
    r <- ggplot(final, aes(x=site, y=as.character(!!sym(col)), fill=grp_outlier_prop)) +
      geom_tile_interactive(aes(tooltip = tooltip)) +
      facet_wrap((facet)) +
      scale_fill_viridis_c(option = 'turbo') +
      theme_classic() +
      coord_flip() +
      labs(title = 'Stability of Mappings Over Time',
           y = 'Code',
           fill = 'Proportion Unstable \nMappings')
    
    p <- girafe(ggobj = r)
    
  }else{
    
    final_db <- copy_to_new(df = final)
    
    final <- final_db %>% 
      mutate(join_col = !!sym(col)) %>%
      left_join(select(vocab_tbl, concept_id, concept_name), by = c('join_col' = 'concept_id')) %>%
      collect_new() %>%
      mutate(denom_fmt = format(!!sym(denom), big.mark = ','),
             tooltip = paste0('Concept Name: ', concept_name, '\nTotal Concept Rows: ', denom_fmt))
    
    r <- ggplot(final, aes(x=site, y=as.character(!!sym(col)), fill=grp_outlier_prop)) +
      geom_tile_interactive(aes(tooltip = tooltip)) +
      facet_wrap((facet)) +
      scale_fill_viridis_c(option = 'turbo') +
      theme_classic() +
      coord_flip() +
      labs(title = 'Stability of Mappings Over Time',
           y = 'Code',
           fill = 'Proportion Unstable \nMappings')
    
    p <- girafe(ggobj = r)
  
    }
 
   return(p)
}


#' *Single Site, Anomaly, Across Time*
#' 
#' Control chart looking at number of mappings over time
#' 
#' using the CHOP-developed package called `rocqi` 
#' 

scv_ss_anom_at <- function(process_output,
                           code_type,
                           facet){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'}
  
  facet <- facet %>% append(col)
  
  n_mappings_yr <- process_output %>%
    group_by(!!!syms(facet), time_start) %>%
    summarise(n_mappings = n())
  
  n_mappings_yr %>% 
    group_by(!!!syms(facet)) %>%
    group_modify(
      ~spc_calculate(
        data = .x, 
        x = time_start,
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
