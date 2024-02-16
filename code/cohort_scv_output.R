
#' Generate concept reference table to accompany output
#' 
#' @param tbl intermediate table generated in the output function that contains the concepts
#'            of interest to be displayed in the reference table
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' @param col the name of the column with the concept that needs to be summarised in the 
#'            refrence table
#' @param denom the denominator count associated with @col to be displayed in the 
#'              reference table
#' @param time logical to define whether @tbl has over time output or not
#' 
#' @return a reference table with summary information about the codes in the output that 
#'         could not be displayed in the associated graph

generate_ref_table <- function(tbl,
                               vocab_tbl,
                               col,
                               denom,
                               time = FALSE){
  if(!time){
    
    if(!is.null(vocab_tbl)){
      
      t <- join_to_vocabulary(tbl = tbl,
                              vocab_tbl = vocab_tbl,
                              col = col) %>%
        rename('denom_col' = denom) %>%
        distinct(site, !!sym(col), concept_name, denom_col) %>%
        gt::gt() %>%
        fmt_number(denom_col, decimals = 0) %>%
        data_color(palette = "Dark2", columns = c(site)) %>%
        cols_label(denom_col = 'Total Count') %>%
        tab_header('Concept Reference Table')
      
    }else{
      
      t <- tbl %>%
        rename('denom_col' = denom) %>%
        distinct(site, !!sym(col), denom_col) %>%
        gt::gt() %>%
        fmt_number(denom_col, decimals = 0) %>%
        data_color(palette = "Dark2", columns = c(site)) %>%
        cols_label(denom_col = 'Total Count') %>%
        tab_header('Concept Reference Table')
      
    }
  }else{
    
    time_inc <- tbl %>% ungroup() %>% distinct(time_increment) %>% pull()
    
    if(!is.null(vocab_tbl)){
      
      t <- join_to_vocabulary(tbl = tbl,
                              vocab_tbl = vocab_tbl,
                              col = col) %>%
        rename('denom_col' = denom) %>%
        distinct(site, !!sym(col), concept_name, denom_col) %>%
        group_by(site, !!sym(col)) %>%
        mutate(denom_col = sum(denom_col)) %>%
        ungroup() %>%
        distinct() %>%
        gt::gt() %>%
        fmt_number(denom_col, decimals = 0) %>%
        data_color(palette = "Dark2", columns = c(site)) %>%
        cols_label(denom_col = 'Total Count (All Time Points)') %>%
        tab_header('Concept Reference Table')
      
    }else{
      
      t <- tbl %>%
        rename('denom_col' = denom) %>%
        distinct(site, !!sym(col), denom_col) %>%
        group_by(site, !!sym(col)) %>%
        mutate(denom_col = sum(denom_col)) %>%
        ungroup() %>%
        distinct() %>%
        gt::gt() %>%
        fmt_number(denom_col, decimals = 0) %>%
        data_color(palette = "Dark2", columns = c(site)) %>%
        cols_label(denom_col = 'Total Count (All Time Points)') %>%
        tab_header('Concept Reference Table')
      
    }
  }
  
  return(t)
  
}


#' Compute number and median mappings per code (SCV)
#'
#' @param tbl intermediate table generated in the output function that contains the concepts
#'            of interest to be used to compute number of mappings
#' @param col the name of the column with the concept that needs to be summarized
#' @param denom the denominator count associated with @col 
#' @param facet grouping variables to be used to compute group-specific counts and distance
#'              from overall median
#'
#' @return dataframe that summarizes the overall median number of mappings, group-specific number of
#'         mappings, and how many MAD from the overall median a code falls
#' 
compute_mappings_per_code <- function(tbl,
                                      col,
                                      denom,
                                      facet){
  
  mappings_total <- tbl %>%
    group_by(!!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    mutate(median = median(n_mappings),
           q1 = quantile(n_mappings, 0.25),
           q3 = quantile(n_mappings, 0.75)) %>%
    select(col, median, q1, q3) %>% ungroup() %>%
    left_join(tbl %>% distinct(!!sym(col), !!sym(denom)))
  
  mappings_group <- tbl %>%
    group_by(!!!syms(facet), !!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    inner_join(mappings_total) %>%
    distinct() %>%
    mutate(mad = mad(n_mappings, center = median)) %>%
    ungroup() %>%
    mutate(dist_median = abs(n_mappings - median),
           n_mad = dist_median/mad)
  
  return(mappings_group)
}


#' *Single Site, Exploratory, No Time*
#' 
#' 
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`
#' 
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' @param num_codes the number of top codes of code_type that should be displayed in the graph
#' @param num_mappings the number of top mappings that should be displayed for each code
#' 
#' @return a heatmap with one facet per code and additional facet groupings, limited to the number
#'         of codes selected with @num_codes 
#'         mapped code along the y-axis and proportion of representation as the fill
#' @return a reference table with additional information about the codes used as the facet.
#'         includes the code and denominator count, and will also include the concept name
#'         if @vocab_tbl is not NULL
#' 
scv_ss_exp_nt <- function(process_output,
                          code_type,
                          facet,
                          vocab_tbl = vocabulary_tbl('concept'),
                          num_codes = 10,
                          num_mappings = 25){
  
  # picking columns / titles 
  if(code_type == 'cdm'){
    denom <-  'denom_concept_ct'
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    prop <- 'concept_prop'
    title <- paste0('Top ', num_mappings, ' Mappings for Top ', num_codes, ' CDM Codes')
  }else if(code_type == 'source'){
    denom <- 'denom_source_ct'
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    prop <- 'source_prop'
    title <- paste0('Top ', num_mappings, ' Mappings for Top ', num_codes, ' Source Codes')
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  
  ## filter output down to most common codes, selecting a user-provided number
  topcodes <- process_output %>%
    ungroup() %>%
    select(col, denom, all_of(facet)) %>%
    distinct() %>%
    group_by(!!! syms(facet)) %>%
    arrange(desc(!! sym(denom))) %>%
    slice(1:num_codes)
  
  ref <- process_output %>% 
    ungroup() %>%
    inner_join(topcodes) 
  
  nmap_total <- ref %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    summarise(nmap = n())
  
  nmap_top <- ref %>%
    select(col, map_col, all_of(facet), prop) %>%
    distinct() %>% 
    group_by(!!sym(col), !!!syms(facet)) %>%
    arrange(desc(!!sym(prop))) %>%
    slice(1:num_mappings)
  
  if(is.null(vocab_tbl)){
    final <- ref %>%
      inner_join(nmap_top) %>%
      left_join(nmap_total) %>%
      mutate(xaxis = paste0(!!sym(col), '\n Total Mappings: ', nmap))
    
    facet <- facet %>% append('xaxis')
    
    p <- final %>% ggplot(aes(x = xaxis, y = as.character(!!sym(map_col)), 
                              fill = !!sym(prop))) + 
      geom_tile() + 
      geom_text(aes(label = !!sym(prop)), size = 2, color = 'black') +
      scale_fill_viridis_c(option = 'turbo') +
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_blank()) +
      labs(title = title,
           x = col,
           y = map_col)
    
    # Summary Reference Table
    ref_tbl <- generate_ref_table(tbl = final,
                                  col = col,
                                  denom = denom,
                                  vocab_tbl = vocab_tbl)
    
  }else{
    
    final_filt <- ref %>%
      inner_join(nmap_top) %>%
      left_join(nmap_total) 
    
    final <- join_to_vocabulary(tbl = final_filt,
                                vocab_tbl = vocab_tbl,
                                col = map_col) %>%
      mutate(xaxis = paste0(!!sym(col), '\n Total Mappings: ', nmap)) 
    
    facet <- facet %>% append('xaxis')
    
    ## ggiraph interactive
    plot <- final %>% ggplot(aes(x = xaxis, y = as.character(!!sym(map_col)), 
                                 fill = !!sym(prop))) +
      geom_tile_interactive(aes(tooltip = concept_name)) +
      geom_text(aes(label = !!sym(prop)), size = 2, color = 'black') +
      scale_fill_viridis_c(option = 'turbo') +
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_blank()) +
      labs(title = title,
           x = col,
           y = map_col)
    
    p <- girafe(ggobj = plot,
                width = 10,
                height = 10)
    
    # Summary Reference Table
    ref_tbl <- generate_ref_table(tbl = final_filt,
                                  col = col,
                                  denom = denom,
                                  vocab_tbl = vocab_tbl)
  }
  
  output <- list(p, ref_tbl)
  
  return(output)
}


#' *Multi Site, Exploratory, No Time*
#' 
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`
#' 
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' @param num_codes the number of top codes of code_type that should be displayed in the graph
#' 
#' @return a searchable and filterable table with mappings, proportion of representation, and
#'         denominator counts for the number of codes selected
#'         in @num_codes
#'         concept name will be included if @vocab_tbl is not NULL
#' 
scv_ms_exp_nt <- function(process_output,
                          code_type,
                          facet,
                          vocab_tbl = vocabulary_tbl('concept'),
                          num_codes = 10){
  
  # picking columns / titles 
  if(code_type == 'cdm'){
    denom <-  'denom_concept_ct'
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    prop <- 'concept_prop'
  }else if(code_type == 'source'){
    denom <- 'denom_source_ct'
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    prop <- 'source_prop'
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  ## Enfore site facetting
  facet <- facet %>% append('site') %>% unique()
  
  ## filter output down to most common codes, selecting a user-provided number
  topcodes <- process_output %>%
    ungroup() %>%
    select(col, denom, all_of(facet)) %>%
    distinct() %>%
    group_by(!!! syms(facet)) %>%
    arrange(desc(!! sym(denom))) %>%
    slice(1:num_codes)
  
  if(is.null(vocab_tbl)){
    final <- process_output %>% 
      inner_join(topcodes)
    
    table <- final %>%
      ungroup() %>%
      select(all_of(facet), source_concept_id, concept_id, ct, prop) %>%
      mutate(pct = !!sym(prop)) %>%
      arrange(!!!syms(facet), desc(ct)) %>%
      gt::gt() %>%
      cols_nanoplot(columns = pct, plot_type = 'bar',
                    autohide = TRUE, new_col_label = 'percent') %>%
      #gtExtras::gt_plt_bar_pct(column = pct) %>%
      fmt_number(columns = ct, decimals = 0) %>%
      fmt_percent(columns = prop, decimals = 0) %>%
      data_color(palette = "Dark2", columns = c(all_of(facet))) %>%
      tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Codes')) %>%
      opt_interactive(use_search = TRUE,
                      use_filters = TRUE)
    
    return(table)
    
  }else{
    
    final_filt <- process_output %>% 
      inner_join(topcodes)
    
    final <- join_to_vocabulary(tbl = final_filt,
                                vocab_tbl = vocab_tbl,
                                col = map_col)
    
    table <- final %>%
      ungroup() %>%
      select(all_of(facet), col, map_col, concept_name, ct, prop) %>%
      mutate(pct = !!sym(prop)) %>%
      arrange(!!!syms(facet), desc(ct)) %>%
      gt::gt() %>%
      cols_nanoplot(columns = pct, plot_type = 'bar',
                    autohide = TRUE, new_col_label = 'percent') %>%
      #gtExtras::gt_plt_bar_pct(column = pct) %>%
      fmt_number(columns = ct, decimals = 0) %>%
      fmt_percent(columns = prop, decimals = 0) %>%
      data_color(palette = "Dark2", columns = c(all_of(facet))) %>%
      tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Codes')) %>%
      opt_interactive(use_search = TRUE,
                      use_filters = TRUE)
    
    return(table)
  }
  
}

#' 
#' *Single Site, Anomaly, No Time*
#' 
#' 
#' 
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`
#' 
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' @param rel_to_median option to select whether values `greater` or `less` than the median are 
#'                      displayed. both will also display values equal to the median
#'                      
#' @return a bar graph with codes with a number of mappings either greater or less than the overall
#'         median number of mappings (based on `rel_to_median`). median is shown as a solid line while
#'         Q1 and Q3 are shown as dotted lines
#'
scv_ss_anom_nt <- function(process_output,
                           code_type,
                           facet,
                           vocab_tbl = vocabulary_tbl('concept'),
                           rel_to_median = 'greater'){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
    denom <- 'denom_source_ct'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    denom <- 'denom_concept_ct'
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  mappings_per_code <- compute_mappings_per_code(tbl = process_output,
                                                 col = col,
                                                 denom = denom,
                                                 facet = facet)
  
  if(rel_to_median == 'greater'){
    tbl_filt <- mappings_per_code %>%
      filter(n_mappings >= median)
  }else if(rel_to_median == 'less'){
    tbl_filt <- mappings_per_code %>%
      filter(n_mappings <= median)
  }else(stop('Invalid selection for rel_to_median: please select `greater` or `less`'))
  
  if(is.null(vocab_tbl)){
    tbl <- tbl_filt %>%
      mutate(denom_fmt = format(!!sym(denom), big.mark = ','),
             tooltip = paste0('Total Concept Mappings: ', n_mappings, '\nTotal Concept Rows: ', denom_fmt))
    
    plot <- tbl %>%
      ggplot(aes(x = as.character(!!sym(col)), y = n_mappings, fill = as.character(!!sym(col)))) +
      geom_col_interactive(aes(tooltip = tooltip)) +
      geom_hline(aes(yintercept = median)) +
      geom_hline(aes(yintercept = q1), linetype = 'dotted') +
      geom_hline(aes(yintercept = q3), linetype = 'dotted') +
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, vjust = 1),
            legend.position = 'none') +
      labs(title = 'Codes with Anomalous Number of Unique Mappings',
           x = col)
    
    girafe(ggobj = plot,
           width = 10,
           height = 10)
  }else{
    
    tbl <- join_to_vocabulary(tbl = tbl_filt,
                              vocab_tbl = vocab_tbl,
                              col = col) %>%
      mutate(denom_fmt = format(!!sym(denom), big.mark = ','),
             tooltip = paste0('Concept Name: ', concept_name, '\nTotal Concept Mappings: ', n_mappings, 
                              '\nTotal Concept Rows: ', denom_fmt))
    
    plot <- tbl %>%
      ggplot(aes(x = as.character(!!sym(col)), y = n_mappings, fill = as.character(!!sym(col)))) +
      geom_col_interactive(aes(tooltip = tooltip)) +
      geom_hline(aes(yintercept = median)) +
      geom_hline(aes(yintercept = q1), linetype = 'dotted') +
      geom_hline(aes(yintercept = q3), linetype = 'dotted') +
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, vjust = 1),
            legend.position = 'none') +
      labs(title = 'Codes with Anomalous Number of Unique Mappings',
           x = col)
    
    girafe(ggobj = plot,
           width = 10,
           height = 10)
  }
  
}

#' 
#' *Multi-Site, Anomaly, No Time*
#' 
#' 
#' shows the number of MAD away from the TOTAL median for each code and site 
#' (i.e. not the MAD away from site specific median)
#' 
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`
#' 
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param rel_to_median option to select whether values `greater` or `less` than the median are 
#'                      displayed. both will also display values equal to the median
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' 
#' @return a heat map with one facet per site and additional facet variables. codes with a number
#'         of mappings greater or less than the overall median (based on `rel_to_median`) are displayed,
#'         with fill representing the number of MAD the code falls away from the overall median
#' 
scv_ms_anom_nt <- function(process_output,
                           code_type,
                           facet,
                           rel_to_median = 'greater',
                           vocab_tbl = vocabulary_tbl('concept')){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    denom <- 'denom_source_ct'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    denom <- 'denom_concept_ct'
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  mappings_per_code <- compute_mappings_per_code(tbl = process_output,
                                                 col = col,
                                                 denom = denom,
                                                 facet = facet)
  
  
  if(rel_to_median == 'greater'){
    tbl_filt <- mappings_per_code %>%
      filter(n_mappings >= median)
  }else if(rel_to_median == 'less'){
    tbl_filt <- mappings_per_code %>%
      filter(n_mappings <= median)
  }else(stop('Invalid selection for rel_to_median: please select `greater` or `less`'))
  
  if(is.null(vocab_tbl)){
    
    tbl <- tbl_filt %>%
      mutate(denom_fmt = format(!!sym(denom), big.mark = ','),
             tooltip = paste0('MAD from Median: ', n_mad, '\nTotal Concept Mappings: ', n_mappings, 
                              '\nTotal Concept Rows: ', denom_fmt))
    
    plot <- tbl %>%
      ggplot(aes(y = as.character(!!sym(col)), x = site, fill = n_mad)) +
      geom_tile_interactive(aes(tooltip = tooltip)) +
      facet_wrap((facet), scales = 'free', ncol = 1) +
      scale_fill_viridis_c(option = 'turbo') +
      labs(title = 'MAD from Median Number of Mappings per Code',
           y = col,
           x = '')
    
    girafe(ggobj = plot,
           width = 10,
           height = 10)
    
  }else{
    
    tbl <- join_to_vocabulary(tbl = tbl_filt,
                              vocab_tbl = vocab_tbl,
                              col = col) %>%
      mutate(denom_fmt = format(!!sym(denom), big.mark = ','),
             tooltip = paste0('Concept Name: ', concept_name, '\nMAD from Median: ', n_mad, 
                              '\nTotal Concept Mappings: ', n_mappings, 
                              '\nTotal Concept Rows: ', denom_fmt))
    
    plot <- tbl %>%
      ggplot(aes(y = as.character(!!sym(col)), x = site, fill = n_mad)) +
      geom_tile_interactive(aes(tooltip = tooltip)) +
      facet_wrap((facet), scales = 'free', ncol = 1) +
      #scale_fill_gradientn(colors = viridis::turbo(10))
      scale_fill_viridis_c(option = 'turbo') +
      labs(title = 'MAD from Median Number of Mappings per Code',
           y = col,
           x = '')
    
    girafe(ggobj = plot,
           width = 10,
           height = 10)
    
  }
  
}


#' *Single Site, Anomaly, Across Time*
#' 
#' Facets by main code (cdm or source) by default, with each line representing
#' a mapping code. using plotly so the legend is interactive and codes can be isolated
#' 
#' 
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`
#' 
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' 
#' @return a line graph with one facet per code displaying the proportion of mapped codes
#'         across the user selected time period
#' @return a reference table with total counts of each code across the entire user selected
#'         time period
#' 
scv_ss_ms_exp_at <- function(process_output,
                          code_type,
                          facet,
                          vocab_tbl = vocabulary_tbl('concept')){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    prop <- 'source_prop'
    denom <- 'denom_source_ct'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    prop <- 'concept_prop'
    denom <- 'denom_concept_ct'
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  facet <- facet %>% append(col, 'site') %>% unique()
  
  if(is.null(vocab_tbl)){
    
    p <- process_output %>%
      mutate(concept_id = as.character(concept_id),
             source_concept_id = as.character(source_concept_id)) %>%
      ggplot(aes(y = !!sym(prop), x = time_start, color = !!sym(map_col))) +
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
    
    process_output_plot <- join_to_vocabulary(tbl = process_output,
                                              vocab_tbl = vocab_tbl,
                                              col = map_col)
    
    p <- process_output_plot %>%
      mutate(concept_id = as.character(concept_id),
             source_concept_id = as.character(source_concept_id)) %>%
      ggplot(aes(y = !!sym(prop), x = time_start, color = !!sym(map_col),
                 label = concept_name,
                 label2 = ct
                 )) +
      geom_line() +
      facet_wrap((facet)) +
      labs(title = 'Code Mapping Pairs Over Time',
           color = map_col)
    
    plot <- ggplotly(p)
    
    ref_tbl <- generate_ref_table(tbl = process_output,
                                  col = col,
                                  denom = denom,
                                  vocab_tbl = vocab_tbl,
                                  time = TRUE)
  }
  
  output <- list(plot, ref_tbl)
  
  return(output)
  
}

#' compute multisite MAD
#' 
#' some small edits to make it more compatible with SCV -- need to return
#' to this to make it more generalizable across functions
#'
#' @param multisite_tbl a tbl with all sites and a `grp_check` column, as well as a `month_end`, 
#'                      `distance`, `site` columns; output from the `check_fot_multisite` function
#' @param facet_var list of variables by which the user would like to facet the output;
#'                  should match the facets used in `check_fot_multisite`
#' @param mad_dev an integer to define the deviation that should be used to compute the upper and lower MAD limits
#' 
#' @return dataframe that includes statistics relating to the number of outliers / anomalous measures are present
#'         in the data based on deviation from the MAD
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


#' *Multi Site, Anomaly, Across Time*
#' 
#' codes where a mapping represents a proportion of all mappings for that code which
#' is +/- 2 MAD away from median. 
#' 
#' graph displays the proportion of mappings per code 
#' that are outliers.
#' 
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`
#' 
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' @param mad_dev an integer to define the deviation that should be used to compute the upper and lower MAD limits
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' 
#' @return a heatmap that shows the proportion of mappings for each code that are unstable across
#'         time, meaning they frequently deviate from the all site centroid
#' 
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
  
  mad2 <- mad %>% left_join(process_output %>% distinct(site, !!sym(col), !!sym(denom)))
  
  if(is.null(vocab_tbl)){
    
    final <- mad2 %>% mutate(denom_fmt = format(!!sym(denom), big.mark = ','),
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
    
    final <- join_to_vocabulary(tbl = mad2,
                                vocab_tbl = vocab_tbl,
                                col = col) %>%
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
#' @param process_output dataframe output by `scv_process`
#' @param code_type type of code to be used in analysis -- either `source` or `cdm`
#' 
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#' 
#' @return a C control chart that highlights points in time where the number of mappings for
#'         a particular code are anomalous; outlying points are highlighted red
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
