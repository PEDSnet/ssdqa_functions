

#' *Single Site, Exploratory, No Time*
#' 
#' 
#' @param process_output 
#' @param code_type 
#' @param facet 
#' @param vocab_tbl 
#' @param num_codes 
#' @param num_mappings
#' 
#' @return 
#' 
csd_ss_exp_nt <- function(process_output,
                          facet = NULL,
                          vocab_tbl = vocabulary_tbl('concept'),
                          num_codes = 10,
                          num_mappings = 10){
  
  # picking columns / titles 
    denom <-  'ct_denom'
    col <- 'variable'
    map_col <- 'concept_id'
    prop <- 'prop_concept'
    title <- paste0('Top ', num_mappings, ' Concepts For ', num_codes, ' Top Variables')
  
  
  ## filter output down to most common codes, selecting a user-provided number
  topcodes <- process_output %>%
    ungroup() %>%
    group_by(!! sym(col)) %>% 
    select(col, denom, all_of(facet)) %>%
    distinct() %>%
    summarise(total_sum = sum(!! sym(denom))) %>% 
    #group_by(!!! syms(facet)) %>%
    #arrange(desc(!! sym(denom))) %>%
    arrange(desc(total_sum)) %>% 
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
  
  final <- 
    ref %>% 
    inner_join(nmap_top) %>% 
    left_join(nmap_total) %>% 
    mutate(xaxis = paste0(!!sym(col), '\n Total Mappings: ', nmap))
  
  facet <- facet %>% append('xaxis')
  
  if(! is.null(vocab_tbl)) {
    final_filt <- join_to_vocabulary(tbl = final %>% mutate(concept_id = as.integer(concept_id)),
                                    vocab_tbl = vocab_tbl,
                                    col = map_col) 
    plot <- final_filt %>% ggplot(aes(x = xaxis, y = as.character(!!sym(map_col)), 
                                 fill = !!sym(prop))) +
      geom_tile_interactive(aes(tooltip = concept_name)) +
      geom_text(aes(label = !!sym(prop)), size = 2, color = 'black') +
      scale_fill_gradient2(low = 'pink', high = 'maroon') + 
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_blank()) +
      labs(title = title,
           x = col,
           y = map_col)
    
    p <- girafe(ggobj = plot,
                width = 10,
                height = 10)
  } else {
    final_filt <- final
    
    p <- final %>% 
      filter(!!sym(prop) > 0.02) %>% ggplot(aes(x = xaxis, y = !!sym(map_col), 
                                                fill = !!sym(prop))) + 
      geom_tile() + 
      geom_text(aes(label = !!sym(prop)), size = 2, color = 'black') +
      scale_fill_gradient2(low = 'pink', high = 'maroon') + 
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_blank()) +
      labs(title = title,
           x = col,
           y = map_col) +
      theme_bw()
    
  }
  
  ref_tbl <- generate_ref_table(tbl = final %>% mutate(concept_id = as.integer(concept_id)),
                                col = col,
                                denom = denom,
                                vocab_tbl = NULL)
  
  output <- list(p, ref_tbl)
  
  return(output)
}


#' *Single Site, Anomaly, No Time*
#' 
#' 
#' @param process_output 
#'--- @param facet 
#' @param vocab_tbl 
#' @param num_codes 
#' @param num_mappings
#' 
#' @return 
#' 
csd_ss_anom_nt <- function(process_output,
                          #facet,
                          num_concept_combined = FALSE,
                          vocab_tbl = vocabulary_tbl('concept'),
                          num_codes = 10,
                          num_mappings = 10,
                          filtered_var = 'general_jia'){
  
  firstcolnames <- join_to_vocabulary(tbl = tbl_input,
                                      vocab_tbl = vocab_tbl,
                                      col = 'concept1') %>% 
    rename(conceptname1=concept_name) %>% select(concept1, conceptname1)
  
  secondcolnames <- join_to_vocabulary(tbl = tbl_input,
                                       vocab_tbl = vocab_tbl,
                                       col = 'concept2') %>% 
    rename(conceptname2=concept_name) %>% select(concept2, conceptname2)
  
  final <- 
    process_output %>% 
    left_join(firstcolnames) %>% 
    left_join(secondcolnames) %>% distinct()
    
    plot <- final %>% filter(variable==filtered_var) %>% filter(above_sd == TRUE) %>% 
      ggplot(aes(x = as.character(concept1), y = as.character(concept2), 
             fill = jaccard_index)) + 
      geom_tile_interactive(aes(tooltip = paste0('concept1 = ',conceptname1, '; n= ',concept1_ct,'\n','concept2 = ',conceptname2,'; n= ',concept2_ct,
                                                 '\n', 'co-occurrence = ', cocount,
                                                 '\n','jaccard sim = ',jaccard_index,
                                                 '\n', 'mean = ',var_jaccard_mean,'\n','sd = ', var_jaccard_sd))) + 
      scale_fill_gradient2(low = 'pink', high = 'maroon') + 
       labs(title = filtered_var,
            x = 'concept1',
            y = 'concept2') +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) 
    
    p <- girafe(ggobj=plot,
                width=10,
                height=10)
  
  ## filter output down to most common codes, selecting a user-provided number
 
  
  return(p)
}

### ACROSS TIME

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
csd_ss_anom_at <- function(process_output,
                           vocab_tbl=vocabulary_tbl('concept'),
                           variable_name='ibd',
                           facet=NULL,
                           top_mapping_n = 6){
  
  # if(code_type == 'source'){
  #   col <- 'source_concept_id'
  # }else if(code_type == 'cdm'){
  #   col <- 'concept_id'}
  
  facet <- facet %>% append('concept_id')
  
  n_mappings_yr <- process_output %>% filter(variable == variable_name) %>% 
    group_by(!!!syms(facet), time_start) %>%
    summarise(n_mappings = n_distinct(concept_id))
  
  top_n <- 
    process_output %>% filter(variable == variable_name) %>% 
    ungroup() %>% 
    group_by(variable, concept_id) %>% 
    summarise(total_ct = sum(ct_concept)) %>% 
    ungroup() %>% arrange(desc(total_ct)) %>% 
    top_n(n=top_mapping_n) %>% select(concept_id) %>% pull()
  
  c_added <- join_to_vocabulary(tbl = process_output %>% filter(variable == variable_name) %>% 
                                  filter(concept_id %in% top_n) %>% 
                                  mutate(concept_id=as.integer(concept_id)),
                              vocab_tbl = vocab_tbl,
                              col = 'concept_id') 
  
  c_plot <- 
    c_added %>% 
    #group_by(!!!syms(facet)) %>%
    group_by(concept_id) %>% 
    group_modify(
      ~spc_calculate(
        data = .x, 
        x = time_start,
        y = ct_concept,
        chart = "c"
      )
    ) %>% 
    ungroup() %>%
    # plot
    spc_plot(engine = "ggplot") + 
    facet_wrap((facet)) + 
    theme(panel.background = element_rect("white", "grey80")) +
    labs(title = 'Control Chart: Proportion of Code Usage Over Time')
  
  ref_tbl <- generate_ref_table(tbl = process_output %>% filter(variable == variable_name) %>% 
                                  filter(concept_id %in% top_n) %>% 
                                  mutate(concept_id=as.integer(concept_id)),
                                col = 'concept_id',
                                denom = 'ct_concept',
                                vocab_tbl = vocab_tbl,
                                time = TRUE)
  
  output <- list(c_plot, ref_tbl)
  
}


#' *Single Site, Exploratory, Across Time*
#' 
#' Facets by main code (cdm or source) by default, with each line representing
#' a mapping code. using plotly so the legend is interactive and codes can be isolated
#' 
#' 
#' @param process_output dataframe output by `scv_process`
#' 
#'                  should match the code_type provided when running `scv_process`
#' @param facet the variables by which you would like to facet the graph
#'  ### IF MULTI SITE --- then will facet by site. In the case of multi-site exploration, 
#'  the documentation should state that the exploratory should look at JUST one variable. 
#'  ### IF SINGLE SITE, then can include more than one variable and will facet by variable
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' 
#' @return a line graph with one facet per code displaying the proportion of mapped codes
#'         across the user selected time period
#' @return a reference table with total counts of each code across the entire user selected
#'         time period
#' 
csd_ss_ms_exp_at <- function(process_output,
                             variable_names = c('ibd','spondyloarthritis'),
                             multi_or_single_site = 'single',
                             vocab_tbl = vocabulary_tbl('concept')){
  
  # if(code_type == 'source'){
  #   col <- 'source_concept_id'
  #   map_col <- 'concept_id'
  #   prop <- 'source_prop'
  #   denom <- 'denom_source_ct'
  # }else if(code_type == 'cdm'){
  #   col <- 'concept_id'
  #   map_col <- 'source_concept_id'
  #   prop <- 'concept_prop'
  #   denom <- 'denom_concept_ct'
  # }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  if(multi_or_single_site=='multi'){
    facet <- facet %>% append('site')
  } else {
    facet <- facet %>% append('variable')
  }
  
  if(is.null(vocab_tbl)){
    
    p <- process_output %>% filter(variable %in% variable_names) %>% 
      ggplot(aes(y = prop_concept, x = time_start, color = concept_id)) +
      geom_line() +
      facet_wrap((facet)) +
      labs(title = 'Code Mapping Pairs Over Time')
    
    plot <- ggplotly(p)
    
    
  }else{
    
    process_output_plot <- join_to_vocabulary(tbl = process_output %>% 
                                                mutate(concept_id=as.integer(concept_id)),
                                              vocab_tbl = vocab_tbl,
                                              col = 'concept_id')
    
    p <-process_output_plot %>% filter(variable %in% variable_names)  %>%
      mutate(concept_id=as.character(concept_id)) %>% 
      ggplot(aes(y = prop_concept, x = time_start, color = concept_id),
             label1=concept_name,
             label2=ct) +
      geom_line() +
      facet_wrap((facet)) +
      labs(title = 'Code Mapping Pairs Over Time',
           color = 'concept_id')
    
    plot <- ggplotly(p)
    
  }
  
  ref_tbl <- generate_ref_table(tbl = process_output %>% 
                                  mutate(concept_id=as.integer(concept_id)) %>% 
                                  group_by(site),
                                col = 'concept_id',
                                denom = 'ct_concept',
                                vocab_tbl = vocab_tbl,
                                time = TRUE)
  
  output <- list(plot, ref_tbl)
  
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
csd_ms_exp_nt <- function(process_output,
                          facet,
                          vocab_tbl = vocabulary_tbl('concept'),
                          num_codes = 10){
  
  # picking columns / titles 

    denom <-  'ct_denom'
    col <- 'variable'
    map_col <- 'concept_id'
    prop <- 'prop_concept'
    ct = 'ct_concept'
  
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
  
  final_filt <- process_output %>% 
    inner_join(topcodes)
  
  if(is.null(vocab_tbl)) {
    final <- final_filt
  } else {
    final <- join_to_vocabulary(tbl = final_filt %>% mutate(concept_id=as.integer(concept_id)),
                                vocab_tbl = vocab_tbl,
                                col = map_col)
  }
  
  table <- 
    final %>%
    ungroup() %>%
    select(-denom) %>%
    mutate(pct = !!sym(prop)) %>%
    arrange(!!!syms(facet), desc(ct)) %>%
    relocate(site) %>% 
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


#' *Multi-Site Anomaly No Time*
#' 
#' @param process_output output from csd_process
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#'                  
#'                  

csd_ms_anom_nt<-function(process_output,
                        vocab_tbl,
                        text_wrapping_char=80,
                        filtered_var='general_jia',
                        comparison_col='prop_concept',
                        grouped_vars=c('variable','concept_id')){
  
  mean_tbl <- 
    compute_dist_mean_conc(tbl=process_output,
                           grp_vars = grouped_vars,
                           var_col = comparison_col,
                           num_sd=2,
                           num_mad=2)
  
  if(is.null(vocab_tbl)) {concept_label='concept_id'
                          data_tbl=mean_tbl} else {
                            data_tbl = join_to_vocabulary(tbl = mean_tbl %>% mutate(concept_id=as.integer(concept_id)),
                                                          vocab_tbl = vocab_tbl,
                                                          col = 'concept_id')
                              concept_label='concept_name'}
  
  comparison_col = comparison_col
  
  dat_to_plot <- data_tbl %>% filter(variable == filtered_var) %>% 
    mutate(text=paste("Concept: ",!!sym(concept_label),
                      "\nSite: ",site,
                      "\nProportion: ",round(!!sym(comparison_col),2),
                      "\nMean proportion:",round(mean,2),
                      "\nMedian proportion: ",round(median,2),
                      "\nSD: ", round(sd,4)))
  
  
  mid<-(max(dat_to_plot$abs_diff_mean,na.rm=TRUE)+min(dat_to_plot$abs_diff_mean,na.rm=TRUE))/2
  
  plt<-ggplot(dat_to_plot %>% filter(variable == filtered_var), aes(x=site, y=!!sym(concept_label), text=text))+
    geom_point(aes(size=!!sym(comparison_col),colour=abs_diff_mean,shape=anomaly_yn))+
    #aes(stringr::str_wrap(!!sym(concept_label), 20)) +
    scale_shape_manual(values=c(20,8))+
    scale_color_gradient2(midpoint=mid,low='#8c510a',mid='#f5f5f5', high='#01665e')+
    scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
    theme_bw()+
    labs(colour="Difference\nfrom mean",
         shape="Anomaly",
         size="")+
    theme(axis.text.x = element_text(angle=90)) 
  
  ggplotly(plt, tooltip="text")
}

