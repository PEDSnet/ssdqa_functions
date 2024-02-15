

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
                          facet,
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
    
    # Summary Reference Table
    ref_tbl <- generate_ref_table(tbl = final,
                                  col = col,
                                  denom = denom,
                                  vocab_tbl = vocab_tbl)
    
  }else{
    
    final_filt <- ref %>%
      inner_join(nmap_top) %>%
      left_join(nmap_total) %>% 
      mutate(concept_id = as.integer(concept_id))
    
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
      scale_fill_gradient2(low = 'pink', high = 'maroon') + 
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
                                  col = map_col,
                                  denom = denom,
                                  vocab_tbl = vocab_tbl)
  }
  
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
scv_ss_anom_at <- function(process_output,
                           vocab_tbl=vocabulary_tbl('concept'),
                           variable_name,
                           facet=NULL){
  
  # if(code_type == 'source'){
  #   col <- 'source_concept_id'
  # }else if(code_type == 'cdm'){
  #   col <- 'concept_id'}
  
  facet <- facet %>% append('variable')
  
  n_mappings_yr <- process_output %>% filter(variable == 'ibd') %>% 
    group_by(!!!syms(facet), time_start) %>%
    summarise(n_mappings = n_distinct(concept_id))
  
  top_six <- 
    process_output %>% filter(variable == variable_name) %>% 
    ungroup() %>% 
    group_by(variable, concept_id) %>% 
    summarise(total_ct = sum(ct_concept)) %>% 
    ungroup() %>% arrange(desc(total_ct)) %>% 
    top_n(n=6) %>% select(concept_id) %>% pull()
  
  c_added <- join_to_vocabulary(tbl = process_output %>% filter(variable == 'ibd') %>% 
                                  filter(concept_id %in% top_six) %>% 
                                  mutate(concept_id=as.integer(concept_id)),
                              vocab_tbl = vocab_tbl,
                              col = 'concept_id') 
  
  c_added %>% 
    #group_by(!!!syms(facet)) %>%
    group_by(concept_id) %>% 
    group_modify(
      ~spc_calculate(
        data = .x, 
        x = time_start,
        y = prop_concept,
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
