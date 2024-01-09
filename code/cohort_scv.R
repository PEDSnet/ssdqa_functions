
### base function skeleton (will eventually go into loop_through_visits)
#' code_type either source or CDM to decide where theyre starting
#' 
#' the domain_tbl csv has info about which columns are the source_concept vs concept
#' 
#' code domain tells the function which line of the csv to use as reference
#' 
#' need to figure out how to clearly differentiate between the concept set param and
#' the codeset utilization param
#' 


check_code_dist <- function(cohort,
                            concept_set,
                            code_type,
                            code_domain,
                            time = FALSE,
                            domain_tbl = read_codeset('scv_domains', 'cccc')){
  
  # pick the right domain/columns
  domain_filter <- domain_tbl %>% filter(domain == code_domain)
  concept_col <- domain_filter$concept_col
  source_col <- domain_filter$source_col
  
  if(code_type=='source') {
     final_col = source_col
  }else if(code_type == 'cdm'){
    final_col = concept_col
  }else{stop(paste0(code_type, ' is not a valid argument. Please select either "source" or "cdm"'))}
  
  if(time){
    
    domain_tbl <- cohort %>%
      inner_join(cdm_tbl(code_domain)) %>%
      filter(!!sym(domain_filter$date_col) >= start_date,
             !!sym(domain_filter$date_col) <= end_date)
    
    
    fact_tbl <- 
      domain_tbl %>% 
      inner_join(concept_set,
                 by=setNames('concept_id',final_col)) %>% 
      select(all_of(group_vars(cohort)),
             all_of(concept_col),
             all_of(source_col),
             time_start,
             time_increment) %>% 
      rename('concept_id' = concept_col,
             'source_concept_id' = source_col) %>%
      group_by(time_start, time_increment, .add = TRUE)
      
  }else{
    
    domain_tbl <- cohort %>%
      inner_join(cdm_tbl(code_domain))
    
    
    fact_tbl <- 
      domain_tbl %>% 
      inner_join(concept_set,
                 by=setNames('concept_id',final_col)) %>% 
      select(all_of(group_vars(cohort)),
             all_of(concept_col),
             all_of(source_col)) %>% 
      rename('concept_id' = concept_col,
             'source_concept_id' = source_col)
    
    }
  
  grouped_output <- 
    fact_tbl %>% 
    group_by(
      concept_id,
      source_concept_id, 
      .add = TRUE
    ) %>% summarise(ct=n()) %>% 
    compute_new()
  
  
  denom_concepts <- 
    fact_tbl %>% 
    group_by(
      concept_id,
      .add = TRUE
    ) %>% summarise(denom_concept_ct=n()) %>% 
    compute_new()
  
  denom_source <- 
    fact_tbl %>% 
    group_by(
      source_concept_id,
      .add = TRUE
    ) %>% summarise(denom_source_ct=n()) %>% 
  compute_new()
  
  grouped_output_totals <- 
    grouped_output %>% left_join(denom_concepts) %>% 
    left_join(denom_source) %>% collect() %>% 
    mutate(concept_prop = round(ct/denom_concept_ct, 2),
           source_prop = round(ct/denom_source_ct,2)) 
    
  
  
}

#' putting it all together
#' HR QUESTION --- if building a single site test, will we allow for more than 
#' one site to be built into the parameter site_list()?

scv_process <- function(cohort = cohort,
                        site_list = c('seattle','cchmc'),
                        domain_tbl=read_codeset('scv_domains', 'cccc'),
                        concept_set = c(45567437, 45553188),
                          #dplyr::union(load_codeset('jia_codes','iccccc'),
                                                  # load_codeset('jia_codes_icd','iccccc')) 
                        code_type = 'source',
                        code_domain = 'condition_occurrence',
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = FALSE, #read_codeset('age_group_definitions'),
                        time = TRUE,
                        time_span = c('2012-01-01', '2020-01-01'),
                        time_period = 'year'
                        ){
  
  # Add site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site,
                                 site_list = site_list)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj
  
  # Set up grouped list
  
  grouped_list <- grouped_list %>% append('domain')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  
  site_output <- list()
  
  # Prep cohort
  
  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups, codeset = NULL) %>% 
    mutate(domain = code_domain) %>% 
    group_by(!!! syms(grouped_list))

  # Execute function
  if(! time) {
    
    for(k in 1:length(site_list_adj)) {
      
      site_list_thisrnd <- site_list_adj[[k]]
      
      # filters by site
      cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))
      
      domain_compute <- check_code_dist(cohort = cohort_site,
                                        code_type = code_type,
                                        code_domain = code_domain,
                                        concept_set = concept_set,
                                        domain_tbl = domain_tbl) 
      
      site_output[[k]] <- domain_compute
      
    }
    
    scv_tbl <- reduce(.x=site_output,
                      .f=dplyr::union)
  
  } else if(time){
    ## Do we need a loop here? works because it groups by site as a default, not sure if
    ## its necessary (which one is faster/more efficient)
    if(!is.vector(concept_set)){stop('For an over time output, please select 1-5 codes from your
                                   concept set and include them as a vector in the concept_set argument.')}
    if(is.vector(concept_set) && length(concept_set) > 5){stop('For an over time output, please select 1-5 
                                                              codes from your concept set and include them as
                                                             a vector in the concept_set argument.')}
    
    concept_set_prep <- as.data.frame(concept_set) %>% rename('concept_id' = concept_set) %>%
      mutate(concept_id = as.integer(concept_id))
    concept_set_prep <- copy_to_new(df = concept_set_prep)
    
    scv_tbl <- compute_fot(cohort = cohort_prep,
                           site_list = site_list_adj,
                           time_span = time_span,
                           time_period = time_period,
                           reduce_id = NULL,
                           check_func = function(dat){
                                 check_code_dist(cohort = dat,
                                                 concept_set = concept_set_prep,
                                                 code_type = code_type,
                                                 code_domain = code_domain,
                                                 domain_tbl = domain_tbl,
                                                 time = TRUE)
                               })
    
  }
  
  
  return(scv_tbl)
  
 
}





#' OUTPUT GENERATION 
#' 
#' 
#' 


#' HR COMMENTS: I reviewed the output, and I like the 
#' option 1 table better for the multi-site analysis, 
#' and the heatmap best for single site, particularly 
#' if we are enforcing a single site for the single site 
#' exploratory. For all of them, wondering if we can somehow make
#' it easy to view the concept names with the concept id's
#' 


#' *Single Site, Exploratory, No Time*
#' 
#' Outputs a graph that shows the top 5 mappings per (user selected number of codes)
#' AND a table that summarizes all mappings for the (user selected number of codes)
#' 
#' 2 possible options for table - one interactive, one not, pros and cons to both so 
#' we'll just need to decide which is best
#' 
#' Use this same thing for multi site and just facet by site? or do we need another
#' visualization

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
    
    plot <- final %>% ggplot(aes(x = xaxis, y = as.character(!!sym(map_col)), 
                                 fill = !!sym(prop))) + 
      geom_tile() + 
      geom_text(aes(label = !!sym(prop)), size = 2, color = 'black') +
      scale_fill_gradient2(low = 'pink', high = 'maroon') + 
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_blank()) +
      labs(title = title,
           x = col,
           y = map_col)
    
    return(plot)
    
  }else{
    
    final_filt <- ref %>%
      inner_join(nmap_top) %>%
      left_join(nmap_total)
    
    final_db <- copy_to_new(df = final_filt)
    
    final <- final_db %>%
      rename('join_col' = map_col) %>%
      left_join(select(vocab_tbl, concept_id, concept_name), 
                by = c('join_col' = 'concept_id')) %>%
      mutate(xaxis = paste0(!!sym(col), '\n Total Mappings: ', nmap))
    
    
    facet <- facet %>% append('xaxis')
    
    ## ggiraph interactive
    plot <- final %>% ggplot(aes(x = xaxis, y = as.character(join_col), 
                                fill = !!sym(prop))) +
      geom_tile_interactive(aes(tooltip = concept_name)) +
      geom_text(aes(label = !!sym(prop)), size = 2, color = 'black') +
      scale_fill_gradient2(low = 'pink', high = 'maroon') + 
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_blank()) +
      labs(title = title,
           x = col,
           y = map_col)
    
    girafe(ggobj = plot,
           width = 10,
           height = 10)
    
    return(plot_int)
    
  }
}


#'
#' *Multi Site, Exploratory, No Time*
#' 
#' Table
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
      select(site, all_of(facet), source_concept_id, concept_id, ct, prop) %>%
      mutate(pct = !!sym(prop)) %>%
      arrange(site, !!!syms(facet), desc(ct)) %>%
      gt::gt(groupname_col = col) %>%
      gtExtras::gt_plt_bar_pct(column = pct) %>%
      fmt_number(columns = ct, decimals = 0) %>%
      fmt_percent(columns = prop, decimals = 0) %>%
      data_color(palette = "Dark2", columns = c(site, all_of(facet))) %>%
      tab_options(row_group.background.color = 'linen',
                  row_group.font.weight = 'bold',
                  container.height = '750px',
                  container.overflow.y = TRUE) %>%
      tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Codes')) 
  
    return(table)
  
  }else{
    
    final_filt <- process_output %>% 
      inner_join(topcodes) %>%
      rename('join_col' = map_col)
    
    final_db <- copy_to_new(df = final_filt)
    
    final <- final_db %>%
      left_join(select(vocab_tbl, concept_id, concept_name),
                by = c('join_col' = 'concept_id')) %>%
      collect_new()
    
    table <- final %>%
      ungroup() %>%
      select(site, all_of(facet), col, join_col, concept_name, ct, prop) %>%
      mutate(pct = !!sym(prop)) %>%
      arrange(site, !!!syms(facet), desc(ct)) %>%
      gt::gt(groupname_col = col) %>%
      gtExtras::gt_plt_bar_pct(column = pct) %>%
      fmt_number(columns = ct, decimals = 0) %>%
      fmt_percent(columns = prop, decimals = 0) %>%
      data_color(palette = "Dark2", columns = c(site, all_of(facet))) %>%
      tab_options(row_group.background.color = 'linen',
                  row_group.font.weight = 'bold',
                  container.height = '750px',
                  container.overflow.y = TRUE) %>%
      cols_label(join_col = map_col) %>%
      tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Codes')) 
    
    return(table)
  }
  
}


#' 
#' *Single Site, Anomaly, No Time*
#' 
#' Bar graph displaying the number of mappings per code
#' filters down to mapping counts that fall above the median or below Q1 (keep it like this?)
#' 
#' Q1 median and Q3 shown on graph
#' 

scv_ss_anom_nt <- function(process_output,
                           code_type,
                           facet,
                           vocab_tbl = vocabulary_tbl('concept'),
                           rel_to_median = 'greater'){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  mappings_per_code <- process_output %>%
    group_by(!!!syms(facet), !!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    mutate(median = median(n_mappings),
           q1 = quantile(n_mappings, 0.25),
           q3 = quantile(n_mappings, 0.75))
  
  if(rel_to_median == 'greater'){
    tbl_filt <- mappings_per_code %>%
      filter(n_mappings > median)
  }else if(rel_to_median == 'less'){
    tbl_filt <- mappings_per_code %>%
      filter(n_mappings < median)
  }else(stop('Invalid selection for rel_to_median: please select `greater` or `less`'))
  
  if(is.null(vocab_tbl)){
    tbl <- tbl_filt
    
    plot <- tbl %>%
      ggplot(aes(x = as.character(join_col), y = n_mappings, fill = as.character(join_col))) +
      geom_col() +
      geom_hline(aes(yintercept = median)) +
      geom_hline(aes(yintercept = q1), linetype = 'dotted') +
      geom_hline(aes(yintercept = q3), linetype = 'dotted') +
      facet_wrap((facet), scales = 'free') +
      theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, vjust = 1),
            legend.position = 'none') +
      labs(title = 'Codes with Anomalous Number of Unique Mappings',
           x = col)
  }else{
    
    tbl_db <- copy_to_new(df = tbl_filt)
    
    tbl <- tbl_db %>%
      rename('join_col' = col) %>%
      left_join(select(vocab_tbl, concept_id, concept_name), 
                by = c('join_col' = 'concept_id'))
    
    plot <- tbl %>%
      ggplot(aes(x = as.character(join_col), y = n_mappings, fill = as.character(join_col))) +
      geom_col_interactive(aes(tooltip = concept_name)) +
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
#' heat map that displays codes where the number of mappings per code is > than the TOTAL
#' median number of mappings
#' (filtered the mappings to avoid crowded axes)
#' 
#' shows the number of MAD away from the TOTAL median for each code and site 
#' (i.e. not the MAD away from site specific median)
#' 

scv_ms_anom_nt <- function(process_output,
                           code_type,
                           facet,
                           rel_to_median = 'greater',
                           mad_dev = 2){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
  }else if(code_type == 'cdm'){
    col <- 'concept_id'
    map_col <- 'source_concept_id'
  }else{stop('Please select a valid code_type - `source` or `cdm`')}
  
  mappings_total <- process_output %>%
    group_by(!!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    mutate(median = median(n_mappings)) %>%
    select(col, median) %>% ungroup()
  
  mappings_group <- process_output %>%
    group_by(!!!syms(facet), !!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    inner_join(mappings_total) %>%
    distinct() %>%
    mutate(mad = mad(n_mappings, center = median)) %>%
    ungroup() %>%
    mutate(dist_median = abs(n_mappings - median),
           n_mad = dist_median/mad)
  
  #' HR COMMENTS: This works well, but a couple of questions:
  #' 1: Is it possible to have a list of the codes pop up if you hover your 
  #' mouse over it? 
  #' 2: I know this is nitpicky, but is it possible to have multiple 
  #' gradients in the heatmap? The blue just makes it kind of hard to 
  #' see the differences unless they are really big
  
  if(rel_to_median == 'greater'){
    tbl_filt <- mappings_group %>%
      filter(n_mappings > median)
  }else if(rel_to_median == 'less'){
    tbl_filt <- mappings_group %>%
      filter(n_mappings < median)
  }else(stop('Invalid selection for rel_to_median: please select `greater` or `less`'))
  
  if(is.null(vocab_tbl)){
    
    tbl <- tbl_filt
    
    plot <- tbl %>%
      ggplot(aes(y = as.character(!!sym(col)), x = site, fill = n_mad)) +
      geom_tile() +
      facet_wrap((facet), scales = 'free', ncol = 1) +
      scale_fill_viridis_c(option = 'turbo') +
      labs(title = 'MAD from Median Number of Mappings per Code',
           y = col)
    
    return(plot)
    
  }else{
    
    tbl_db <- copy_to_new(df = tbl_filt)
  
    tbl <- tbl_db %>%
      rename('join_col' = col) %>%
      left_join(select(vocab_tbl, concept_id, concept_name), 
                by = c('join_col' = 'concept_id'))
    
    plot <- tbl %>%
      ggplot(aes(y = as.character(join_col), x = site, fill = n_mad)) +
      geom_tile_interactive(aes(tooltip = concept_name)) +
      facet_wrap((facet), scales = 'free', ncol = 1) +
      #scale_fill_gradientn(colors = viridis::turbo(10))
      scale_fill_viridis_c(option = 'turbo') +
      labs(title = 'MAD from Median Number of Mappings per Code',
           y = col)
    
    girafe(ggobj = plot,
           width = 10,
           height = 10)
    
  }
  
}






scv_output <- function(process_output,
                       output_function,
                       code_type,
                       facet,
                       num_codes = 10,
                       num_mappings = 25,
                       rel_to_median = 'greater',
                       mad_dev = 2,
                       vocab_tbl = vocabulary_tbl('concept'),
                       save_as_png = FALSE,
                       file_path = NULL){
  
  ## Run output functions
  if(output_function == 'scv_ms_anom_nt'){
    scv_output <- scv_ms_anom_nt(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 rel_to_median = rel_to_median,
                                 mad_dev = mad_dev)
  }else if(output_function == 'scv_ss_anom_nt'){
    scv_output <- scv_ss_anom_nt(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 rel_to_median = rel_to_median,
                                 vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ms_exp_nt'){
    scv_output <- scv_ms_exp_nt(process_output = process_output,
                                code_type = code_type,
                                facet = facet,
                                num_codes = num_codes,
                                vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ss_exp_nt'){
    scv_output <- scv_ss_exp_nt(process_output = process_output,
                                code_type = code_type,
                                facet = facet,
                                num_codes = num_codes,
                                num_mappings = num_mappings,
                                vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ms_anom_at'){
    scv_output <- scv_ms_anom_at(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet,
                                 mad_dev = mad_dev)
  }else if(output_function == 'scv_ss_anom_at'){
    scv_output <- scv_ss_anom_at(process_output = process_output,
                                 code_type = code_type,
                                 facet = facet)
  }else if(output_function == 'scv_ms_exp_at'){
    scv_output <- scv_ss_ms_exp_at(process_output = process_output,
                                   code_type = code_type,
                                   facet = facet,
                                   vocab_tbl = vocab_tbl)
  }else if(output_function == 'scv_ss_exp_at'){
    scv_output <- scv_ss_ms_exp_at(process_output = process_output,
                                   code_type = code_type,
                                   facet = facet,
                                   vocab_tbl = vocab_tbl)
  }
  
  return(scv_output)
  
}
