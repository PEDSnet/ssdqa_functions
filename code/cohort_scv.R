
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
      
  }else{
    
    domain_tbl <- cohort %>%
      inner_join(cdm_tbl(code_domain))
    }
  
  fact_tbl <- 
    domain_tbl %>% 
    inner_join(concept_set,
               by=setNames('concept_id',final_col)) %>% 
    select(all_of(group_vars(cohort)),
           all_of(concept_col),
           all_of(source_col)) %>% 
    rename('concept_id' = concept_col,
           'source_concept_id' = source_col)
  
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
                        time_span = c('2012-01-01', '2020-01-01')
                        ){
  
  # Set up grouped list
  
  grouped_list <- c('site', 'domain')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  
  # Prep cohort
  
  cohort_prep <- prepare_pf(cohort_tbl = cohort, age_groups = age_groups, codeset = NULL) %>% 
    mutate(domain = code_domain) %>% 
    group_by(!!! syms(grouped_list))
  
  site_output <- list()
  
  #' HR COMMENT: I think we should do a check in here --
  #' if single site, then we combine the data into 
  #' one site and replace the different site names 
  #' with something generic like `combined`
  
  if(! time) {
    
    for(k in 1:length(site_list)) {
      
      site_list_thisrnd <- site_list[[k]]
      
      # filters by site
      cohort_site <- cohort_prep %>% filter(site%in%c(site_list_thisrnd))
      
      domain_compute <- check_code_dist(cohort = cohort_site,
                                        code_type = code_type,
                                        code_domain = code_domain,
                                        concept_set = concept_set,
                                        domain_tbl = domain_tbl) 
      
      site_output[[k]] <- domain_compute
      
      ### HR COMMENT: WHAT IS THE PURPOSE OF THIS STEP IF WE REDUCE IN SCV_TBL
      all_site <- reduce(.x=site_output,
                         .f=dplyr::union) 
      
    }
    
    scv_tbl <- reduce(.x=site_output,
                      .f=dplyr::union)
  
  } else if(time){
    
    #' HR COMMENT: I think we need to also filter by the `time_period` parameter here
    #' and add it to the main function parameter. We can default it
    #' to a year, but we need the user to be able to change it if they want. 
    scv_tbl <- compute_fot_scv(cohort = cohort_prep,
                               site_list = site_list,
                               code_type = code_type,
                               code_domain = code_domain,
                               concept_set = concept_set,
                               time_span = time_span,
                               domain_tbl = domain_tbl)
    
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

scv_ss_exp_nt <- function(scv_process,
                          output,
                          facet,
                          num_codes = 10){
  
  ## HR COMMENT: SHOULD WE AUTOMATE THESE CHOICES?
  # picking columns / titles 
  if(output == 'concept_prop'){
    denom <-  'denom_concept_ct'
    col <- 'concept_id'
    map_col <- 'source_concept_id'
    title <- paste0('Top 5 Mappings for Top ', num_codes, ' CDM Codes')
  }else if(output == 'source_prop'){
    denom <- 'denom_source_ct'
    col <- 'source_concept_id'
    map_col <- 'concept_id'
    title <- paste0('Top 5 Mappings for Top ', num_codes, ' Source Codes')
  }else{stop('Please select a valid output')}
  
  
  ## filter output down to most common codes, selecting a user-provided number
  ## HR COMMENT: CHANGE FILTER FROM THE OBJECT NAME HERE
  ## HR COMMENT: CAN REMOVE FACET IF IT IS NULL
  if(!is.null(facet)){
    filter <- scv_process %>%
      ungroup() %>%
      select(col, denom, all_of(facet)) %>%
      distinct() %>%
      group_by(!!! syms(facet)) %>%
      arrange(desc(!! sym(denom))) %>%
      slice(1:num_codes)
  }else{
    filter <- scv_process %>%
      ungroup() %>%
      select(col, denom) %>%
      distinct() %>%
      arrange(desc(!! sym(denom))) %>%
      slice(1:num_codes)
  }
  
  final <- scv_process %>% 
    inner_join(filter)
  
  #' HR COMMENT: I ungrouped here because `final` has some grouped variables, 
  #' and it doesn't seem like new groupings are being *added* here.
  graph <- final %>% ungroup() %>% 
    select(concept_id, source_concept_id, output, all_of(facet)) %>%
    group_by(!!sym(col), !!!syms(facet)) %>%
    arrange(desc(!!sym(output))) %>%
    slice(1:5)
  
  ## new graph: shows top 5 mappings for however many codes are selected by the user; definitely
  ## much easier to look at when the number of mappings gets high
  plot <- graph %>% ggplot(aes(x = as.character(!!sym(col)), y = as.character(!!sym(map_col)), 
                               fill = !!sym(output))) + 
    geom_tile() + 
    geom_text(aes(label = !!sym(output)), size = 2, color = 'black') +
    scale_fill_gradient2(low = 'pink', high = 'maroon') + 
    facet_wrap((facet %>% append(col)), scales = 'free') +
    theme(axis.text.x = element_blank()) +
    labs(title = title,
         x = col,
         y = map_col)
  
  ## option 1 for table: more of a standard html table, can mess around with colors, 
  ## probably easier to save as an object
  
  #' HR COMMENT: I got an error when trying to run this, as 
  #' I think the function `tab_header` is part of a package that was not downloaded 
  #' to execute. That package should go in the `driver` file.
  table <- final %>%
    ungroup() %>%
    select(site, all_of(facet), source_concept_id, concept_id, ct, output) %>%
    mutate(pct = !!sym(output)) %>%
    arrange(site, !!!syms(facet), desc(ct)) %>%
    gt::gt(groupname_col = col) %>%
    gt_plt_bar_pct(column = pct) %>%
    fmt_number(columns = ct, decimals = 0) %>%
    fmt_percent(columns = output, decimals = 0) %>%
    data_color(palette = "Dark2", columns = c(site, all_of(facet))) %>%
    tab_options(row_group.background.color = 'linen',
                row_group.font.weight = 'bold',
                container.height = '750px',
                container.overflow.y = TRUE) %>%
    tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Codes')) 
  
  ## option 2: interactive, compresses the table into accordion sections which makes it a little
  ## easier to read, not sure how well it will save, not as pretty as the other one lol
  
  #' HR COMMENT: Also received an error for this one, 
  #' this time with the function `reactablefmtr`
  table_v2 <- final %>%
    ungroup() %>%
    select(site, all_of(facet), source_concept_id, concept_id, ct, output) %>%
    mutate(pct = !!sym(output)) %>%
    select(-!!sym(output)) %>%
    reactable::reactable(groupBy = c(col, 'site'),
                         bordered = TRUE,
                         striped = TRUE,
                         theme = reactableTheme(
                           cellPadding = "8px 12px",
                           style = list(
                             fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                           )),
                         columns = list(site = colDef(aggregate = "unique"),
                                        pct = colDef(cell = data_bars(., fill_color = '#12047d', 
                                                                      bar_height = 30,
                                                                      number_fmt = scales::percent,
                                                                      text_position = 'above')),
                                        ct = colDef(format = colFormat(separators = TRUE))),
                         pagination = FALSE) %>% 
    reactablefmtr::add_title(paste0('All Available Mappings for Top ', num_codes, ' Codes'))
  
  ## will pick one of the table options for final version of the function
  output <- list(plot, table, table_v2)
  
  return(output)
  
}


#' 
#' *Single Site, Anomaly, No Time*
#' 
#' Bar graph displaying the number of mappings per code
#' filters down to mapping counts that fall above the median or below Q1 (keep it like this?)
#' 
#' Q1 median and Q3 shown on graph
#' 

scv_ss_anom_nt <- function(scv_process,
                           output,
                           facet){
  
  if(output == 'source_prop'){
    col <- 'source_concept_id'
  }else if(output == 'concept_prop'){
    col <- 'concept_id'
  }else{stop('Please select a valid output')}
  
  mappings_per_code <- scv_process %>%
    group_by(!!!syms(facet), !!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    mutate(median = median(n_mappings),
           q1 = quantile(n_mappings, 0.25),
           q3 = quantile(n_mappings, 0.75))
  
  #' HR COMMENT: The output here is a little confusing. 
  #' The number of mappings all seem to exceed the median in 
  #' both the graph outputs. I think we should review
  #' the output of this graph - there might be a better way 
  #' to do this - maybe number of mappings in the bottom and top quartiles
  #' and just a graph with the top 5 and bottom 5, and then paired with a table
  #' that lists the rest. 
  plot <- mappings_per_code %>%
    filter(n_mappings > median | n_mappings < q1) %>%
    ggplot(aes(x = as.character(!!sym(col)), y = n_mappings, color = as.character(!!sym(col)))) +
    geom_col() +
    geom_hline(aes(yintercept = median)) +
    geom_hline(aes(yintercept = q1), linetype = 'dotted') +
    geom_hline(aes(yintercept = q3), linetype = 'dotted') +
    facet_wrap((facet), scales = 'free') +
    theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, vjust = 1),
          legend.position = 'none') +
    labs(title = 'Codes with Anomalous Number of Unique Mappings')
  
  return(plot)
  
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

scv_ms_anom_nt <- function(scv_process,
                           output,
                           facet,
                           mad_dev = 2){
  
  #' HR COMMENT: In the single-site graphs, the output
  #' is `source_prop` or `concept_prop`... why 
  #' is it changed here? It doesn't matter which we keep, 
  #' but we should try to be consistent. 
  if(output == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
  }else if(output == 'cdm'){
    col <- 'concept_id'
    map_col <- 'source_concept_id'
  }else{stop('Please select a valid output')}
  
  mappings_total <- scv_process %>%
    group_by(!!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    mutate(median = median(n_mappings)) %>%
    select(col, median) %>% ungroup()
  
  mappings_group <- scv_process %>%
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
  #' 3: Should we do n > median as well as n < median? Should we have that 
  #' as a potential option?
  plot <- mappings_group %>%
    filter(n_mappings > median) %>%
    ggplot(aes(y = as.character(!!sym(col)), x = site, fill = n_mad)) +
    geom_tile() +
    facet_wrap((facet), scales = 'free', ncol = 1) +
    #theme(axis.text.y = element_text(size = 4)) +
    labs(title = 'MAD from Median Number of Mappings per Code',
         subtitle = 'Filtered to codes where n > median')
  
  return(plot)
  
}
