
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
  } else {final_col = concept_col}
  
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



### output generation skeleton

#' use the right denominator to sort the output and find the most common codes for filtering purposes
#' sort the output by the most commonly occurring codes; group where needed
#' 
#' thinking about options for output:
#' -- snomed --> icd as a lot of codes, even when excluding those that don't appear in the data,
#'    so its a little better to facet by snomed code (still a little messy for some codes)
#' -- icd --> snomed is easier to read without facetting, and facetting may make the graph a little
#'    sparse looking
#' -- how does this look in other source <--> concept mappings

ss_exp_nt <- function(process_output,
                      output,
                      facet,
                      num_codes = 10){
  
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
  }
  
  # sorting output to select the most commonly occurring codes and using those in the output
  # is this the best way to filter down the output?
    if(!is.null(facet)){
      filter <- process_output %>%
        ungroup() %>%
        select(col, denom, all_of(facet)) %>%
        distinct() %>%
        group_by(!!! syms(facet)) %>%
        arrange(desc(!! sym(denom))) %>%
        slice(1:num_codes)
    }else{
      filter <- process_output %>%
        ungroup() %>%
        select(col, denom) %>%
        distinct() %>%
        arrange(desc(!! sym(denom))) %>%
        slice(1:num_codes)
    }
    
    final <- process_output %>% 
      inner_join(filter)
    
    graph <- final %>% 
      select(concept_id, source_concept_id, output, all_of(facet)) %>%
      group_by(!!sym(col), !!!syms(facet)) %>%
      arrange(desc(!!sym(output))) %>%
      slice(1:5)
  
  # option 1: heatmap, option to facet by group, not legible for snomed --> icd mappings
  plot <- graph %>% ggplot(aes(x = as.character(!!sym(col)), y = as.character(!!sym(map_col)), 
                               fill = !!sym(output))) + 
    geom_tile() + 
    geom_text(aes(label = !!sym(output)), size = 2, color = 'black') +
    scale_fill_gradient2(low = 'pink', high = 'maroon') + 
    facet_wrap2((facet %>% append(col)), scales = 'free', strip = strip) +
    theme(axis.text.x = element_blank()) +
    labs(title = title,
         x = col,
         y = map_col)
  
  table <- final %>%
    ungroup() %>%
    select(site, all_of(facet), source_concept_id, concept_id, ct, output) %>%
    mutate(pct = !!sym(output)) %>%
    arrange(site, !!!syms(facet), desc(ct)) %>%
    gt(groupname_col = col) %>%
    gt_plt_bar_pct(column = pct) %>%
    fmt_number(columns = ct, decimals = 0) %>%
    fmt_percent(columns = output, decimals = 0) %>%
    data_color(palette = "Dark2", columns = c(site, all_of(facet))) %>%
    tab_options(row_group.background.color = 'linen',
                row_group.font.weight = 'bold') %>%
    tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Codes')) 
  
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
    add_title(paste0('All Available Mappings for Top ', num_codes, ' Codes'))
  
  output <- list(plot, table)
  
  return(output)
    
}



scv_ss_anom_nt <- function(scv_process,
                           output,
                           facet){
  
  if(output == 'source_prop'){
    col <- 'source_concept_id'
  } else {
    col <- 'concept_id'
  }
  
  mappings_per_code <- scv_process %>%
    group_by(!!!syms(facet), !!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    mutate(median = median(n_mappings),
           q1 = quantile(n_mappings, 0.25),
           q3 = quantile(n_mappings, 0.75))
  
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
    labs(title = 'Codes with Above-Median Number of Unique Mappings')
  
  return(plot)
  
}


scv_ms_anom_nt <- function(scv_process,
                           output,
                           facet,
                           mad_dev = 2){
  
  if(output == 'source'){
    col <- 'source_concept_id'
    map_col <- 'concept_id'
  } else {
    col <- 'concept_id'
    map_col <- 'source_concept_id'
  }
  
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



#' putting it all together
#' HR QUESTION --- if building a single site test, will we allow for more than 
#' one site to be built into the parameter site_list()?

scv_process <- function(cohort = cohort,
                        site_list = c('seattle','cchmc'),
                        domain_tbl=read_codeset('scv_domains', 'cccc'),
                        concept_set = dplyr::union(load_codeset('jia_codes','iccccc'),
                                                   load_codeset('jia_codes_icd','iccccc')),
                        code_type = 'source',
                        code_domain = 'condition_occurrence',
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = read_codeset('age_group_definitions'),
                        time = FALSE,
                        time_span = c('2014-01-01', '2023-01-01')
                        ){
  
  # Set up grouped list
  
  grouped_list <- c('site', 'domain')
  
  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  
  # Prep cohort
  
  cohort_prep <- prepare_pf(cohort_tbl = cohort, age_groups = age_groups, codeset = NULL) %>% 
    mutate(domain = code_domain) %>% 
    group_by(!!! syms(grouped_list))
  
  site_output <- list()
  
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
      
      all_site <- reduce(.x=site_output,
                         .f=dplyr::union) 
      
    }
    
    scv_tbl <- reduce(.x=site_output,
                      .f=dplyr::union)
  
  } else if(time){
    
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
