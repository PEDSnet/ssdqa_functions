
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
                      num_codes){
  
  # picking columns / titles 
  if(output == 'concept_prop'){
    denom <-  'denom_concept_ct'
    col <- 'concept_id'
    title <-  'Proportion of CDM Code Mappings'
    facet <- facet %>% append('concept_id')
  }else if(output == 'source_prop'){
    denom <- 'denom_source_ct'
    col <- 'source_concept_id'
    title <- 'Proportion of Source Code Mappings'
  }
  
  # sorting output to select the most commonly occurring codes and using those in the output
  # is this the best way to filter down the output?
    if(!is.null(facet)){
      filter <- process_output %>%
        select(col, denom, all_of(facet)) %>%
        distinct() %>%
        group_by(!!! syms(facet)) %>%
        arrange(desc(!! sym(denom))) %>%
        slice(1:num_codes)
    }else{
      filter <- process_output %>%
        select(col, denom) %>%
        distinct() %>%
        arrange(desc(!! sym(denom))) %>%
        slice(1:num_codes)
    }
    
    final <- process_output %>% 
      inner_join(filter) 
  
  # option 1: heatmap, option to facet by group, not legible for snomed --> icd mappings
  final %>% ggplot(aes(x = as.character(source_concept_id), y = as.character(concept_id), fill = !!sym(output))) + 
    geom_tile() + 
    geom_text(aes(label = !!sym(output)), size = 2, color = 'black') +
    scale_fill_gradient2(low = 'pink', high = 'maroon') + 
    facet_wrap((facet), scales = 'free')
}

scv_ss_anom_nt <- function(process_output,
                           code_type,
                           facet){
  
  if(code_type == 'source'){
    col <- 'source_concept_id'
    prop_col <- 'source_prop'
  } else {
    col <- 'concept_id'
    prop_col <- 'concept_prop'
  }
  
  mappings_per_code <- process_output %>%
    group_by(!!!syms(facet), !!sym(col)) %>%
    summarise(n_mappings = n()) %>%
    mutate(median = median(n_mappings),
           q1 = quantile(n_mappings, 0.25),
           q3 = quantile(n_mappings, 0.75)) 
  
  # mapping_pairs <- process_output %>%
  #   group_by(!!!syms(facet), concept_id, source_concept_id) %>%
  #   mutate(median = median(!!sym(prop_col)),
  #          q1 = quantile(!!sym(prop_col), 0.25),
  #          q3 = quantile(!!sym(prop_col), 0.75)) 
  
  # plot1 <- id_outliers %>% filter(concept_prop > median) %>%
  #   ggplot(aes(x = as.character(concept_id), y = as.character(source_concept_id), fill = !!sym(output))) + 
  #   geom_tile() + 
  #   coord_flip() +
  #   geom_text(aes(label = !!sym(output)), size = 2, color = 'black') +
  #   scale_fill_gradient2(low = 'pink', high = 'maroon') + 
  #   facet_wrap((facet), scales = 'free') +
  #   theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, vjust = 1)) +
  #   labs(title = 'Mapping Pairs with Representative Proportions +/- 2 MAD away from Median')
  
  
  plot <- mappings_per_code %>%
    filter(n_mappings > median) %>%
    ggplot(aes(x = as.character(!!sym(col)), y = n_mappings)) +
    geom_col() +
    geom_hline(aes(yintercept = median)) +
    geom_hline(aes(yintercept = q1), linetype = 'dotted') +
    geom_hline(aes(yintercept = q3), linetype = 'dotted') +
    facet_wrap((facet), scales = 'free') +
    theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, vjust = 1)) +
    labs(title = 'Codes with Above-Median Number of Unique Mappings')
  
}



#' putting it all together

scv_process <- function(cohort = cohort,
                        site_list = c('seattle','cchmc'),
                        domain_tbl=read_codeset('scv_domains', 'cccc'),
                        concept_set = dplyr::union(load_codeset('jia_codes'),
                                                   load_codeset('jia_codes_icd')),
                        code_type = 'source',
                        code_domain = 'condition_occurrence',
                        multi_or_single_site = 'single',
                        anomaly_or_exploratory='exploratory',
                        age_groups = read_csv('specs/age_group_definitions.csv'),
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
                               code_type = code_type,
                               code_domain = code_domain,
                               concept_set = concept_set,
                               time_span = time_span,
                               domain_tbl = domain_tbl)
    
  }
  
  
  return(scv_tbl)
  
 
}
