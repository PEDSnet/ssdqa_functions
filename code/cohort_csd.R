

#' Base CSD function
#' 
#' @param cohort 
#' @param concept_set 
#' @param code_type 
#' @param code_domain 
#' @param time 
#' @param domain_tbl 
#' 
#' @return 
#' 
check_code_dist_csd <- function(cohort_codedist,
                                concept_set,
                                code_domain,
                                time = FALSE,
                                domain_tbl = read_codeset('scv_domains', 'cccc')){
  
  # pick the right domain/columns
  #domain_filter <- domain_tbl %>% filter(domain == code_domain)
  domain_filter <- 
    concept_set %>% select(domain) %>% distinct() %>% 
    inner_join(domain_tbl)
  concept_set_db <- copy_to_new(df=concept_set, name='concept_set')
  #concept_col <- domain_filter$concept_col
  #source_col <- domain_filter$source_col
  
  # if(code_type=='source') {
  #   final_col = source_col
  # }else if(code_type == 'cdm'){
  #   final_col = concept_col
  # }else{stop(paste0(code_type, ' is not a valid argument. Please select either "source" or "cdm"'))}
  
  fact_tbl_final <- list()
  
  for(i in 1:nrow(domain_filter)) {
    
    dates <- domain_filter$date_col[[i]]
    
    domain_tbl_name <- domain_filter[i,1] %>% pull
    domain_tbl_cdm <- cohort_codedist %>% 
      inner_join(cdm_tbl(domain_tbl_name)) %>%
      filter(!!sym(dates) >= start_date,
             !!sym(dates) <= end_date)  
    final_col <- domain_filter[i,]$concept_col
    
    if(time){
      fact_tbl <- 
        domain_tbl_cdm %>% 
        inner_join(concept_set_db,
                   by=setNames('concept_id',final_col)) %>% 
        select(all_of(group_vars(cohort_codedist)),
               all_of(final_col),
               variable,
               time_start,
               time_increment) %>% 
        group_by(time_start,
                 time_increment,
                 .add=TRUE) %>%  
        rename('concept_id' = final_col) 
    } else {
      fact_tbl <- 
        domain_tbl_cdm %>% 
        inner_join(concept_set_db,
                   by=setNames('concept_id',final_col)) %>% 
        select(all_of(group_vars(cohort_codedist)),
               all_of(final_col),
               variable) %>% 
        rename('concept_id' = final_col) 
      
    }
    
    cts <- 
      fact_tbl %>% 
      group_by(
        concept_id,
        variable,
        .add=TRUE
      ) %>% 
      summarise(ct_concept=n()) %>% 
      collect() 
    
    fact_tbl_final[[i]] <- cts
  }
  
  fact_tbl_final_reduce <- 
    reduce(.x = fact_tbl_final,
           .f= dplyr::union)
  
  denom <- 
    fact_tbl_final_reduce %>% 
    ungroup(concept_id) %>% 
    group_by(variable,
             .add=TRUE) %>% 
    # group_by(
    #   -concept_id,
    #   .add=TRUE
    # ) %>% 
    summarise(ct_denom=sum(ct_concept)) %>% 
    collect()
  
  props <- 
    denom %>% 
    inner_join(fact_tbl_final_reduce, multiple='all') %>% 
    mutate(prop_concept = round(ct_concept/ct_denom, 2),
           concept_id = as.character(concept_id))
  
  
}
  
#   if(time){
#     
#     domain_tbl <- cohort_codedist %>%
#       inner_join(cdm_tbl(code_domain)) %>%
#       filter(!!sym(domain_filter$date_col) >= start_date,
#              !!sym(domain_filter$date_col) <= end_date)
#     
#     
#     fact_tbl <- 
#       domain_tbl %>% 
#       inner_join(concept_set,
#                  by=setNames('concept_id',final_col)) %>% 
#       select(all_of(group_vars(cohort_codedist)),
#              all_of(concept_col),
#              all_of(source_col),
#              time_start,
#              time_increment) %>% 
#       rename('concept_id' = concept_col,
#              'source_concept_id' = source_col) %>%
#       group_by(time_start, time_increment, .add = TRUE)
#     
#   }else{
#     
#     fact_tbl_final <- list()
#     
#     for(i in 1:nrow(domain_filter)) {
#       
#       domain_tbl_name <- domain_filter[i,1] %>% pull
#       domain_tbl_cdm <- cohort_codedist %>% 
#         inner_join(cdm_tbl(domain_tbl_name))  
#       final_col <- domain_filter[i,]$concept_col
#       
#       fact_tbl <- 
#         domain_tbl_cdm %>% 
#         inner_join(concept_set_db,
#                    by=setNames('concept_id',final_col)) %>% 
#         select(all_of(group_vars(cohort_codedist)),
#                all_of(final_col),
#                variable) %>% 
#         rename('concept_id' = final_col) 
#       
#       cts <- 
#         fact_tbl %>% 
#         group_by(
#           concept_id,
#           variable,
#           .add=TRUE
#         ) %>% 
#         summarise(ct_concept=n()) %>% 
#         collect()
#       
#       denom <- 
#         fact_tbl %>% 
#         group_by(
#           variable,
#           .add=TRUE
#         ) %>% 
#         summarise(ct_denom=n()) %>% 
#         collect()
#       
#       props <- 
#         denom %>% 
#         inner_join(cts, multiple='all') %>% 
#         mutate(prop_concept = round(ct_concept/ct_denom, 2),
#                concept_id = as.character(concept_id))
#       
#       fact_tbl_final[[i]] <- props
#       
#     }
#     
#     fact_tbl_final_reduce <- 
#       reduce(.x = fact_tbl_final,
#              .f= dplyr::union)
#     
#   }
#   
#   fact_tbl_final_reduce
#   
# }


#' Base CSD function
#' 
#' @param cohort 
#' @param concept_set 
#' @param code_type 
#' @param code_domain 
#' @param time 
#' @param domain_tbl 
#' 
#' @return 
#' 
check_code_dist_ssanom <- function(cohort_codedist,
                                   concept_set,
                                   code_domain,
                                   time = FALSE,
                                   num_concept_combined = FALSE,
                                   num_concept_1 = 30,
                                   num_concept_2 = 30,
                                   domain_tbl = read_codeset('scv_domains', 'cccc')){
  
  
  domain_filter <- 
    concept_set %>% select(domain) %>% distinct() %>% 
    inner_join(domain_tbl)
  concept_set_db <- copy_to_new(df=concept_set, name='concept_set')
  variable_list <- concept_set_db %>% distinct(variable) %>% pull()
  
      variable_summary <- list() 
      
    for(i in variable_list) {
      
      variable_filtered <- 
        concept_set_db %>% filter(variable == i)
      
      domain_num <- 
        variable_filtered %>% select(domain) %>% distinct() %>% pull()
      
      variable_combined <- list()
      
      for(n in 1:length(domain_num)) {
        
        domain_name <- domain_num[[n]]
        
        final_col <- 
          domain_filter %>% 
          filter(domain == domain_name) %>% 
          select(concept_col) %>% pull()
        
        one_domain_tbl <- 
          cohort_codedist %>% 
          inner_join(
            cdm_tbl(domain_name) 
          ) %>% 
          inner_join(variable_filtered,
                     by=setNames('concept_id',final_col)) %>% 
          #inner_join(cohort_codedist) %>% 
          select(person_id,
                 all_of(group_vars(cohort_codedist)),
                 all_of(final_col),
                 variable) %>% 
          rename('concept_id'=final_col) %>% 
          mutate(domain = domain_name) %>% 
          group_by(person_id,concept_id, variable, domain,
                   .add=TRUE) %>% 
          summarise(ct=n()) %>% 
          compute_new(temporary=TRUE)
        
        variable_combined[[n]] <- one_domain_tbl
        
      }
      
      variable_flattened <- reduce(.x=variable_combined,
                                   .f=dplyr::union)
      
      var_domain_lookup <- 
        variable_flattened %>% 
        ungroup %>% select(concept_id,variable) %>% distinct() %>%  collect()
      
     jaccards <- compute_jaccard(variable_flattened) %>% 
       mutate(variable = i)
     
     variable_summary[[i]] <- jaccards
      
    }
      
      combined <- reduce(.x=variable_summary,
                         .f=dplyr::union)
      
      if(! num_concept_combined) {
        combined_filtered <- 
          combined %>% 
          filter(concept1_ct > num_concept_1 | concept2_ct > num_concept_2)
      } else {combined_filtered <- 
        combined %>% 
        filter(concept1_ct > num_concept_1,
             concept2_ct > num_concept_2)}
      
      x_vars_meansd <- 
        combined_filtered %>% 
        group_by(variable) %>% 
        summarise(var_jaccard_mean=mean(jaccard_index),
                  var_jaccard_sd=sd(jaccard_index))
      
      tbl_input <- 
        combined_filtered %>% 
        inner_join(x_vars_meansd) %>% 
        mutate(above_sd=
                 case_when(jaccard_index > (var_jaccard_mean + var_jaccard_sd) ~ TRUE,
                           TRUE ~ FALSE)) %>% 
        mutate(across(where(is.double), \(x) round(x, digits=3)))
   
        
      
}
    
    # fact_tbl_final_reduce <- 
    #   reduce(.x = fact_tbl_final,
    #          .f= dplyr::union)
    # 
    # 
    # fact_tbl_final_reduce
    
 
compute_jaccard <- function(jaccard_input_tbl) {
  
  persons_concepts <- 
    jaccard_input_tbl %>% ungroup %>% #distinct() %>% collect()
    select(person_id,
           concept_id) %>% distinct() %>% collect()
  
  persons_concepts_cts <- 
    persons_concepts %>% 
    group_by(concept_id) %>% 
    summarise(concept_person_ct=n_distinct(person_id))
  
  concord <- 
    persons_concepts %>% table() %>% crossprod()
  diag(concord) <- -1
  
  best <- as_tibble(concord, rownames='concept1') %>% 
    pivot_longer(!concept1, names_to = 'concept2', values_to='cocount') %>% 
    filter(cocount != -1L) %>% mutate(across(.cols = c(concept1, concept2, cocount), .fns=as.integer)) %>%
    left_join(persons_concepts_cts, by = c('concept1'='concept_id'))%>%
    rename(concept1_ct=concept_person_ct)%>%
    left_join(persons_concepts_cts, by = c('concept2'='concept_id'))%>%
    rename(concept2_ct=concept_person_ct) %>%
    mutate(concept_count_union=concept1_ct+concept2_ct-cocount,
           jaccard_index=cocount/concept_count_union) %>% 
    mutate(concept1_prop=round(cocount/concept1_ct,2),
           concept2_prop=round(cocount/concept2_ct,2)) %>% 
    filter(concept1_ct > 0 & concept2_ct > 0 & cocount > 0) %>% 
    filter(concept1 > concept2) 
  
  best
  
}




