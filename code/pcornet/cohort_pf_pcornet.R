
#' function to take in domain and compute patient facts
#' 
#' @param cohort the cohort for which to iterate 
#' @param pf_input_tbl the tbl that should be iterated 
#' so person facts are computed
#' @param grouped_list a vector to group input by. Defaults to `person_id`,
#' `start_date`, `end_date`, `fu`, `site`
#' @param domain_tbl the config CSV file
#' 
#' @return the column `person_id`, for the specified visit type, 
#' the number of facts for the person - each fact corresponds to the 
#' fact in the CSV file; each column is a domain
#' 

compute_pf_pcnt <- function(cohort, 
                            pf_input_tbl,
                            grouped_list,
                            domain_tbl) {
  
  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))
  
  
  for (i in 1:length(domain_list)) {
    
    domain_name = domain_list[[i]][[1]]
    message(paste0('Starting domain ', domain_list[[i]][1]))
    
    ## checks to see if the table needs to be filtered in any way; 
    ## allow for one filtering operation
    if(!is.na(domain_list[[i]][[3]]) && !is.na(domain_list[[i]][[4]])) {
      
      filter_var <- domain_list[[i]][[3]] 
      filter_vec <- as.integer(strsplit(domain_list[[i]][[4]],split=',',fixed = TRUE)[[1]])
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
        filter(!! sym(filter_var) %in% c(filter_vec))
      
    } else if(!is.na(domain_list[[i]][[3]] && is.na(domain_list[[i]][[4]]))){
      
      filter_var <- domain_list[[i]][[3]] 
      samp <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% select(!!sym(filter_var)) %>%
        head(1) %>% collect()
      var_class <- unlist(lapply(samp, class))
      
      if(var_class == 'character'){
        domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
          filter(!!sym(filter_var) != 'NI', !!sym(filter_var) != 'OT',
                 !!sym(filter_var) != 'UN', !is.na(!!sym(filter_var)))
      }else{
        domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% 
          filter(! is.na(!!sym(filter_var)))
      }
      
    }else{domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]]))}
    
    ## computes facts per patient by a named list of grouped variables
    ## assumes patid is part of named list
    pf <- 
      pf_input_tbl %>% 
      inner_join(select(domain_tbl_use,
                        encounterid)) %>% 
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>% 
      ungroup() %>% 
      mutate(domain=domain_name) %>% 
      mutate(fact_ct_strat=round(total_strat_ct/fu,2)) %>% 
      select(-total_strat_ct) %>% 
      select(patid,
             domain,
             fact_ct_strat) %>%
      pivot_wider(names_from=domain,
                  values_from=fact_ct_strat) %>% 
      right_join(cohort) %>%
      relocate(patid) %>%
      compute_new(indexes=list('patid'))
    
    domain_results[[domain_name]] <- pf
  }
  
  domain_results_left_join <- 
    reduce(.x=domain_results,
           .f=left_join)
}


#' combine PF output into one table per study
#'
#' @param study_abbr string that corresponds with the study/studies of interest
#' @param visit_type_list the types of visits in the PF output; defaults to:
#'                        inpatient, outpatient, other_visit, all
#'
#' @return a list of dataframes that contain all sites and all domains for each study
#'         listed in `study_abbr`. the resulting dataframes will also have an
#'         age grouping column added.
#'         
#' 
combine_study_facts <- function(pf_tbl,
                                study_abbr,
                                domain_list,
                                time = FALSE,
                                visit_type_list = list('inpatient','outpatient',
                                                       'other_visit','all')) {
  final_list <- list()
  
  pf_visits <- 
    str_remove(names(pf_tbl),'(pf_)')
  
  names(pf_tbl) <- str_remove(names(pf_tbl), '(pf_)')
  
  for(i in 1:length(visit_type_list)) {
    
    possible_cols <-  domain_list %>% 
      select(domain) %>% c()
    
    visit_type_pulled <- 
      paste0(visit_type_list[[i]])
    
    pf_tbl_visittype <- 
      pf_tbl[[visit_type_pulled]]
    
    # visit_type_pulled <- 
    #   get_results(paste0(visit_type_list[[i]]))  %>% 
    #   select(-any_of('fact_ct_strat')) 
    
    tbl_cols <- pf_tbl_visittype %>% colnames()
    
    selected_cols <- 
      intersect(possible_cols[[1]],
                tbl_cols)
    
    if(!time){
    mutated_tbl <- 
      pf_tbl_visittype %>% 
      pivot_longer(cols=all_of(selected_cols),
                   names_to='domain',
                   #names_to='var_name',
                   values_to='var_val') %>%
      mutate(var_ever=case_when(!is.na(var_val)~1L,
                                TRUE~0L)) %>% 
      mutate(var_val=case_when(is.na(var_val) ~ 0,
                                TRUE ~ var_val)) %>% 
       mutate(study=study_abbr,
              visit_type=visit_type_pulled)
    } else {mutated_tbl <- pf_tbl_visittype %>% mutate(study=study_abbr,
                                                 visit_type=visit_type_pulled)}
    
    
    final_list[[i]] <- mutated_tbl
    
  }
  
  final_list_reduce <- reduce(.x=final_list,
                              .f=dplyr::union)
  final_list_reduce
  
}


## Get results_tbl

get_results <- function(tbl_name) {
  
  rslt <- results_tbl(tbl_name) %>% collect()
  
}


#' output a list of tables to the database 
#' 
#' @param output_list list of tables to output
#' @param append logical to determine if you want to append if the table exists
#' 
#' @return tables output to the database; if 
#' table already exists, it will be appended
#' 

output_list_to_db <- function(output_list,
                              append=FALSE) {
  
  
  if(append) {
    
    for(i in 1:length(output_list)) {
      
      output_tbl_append(data=output_list[[i]],
                        name=names(output_list[i]))
      
    }
    
  } else {
    
    for(i in 1:length(output_list)) {
      
      output_tbl(data=output_list[[i]],
                 name=names(output_list[i]))
      
    }
    
  }
  
}

#' output table to database if it does not exist, or
#' append it to an existing table with the same name if it does
#' 
#' @param data the data to output
#' @param name the name of the table to output 
#' 
#' Parameters are the same as `output_tbl`
#' 
#' @return The table as it exists on the databse, with the new data
#' appended, if the table already existts.
#' 

output_tbl_append <- function(data, name = NA, local = FALSE,
                              file = ifelse(config('execution_mode') !=
                                              'development', TRUE, FALSE),
                              db = ifelse(config('execution_mode') !=
                                            'distribution', TRUE, FALSE),
                              results_tag = TRUE, ...) {
  
  if (is.na(name)) name <- quo_name(enquo(data))
  
  if(db_exists_table(config('db_src'),intermed_name(name,temporary=FALSE))) {
    
    tmp <- results_tbl(name) %>% collect_new 
    new_tbl <- 
      dplyr::union(tmp,
                   data %>% collect())
    output_tbl(data=new_tbl,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  } else {
    output_tbl(data=data,
               name=name,
               local=local,
               file=file,
               db=db,
               results_tag = TRUE, ...)
  }
  
  
}


#' *MS Anomaly Across Time Output*
#' 
#' @param process_output the input tbl to compute an AUC for the multi-site across time analysis  
#' @param grp_vars variables to group by to compute the aggregated proportion for all sites
#' 
#' @return a dataframe with the AUC values for each variable included in the function input table
#' 

compute_pf_auc <- function(process_output,
                            grp_vars=c('time_start',
                                       'time_increment',
                                       'visit_type',
                                       'domain')) {
  
  
  process_output <- process_output %>%
    mutate(prop_pt_fact = fact_ct_denom / pt_ct_denom)
  
  x <- compute_dist_mean_median(tbl=process_output,
                                grp_vars=grp_vars,
                                var_col='prop_pt_fact',
                                num_sd = 2,
                                num_mad = 2)  %>% 
    rename(mean_allsiteprop=mean)
  
  x_filtered <- 
    x %>% select(site,
                 !!!syms(grp_vars),
                 prop_pt_fact,
                 mean_allsiteprop)
  
  # x_variableconcepts <- 
  #   x_filtered %>% distinct(variable,concept_id)
  
  x_concepts <- 
    x_filtered %>% ungroup() %>% distinct(domain) %>% pull()
  
  output <- list()
  
  for(i in 1:length(x_concepts)) {
    
    aucs <- compute_auc_at(tbl_name= x_filtered %>% filter(domain==x_concepts[[i]]) %>%
                             ungroup(),
                           iterate_var = 'site',
                           time_var = 'start_date',
                           outcome_var = 'prop_pt_fact',
                           gold_standard_var = 'mean_allsiteprop') %>% 
      mutate(domain=x_concepts[[i]],
             auc_mean=round(mean(auc_value, na.rm = TRUE),4),
             auc_sd=round(sd(auc_value, na.rm = TRUE),4))
    
    
    
    
    output[[i]] <- aucs  
    
  }
  
  output_reduced <- reduce(.x=output,
                           .f=dplyr::union) #%>% 
  #inner_join(x_variableconcepts)
  
  
}