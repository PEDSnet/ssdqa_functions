
########################## DATA PRE PROCESSING #####################################

#' Compute median fact counts by visit & fact type and by site, visit, & fact type
#'
#' @param data_input the list output of `combine_study_facts`
#' @param agegrp boolean that determines whether the output should also
#'               be grouped by age group
#'
#' @return dataframe that contains the total median number of facts for that visit & fact_type
#'         as well as the median number of facts for the site, visit, & fact type for each
#'         study 
#' 
compute_pf_medians <- function(data_input,
                               agegrp=FALSE,
                               codeset=FALSE) {
  
  
  data_input_cols <- data_input %>% colnames()
  
  if('cohort' %in% data_input_cols) {
    data_input_grp <- 
      data_input %>% group_by(cohort)
  } else {data_input_grp <- data_input}
  
  if(agegrp) {data_input_grp <- data_input_grp %>% group_by(age_grp,.add=TRUE)}
  if(codeset) {data_input_grp <- data_input_grp %>% group_by(flag,.add=TRUE)}
  
  site_distance_medians_tbl <- 
    data_input_grp %>% 
    group_by(study,
             visit_type,
             var_name,
             .add=TRUE) %>% 
    mutate(median_all_with0s=median(var_val),
           median_all_without0s=median(var_val[var_val!=0])) %>% 
    ungroup()
  
  site_distance_final <- 
    data_input_grp %>% 
    left_join(site_distance_medians_tbl) %>% 
    group_by(study,
             site,
             visit_type,
             var_name,
             median_all_with0s,
             median_all_without0s,
             .add=TRUE) %>% 
    summarise(n_tot=n(),
              n_w_fact=sum(var_ever),
              median_site_with0s=median(var_val),
              median_site_without0s=median(var_val[var_val!=0])) %>% ungroup() %>% 
    mutate(prop_all_w_fact=round(n_w_fact/n_tot,3))
  
  site_distance_final <- 
    site_distance_final %>% replace(is.na(.), 0) %>% 
    mutate(across(everything(), ~ replace(.x, is.nan(.x), 0)))
  
  
  site_distance_final
  
}


########################## PRE VISUALIZATION PROCESSING #####################################

### One table per site & outcome_var

create_list_input_sepsites <- function(data_tbl,
                                       outcome_var) {
  
  site_list <- 
    data_tbl %>% select(site) %>% distinct() %>% pull() %>% as.list()
  
  final_list <- list()
  
  thresholds <- read_codeset('thresholds', col_types='ccd')
  
  for(i in 1:length(site_list)) {
    elem1 <- data_tbl %>% filter(site==site_list[[i]])%>%
      left_join(thresholds, by = c('visit_type', 'var_name'='domain'))
    elem2 <- outcome_var
    
    args_list <- 
      list('data_tbl' = elem1,
           'outcome' = elem2)
    
    final_list[[i]] <- args_list
    
  }
  
  final_list
}


### One table per domain for the specified visit_type_nm

#' @param data_tbl --- the data tbl that will be used for the `prod_bar_sepvisittype` function
#' @param visit_type_nm --- the visit type to produce output for
#' 
#' 
create_list_input_sepvisittype <- function(data_tbl,
                                           visit_type_nm) {
  
  domain_config_domains <- 
    domain_config %>% select(domain) %>% pull() %>% as.list()
  
  final_list <- list()
  
  thresholds <- read_codeset('thresholds', col_types='ccd')
  
  for(i in 1:length(domain_config_domains)) {
    elem1 <- data_tbl %>% filter(visit_type==visit_type_nm) %>%
      left_join(thresholds, by = c('visit_type', 'var_name'='domain'))
    elem2 <- domain_config_domains[[i]]
    
    args_list <- 
      list('data_tbl' = elem1,
           'var_name_label' = elem2)
    
    final_list[[i]] <- args_list
    
  }
  
  final_list
}


### One table per visit_type & var_name_list element, w/ argument for facet_var_nm

#' @param data_tbl --- the data tbl that will be used for the `prod_bar_facet` function
#' @param visit_type_nm --- the visit type to produce output for
#' 
#' 
create_list_input_facet_sepvarname <- function(data_tbl,
                                               visit_type_nm,
                                               facet_var_nm,
                                               var_name_list) {
  
  domain_list <- 
    var_name_list
  
  final_list <- list()
  
  for(i in 1:length(domain_list)) {
    elem1 <- data_tbl %>% filter(visit_type==visit_type_nm)
    elem2 <- domain_list[[i]]
    
    args_list <- 
      list('data_tbl' = elem1,
           'var_name_label' = elem2,
           'facet_var' = facet_var_nm)
    
    final_list[[i]] <- args_list
    
  }
  
  final_list
}


### one table per site for the pyramid graphs

create_list_input_sepsites_pyr <- function(data_tbl) {
  
  site_list <- 
    data_tbl %>% select(site) %>% distinct() %>% pull() %>% as.list()
  
  final_list <- list()
  
  for(i in 1:length(site_list)) {
    elem1 <- data_tbl %>% filter(site==site_list[[i]])
    
    args_list <- 
      list('data_tbl' = elem1)
    
    final_list[[i]] <- args_list
    
  }
  
  final_list
}


############################## LOF PREP #######################################

#' iterate through all sites `compute_lof`
#' for all sites
#' 
#' @param input_tbls the input generated from `compute_lof`
#' @param var_list_args the list of arguments that include `var_name` and associated hex color
#' @param 
#' 

#' Function to loop through table and apply function `prod_bar_sepsites`
#' 
#' @param list_name object name of the list produced from `create_list_input_sepsites`
#' 

create_sepsite_output_lof <- function(input_tbls,
                                      var_list_arg) {
  
  output_sepsites <- list()
  
  for(i in 1:length(input_tbls)) {
    
    final <- compute_lof(data_tbl=input_tbls[[i]],
                         var_list_arg=var_list_arg)
    
    final_reduce <- reduce(.x=final,
                           .f=dplyr::union) 
    
    output_sepsites[[i]] <- final
  }
  
  output_sepsites
  
}

#' @param var_list_arg two element list, with the firstbeing the variable name
#' and the second being the associated hex color for the graph output
#' @param data_tbl the data tbl to perform LOF on 
#' 
##' @param var_list list of lists with the following elements: 
##'   list(list(`first` = list(`domain1`, `hex_color1`),
##'                       list(`domain2`, `hex_color2`), etc))
##'                       

##' 
compute_lof <- function(data_tbl,
                        var_list_arg,
                        remove_outliers = TRUE) {
  
  var_list <- var_list_arg
  # var_list = list('first' = list('conditions_all', '#a5879e'),
  #                 'second' = list('anthropometrics','#e52b50'),
  #                 'third' = list('labs', '#6e9f65'))
  final_lof <- list()
  #colors_final <- c()
  
  for(i in 1:length(var_list)) {
    
    message(paste0('Starting ',var_list[[i]][[1]]))
    
    var_nm <- var_list[[i]][[1]]
    
    data_tbl_site <- data_tbl %>% select(site) %>% distinct() %>% pull()
    
    lof_tbl <- data_tbl %>% 
      filter(var_name == var_nm) %>% 
      filter(var_ever == 1) %>% 
      group_by(visit_type) %>% 
      arrange(.by_group = TRUE) %>% ungroup() %>% distinct() 
    
    k_val <- gen_k(lof_tbl)
    
    lof_setup_scale <- as.data.frame(scale(lof_tbl %>% select(var_val))) 
    
    lof_t <- lof(lof_setup_scale, k=k_val)
    
    lof_tbl$lof_val <- lof_t
    
    lof_tbl <- 
      lof_tbl %>% 
      filter(!is.infinite(lof_val),
             !is.na(lof_val),
             !is.nan(lof_val))
    
    color_final <- var_list[[i]][[2]]
    
    quantiles <- quantile(lof_tbl$lof_val, probs=c(0.1, 0.8, 0.9, 0.95, 0.99))
    
    final_lof[[i]] <- lof_tbl %>% mutate(hex_color=color_final) %>% 
      mutate(tenth_percentile=quantiles[[1]],
             eightieth_percentile=quantiles[[2]],
             ninetieth_percentile=quantiles[[3]],
             ninetyfifth_percentile=quantiles[[4]],
             ninetyninth_percentile=quantiles[[5]])
    
  }
  
  final_lof
  
}


#' Get general k value for LOF computation
#'
#' @param dat dataframe with one row for each variable in each visit type
#'
#' @return one value that will be used as the k value in `compute_lof`
#' 
gen_k <- function(dat) {
  
  dat_grprows <- 
    dat %>% filter(var_ever == 1) %>% 
    group_by(
      visit_type
    ) %>% summarise(ct=n())
  
  k_val <- 
    dat_grprows %>% 
    ungroup() %>% 
    summarise(median_ct=median(ct)) %>% 
    mutate(k_val=sqrt(floor(0.10*median_ct))) %>% 
    mutate(k_val=case_when(k_val < 30 ~ 30,
                           k_val > 60 ~ 60,
                           TRUE ~ k_val)) %>% 
    select(k_val) %>% pull()
}


##' creates list of input for the `compute_lof` function
##' @param data_tbl the data_tbl --- should not filter by site (will be done in the function);
##' the `compute_lof` function will iterate through the var_name, so this should also 
##' not be filtered by that or the visit type, since visit_type is part of the `lof` computation. 


create_lof_input <- function(data_tbl){
  
  site_list <- 
    data_tbl %>% select(site) %>% distinct() %>% pull() %>% as.list()
  
  
  
  final_list <- list()
  
  for(i in 1:length(site_list)) {
    
    # tmp_list <- list()
    elem1 <- data_tbl %>% filter(site==site_list[[i]])
    final_list[[i]] <- elem1
    
  }
  
  final_list
}

#' Reduce the nested list from `create_sepsite_output_lof`
#'
#' @param lof_comp_output nested list output from `create_sepsite_output_lof`
#'
#' @return a list of dataframes, each dataframe belonging to one site with the 
#'         previously separated fact type tables combined into one
#' 
sepsite_lof_reduce <- function(lof_comp_output) {
  
  output_sepsite <- list()
  
  for(i in 1:length(lof_comp_output)) {
    
    reduce_site_lofs <- reduce(.x=lof_comp_output[[i]],
                               .f=dplyr::union)
    
    output_sepsite[[i]] <- reduce_site_lofs
  }
  
  output_sepsite %>% reduce(dplyr::union)
  
}

############################ K-means prep ######################################

prep_kmeans <- function(dat) {
  
  kmeans_prep <- 
    dat %>% 
    select(
      site,var_name, visit_type,
      median_site_without0s
    ) %>% pivot_wider(id_cols = site,
                      names_from = c(var_name, visit_type),
                      values_from = median_site_without0s) 
  
  kmeans_prep <- 
    kmeans_prep %>% replace(is.na(.), 0) %>% 
    mutate(across(everything(), ~ replace(.x, is.nan(.x),0)))
  
  kmeans_mat <- 
    kmeans_prep %>% 
    column_to_rownames(., var='site')
  
  kmeans_scaled <- scale(kmeans_mat) 
  
  kmeans_scaled[, !colSums(!is.finite(kmeans_scaled))]
  
}


############################# GENERATE OUTPUT ##################################

#' ******Sep Sites Bar Output******

#' Function to loop through table and apply function `prod_bar_sepsites`
#' 
#' @param list_name object name of the list produced from `create_list_input_sepsites`
#' @param thresh boolean indicator of whether to include thresholds on plot
#' 

create_sepsite_output <- function(list_name,
                                  thresh=FALSE) {
  
  output_sepsites <- list()
  
  for(i in 1:length(list_name)) {
    
    final <- prod_bar_sepsites(data_tbl=list_name[[i]][[1]],
                               outcome=list_name[[i]][[2]],
                               incl_thresh = thresh)
    
    output_sepsites[[i]] <- final
  }
  
  output_sepsites
  
}

#' produces output for a given outcome (proportion of visits, median number, etc)
#' for a given data output, faceted by visit type. Side bar plots, with each bar
#' the pf_domain, or var_name. 
#' 
#' This function relies on one of the filters being BY SITE. This is a site-specific
#' output measure.
#' 
#' @param data_tbl the data tbl
#' @param outcome the outcome, as a string; must be the column name in `data_tbl`
#' @param incl_thresh boolean indicator of whether to overlay thresholds on the plot
#' 
#' @return see description; ggcharts style output
#' 

prod_bar_sepsites <- function(data_tbl,
                              outcome,
                              incl_thresh) {
  
  site_nm <- data_tbl %>% select(site) %>% distinct() %>% pull()
  
  color <- site_report_config %>% 
    filter(site == site_nm) %>% 
    select(site_color) %>% pull()
  if(incl_thresh&str_detect(outcome,"prop")){
    bar_chart(data_tbl, x=var_name,y=!! sym(outcome),facet=visit_type, bar_color=color) +
      ggtitle(paste0(site_nm))+
      geom_point(aes(x=var_name,y=threshold_prop))
  }
  else if(incl_thresh&str_detect(outcome,"med")){
    bar_chart(data_tbl, x=var_name,y=!! sym(outcome),facet=visit_type, bar_color=color) +
      ggtitle(paste0(site_nm))+
      geom_point(aes(x=var_name,y=threshold_med))
  }
  else{
    bar_chart(data_tbl, x=var_name,y=!! sym(outcome),facet=visit_type, bar_color=color) +
      ggtitle(paste0(site_nm))
  }
}


#' ******Sep Visit Types Output******

#' Function to loop through table and apply function `prod_bar_sepvisittype`
#' 
#' @param list_name object name of the list produced from `create_list_input_sepvisittype`
#' @param thresh boolean indicator of whether to include thresholds on plot
#' 


create_sepvisittypes_output <- function(list_name,
                                        thresh=FALSE) {
  
  output_sepvisittypes <- list()
  
  for(i in 1:length(list_name)) {
    
    final <- prod_bar_sepvisittype(data_tbl=list_name[[i]][[1]],
                                   var_name_label=list_name[[i]][[2]],
                                   incl_thresh=thresh)
    
    output_sepvisittypes[[i]] <- final
  }
  
  output_sepvisittypes
  
  
}

#' produces output for median number of facts (for patients who have the fact)
#' for a given data input. Side bar plots, with each bar the site name. Requires 
#' site as a column in the data. As part of the `data_tbl` input, this function 
#' REQUIRES that there is a filter for a particular visit type. Otherwise, 
#' it will break.
#' 
#' 
#' @param data_tbl the data tbl
#' @param var_name_label the outcome, as a string; must be the column name in `data_tbl`
#' @param incl_thresh boolean indicator of whether to overlay thresholds on plot
#' 
#' @return see description; ggcharts style output
#' 


prod_bar_sepvisittype <- function(data_tbl,
                                  var_name_label,
                                  incl_thresh) {
  
  color <- domain_config %>% 
    filter(domain == var_name_label) %>% 
    select(domain_color) %>% pull()
  
  if(incl_thresh){
    bar_chart(data_tbl %>% filter(var_name==var_name_label), x=site,y=median_site_without0s,bar_color=color) +
      geom_line(aes(x=site,y=threshold_med,group=var_name))+
      scale_color_manual(values=site_colors) +
      ggtitle(paste0(var_name_label))
  }
  else{
    bar_chart(data_tbl %>% filter(var_name==var_name_label), x=site,y=median_site_without0s,bar_color=color) +
      scale_color_manual(values=site_colors) +
      ggtitle(paste0(var_name_label))
  }
  
}


#' ******Facet Var Output******

# Function to loop through table and apply function `prod_bar_facet`

create_facet_graphs_byvarname <- function(list_name) {
  
  output_varname <- list()
  
  for(i in 1:length(list_name)) {
    
    final <- prod_bar_facet(data_tbl=list_name[[i]][[1]],
                            var_name_label=list_name[[i]][[2]],
                            facet_var_nm=list_name[[i]][[3]])
    
    output_varname[[i]] <- final
    
  }
  
  output_varname
}

#' stratifies bar graphs by a particular variable
#' 
#' *** NOTE THAT THIS DATA INPUT SHOULD ALREADY FILTER BY VISIT TYPE FROM THE INPUT;
#' IF NOT, ENSURE THAT THE DATA TBL ARG FILTERS BY VISIT TYPE *** 
#' 
#' 
#' @param data_tbl the data tbl
#' @param var_name_label the outcome, as a string; must be the column name in `data_tbl`
#' @param facet_var the variable to facet by (in our use case, `age_ce_grp`)
#' 
#' @return 
#' 


prod_bar_facet <- function(data_tbl,
                           var_name_label,
                           facet_var_nm) {
  
  color <- domain_config %>% 
    filter(domain == var_name_label) %>% 
    select(domain_color) %>% pull()
  
  bar_chart(data_tbl %>% filter(var_name==var_name_label), 
            facet=!! sym(facet_var_nm), x=site,y=median_site_without0s,bar_color=color) +
    ggtitle(paste0(var_name_label))
  
}


#' ******Pyramid Output******

#' Function to loop through table and apply function `produce_pyramid`
#' 
#' @param list_name object name of the list produced from `create_list_input_sepsites`
#' 

create_sepsite_pyramids <- function(list_name) {
  
  output_sepsites <- list()
  
  for(i in 1:length(list_name)) {
    
    final <- produce_pyramid(data_tbl=list_name[[i]][[1]])
    
    output_sepsites[[i]] <- final
  }
  
  output_sepsites
  
}

#' @param data_input data to produce pyramid plots with 
#' 
#' *** NOTE THAT THE DATA INPUT SHOULD FILTER BY VISIT TYPE ***
#' 
#' @return Measure is the median number of measurements; 
#' the two bar graphs are the two cohorts in the sample
#' 

produce_pyramid  <- 
  function(data_tbl) {
    
    site_nm <- data_tbl %>% select(site) %>% distinct() %>% pull()
    
    pc <- 
      pyramid_chart(
        data=data_tbl,
        x=var_name,
        y=median_site_without0s,
        group=cohort,
        sort='ascending',
        title=paste0(site_nm),
        bar_colors = c('#6fa8dc','#93c47d')
      ) + ggtitle(paste0(site_nm))
    
  }


#' ******LOF Output******

#' computes graph of the lof_value
#' 
#' @param lof_list_output the output from `compute_lof`
#' @param remove_outliers a logical determining whether outliers shoudl be removed for the graph
#' 
#' @return A graph with overlapping density plots`for a particular site


compute_lof_graphs <- function(lof_site_output,
                               remove_outliers=TRUE) {
  
  
  if(remove_outliers) {final_reduce_lim <- 
    lof_site_output %>% filter(lof_val < 1.5,
                               !is.na(lof_val))} else {final_reduce_lim <- lof_site_output %>% 
                                 filter(!is.na(lof_val))}
  
  color_vector <- final_reduce_lim %>% select(var_name,hex_color) %>% distinct() %>% deframe()
  site_nm <- final_reduce_lim %>% select(site) %>% distinct() %>% pull()
  
  final_ggplot <- 
    ggplot(data=final_reduce_lim,
           aes(group=var_name,
               fill=var_name)) +
    geom_density(aes_string(x='lof_val',
                            #adjust=2.0,
                            alpha=0))+
    scale_fill_manual(values=color_vector) +
    labs(x='LOF Value') +
    ggtitle(paste0(site_nm))
  
  
}


#' provides table summary of LOF measures
#' 
#' @param lof_list_output the output from `compute_lof`
#' 
#' 

provide_table_lof <- function(lof_reduce_output) {
  
  
  tbls <- lof_reduce_output %>%
    # reduce(.x=collapsed_tbls,
    #        .f=dplyr::union) %>% 
    select(
      site,
      var_name,
      tenth_percentile,
      eightieth_percentile,
      ninetieth_percentile,
      ninetyfifth_percentile,
      ninetyninth_percentile
    ) %>% distinct()
  
  tbls
  
}


#' iterate through all sites `compute_lof_graphs`
#' for all sites
#' 
#' @param input_tbls the input generated from `compute_lof`
#' @param var_list_args the list of arguments that include `var_name` and associated hex color
#' @param 
#' 

#' Function to loop through table and apply function `prod_bar_sepsites`
#' 
#' @param list_name object name of the list produced from `create_list_input_sepsites`
#' 

create_sepsite_output_lofgraph <- function(lof_reduce_output) {
  
  lof_split_output <- split(lof_reduce_output, f = lof_reduce_output$site)
  
  output_sepsites <- list()
  
  for(i in 1:length(lof_split_output)) {
    
    final <- compute_lof_graphs(lof_site_output = lof_split_output[[i]])
    
    output_sepsites[[i]] <- final
  }
  
  output_sepsites
  
}


create_multisite_output <- function(dat){
  
  graph <- ggplot(dat, aes(x=var_name,y=median_site_without0s, colour=site))+
    geom_point(size=3)+
    geom_point(aes(x=var_name, y=median_all_without0s), shape=8, size=3, color="black")+
    facet_wrap(~visit_type, scales="free_x", ncol=2)+
    theme_bw()+
    coord_flip()
}
