

#' *Single Site, Exploratory, No Time*
#' 
#' 
#' @param process_output the output from `csd_process`
#' @param num_codes an integer to represent the top number of codes to include in the mappings for the exploratory analyses;
#'                  will pick the codes based on the highest count of the most commonly appearing variables; 
#' @param num_mappings an integer to represent the top number of mappings for a given variable in the exploratory analyses
#' @param facet variables to facet by; defaults to NULL
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' 
#' @return a list with two elements: 
#'        1) heatmap for to `n` concepts (`num_codes`)  and `x` variables (`num_mappings`), with proportion for each concept. 
#'        If `vocab_tbl` is not NULL, then will the name of the concept when hovering; 
#'        2) a table with each mapping and the total variable count
#' 
csd_ss_exp_nt <- function(process_output,
                          facet = NULL,
                          #vocab_tbl = vocabulary_tbl('concept'),
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
  
    plot <- final %>% ggplot(aes(x = xaxis, y = as.character(!!sym(map_col)), 
                                 fill = !!sym(prop))) +
      geom_tile_interactive(aes(tooltip = concept_name)) +
      geom_text(aes(label = !!sym(prop)), size = 2, color = 'black') +
      scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) + 
      facet_wrap((facet), scales = 'free') +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      labs(title = title,
           x = col,
           y = map_col)
    
    p <- girafe(ggobj = plot,
                width = 10,
                height = 10)

  ref_tbl <- generate_ref_table(tbl = final %>% mutate(concept_id = as.integer(concept_id)) %>%
                                  select(-concept_name),
                                id_col = col,
                                name_col = col,
                                denom = denom)
  
  output <- list(p, ref_tbl)
  
  return(output)
}


#' *Single Site, Anomaly, No Time*
#' 
#' 
#' @param process_output the output from `csd_process`
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' @param filtered_var the variable to perform the jaccard similarity index for
#' 
#' @return for a given variable, a heatmap of the jaccard index for each concept pair; 
#'         if the user hovers over the heatmap, the co-occurrence count, jaccard score for the pair,
#'         mean jaccard score for the variable, and concepts will show.
#' 
csd_ss_anom_nt <- function(process_output,
                          #facet,
                          #num_concept_combined = FALSE,
                          vocab_tbl = vocabulary_tbl('concept'),
                          #num_codes = 10,
                          #num_mappings = 10,
                          filtered_var = 'general_jia'){
  
  firstcolnames <- join_to_vocabulary(tbl = process_output, #tbl_input,
                                      vocab_tbl = vocab_tbl,
                                      col = 'concept1') %>% 
    rename(conceptname1=concept_name) %>% select(concept1, conceptname1)
  
  secondcolnames <- join_to_vocabulary(tbl = process_output, #tbl_input,
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
      scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
       labs(title = filtered_var,
            x = 'concept1',
            y = 'concept2') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) 
    
    p <- girafe(ggobj=plot,
                width=10,
                height=10)
  
  return(p)
}

### ACROSS TIME

#' *Single Site, Anomaly, Across Time*
#' 
#' Control chart looking at number of mappings over time
#' 
#' using the CHOP-developed package called `rocqi` 
#' 
#' @param process_output dataframe output by `csd_process`
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' @param filtered_var the variable to perform the anomaly detection for
#' @param facet the variables by which you would like to facet the graph; defaults to NULL
#' @param top_mapping_n integer value for the number of concepts to show mappings across time for;
#'                      graph will be faceted by concept
#' 
#' @return a C control chart that highlights points in time where the number of mappings for
#'         a particular code are anomalous; outlying points are highlighted red
#' 
csd_ss_anom_at <- function(process_output,
                           filtered_var='ibd',
                           filter_concept=81893,
                           facet=NULL,
                           top_mapping_n = 6){
  
  time_inc <- process_output %>% filter(!is.na(time_increment)) %>% distinct(time_increment) %>% pull()
  
  if(time_inc == 'year'){
  
  facet <- facet %>% append('concept_id') %>% unique()
  
  top_n <- 
    process_output %>% filter(variable == filtered_var) %>% 
    ungroup() %>% 
    group_by(variable, concept_id) %>% 
    summarise(total_ct = sum(ct_concept)) %>% 
    ungroup() %>% arrange(desc(total_ct)) %>% 
    top_n(n=top_mapping_n) %>% select(concept_id) %>% pull()
  
  c_added <- process_output %>% filter(variable == filtered_var) %>% 
    filter(concept_id %in% top_n)

  
  c_final <- c_added %>% group_by(!!!syms(facet), time_start, ct_concept) %>%
    unite(facet_col, !!!syms(facet), sep = '\n') 
  
  c_plot <- qic(data = c_final, x = time_start, y = ct_concept, chart = 'pp', facet = ~facet_col,
                title = 'Control Chart: Code Usage Over Time', show.grid = TRUE, n = ct_denom,
                ylab = 'Proportion', xlab = 'Time')
  
  op_dat <- c_plot$data
  
  new_pp <- ggplot(op_dat,aes(x,y)) +
    geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
    geom_line(colour = ssdqa_colors_standard[[12]], size = .5) +  
    geom_line(aes(x,cl)) +
    geom_point(colour = ssdqa_colors_standard[[6]] , fill = ssdqa_colors_standard[[6]], size = 1) +
    geom_point(data = subset(op_dat, y >= ucl), color = ssdqa_colors_standard[[3]], size = 2) +
    geom_point(data = subset(op_dat, y <= lcl), color = ssdqa_colors_standard[[3]], size = 2) +
    facet_wrap(~facet1) +
    ggtitle(label = 'Control Chart: Code Usage Over Time') +
    labs(x = 'Time',
         y = 'Proportion')+
    theme_minimal()
  
  output_int <- ggplotly(new_pp)
  
  ref_tbl <- generate_ref_table(tbl = c_added %>% filter(variable == filtered_var) %>% 
                                  filter(concept_id %in% top_n) %>% 
                                  mutate(concept_id=as.integer(concept_id)),
                                id_col = 'concept_id',
                                denom = 'ct_concept',
                                name_col = 'concept_name',
                                #vocab_tbl = vocab_tbl,
                                time = TRUE)
  
  output <- list(output_int, ref_tbl)
  
  }else{
    
    concept_nm <- process_output %>% 
      filter(!is.na(concept_name), concept_id == filter_concept) %>% 
      distinct(concept_name) %>% pull()
    
    anomalies <- 
      plot_anomalies(.data=process_output %>% filter(concept_id == filter_concept),
                     .date_var=time_start) %>% 
      layout(title = paste0('Anomalies for Code ', filter_concept, ': ', concept_nm))
    
    decomp <- 
      plot_anomalies_decomp(.data=process_output %>% filter(concept_id == filter_concept),
                            .date_var=time_start) %>% 
      layout(title = paste0('Anomalies for Code ', filter_concept, ': ', concept_nm))
    
    output <- list(anomalies, decomp)
    
  }
  
  return(output)
  
}


#' *Single Site, Exploratory, Across Time*
#' 
#' Facets by main code (cdm or source) by default, with each line representing
#' a mapping code. using plotly so the legend is interactive and codes can be isolated
#' 
#' 
#' @param process_output dataframe output by `csd_process`
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' @param filtered_var the variable to perform the anomaly detection for
#' @param facet the variables by which you would like to facet the graph; defaults to NULL
#' 
#' @return a line graph with one facet per code displaying the proportion of mapped codes
#'         across the user selected time period
#' @return a reference table with total counts of each code across the entire user selected
#'         time period
#' 
csd_ss_exp_at <- function(process_output,
                             facet=NULL,
                             filtered_var = c('ibd','spondyloarthritis'),
                             #multi_or_single_site = 'multi',
                             #vocab_tbl = vocabulary_tbl('concept'),
                             output_value='prop_concept'){
  
  output_value <- output_value
  
  site_num <- 
    process_output %>% ungroup() %>%select(site) %>% distinct() %>% pull()
  
  if(length(site_num)>1){
    facet <- facet %>% append('site')
  } else {
    facet <- facet %>% append('variable')
  }

  
  dat_to_plot <- process_output %>% filter(variable %in% filtered_var) %>% 
    mutate(text=paste("Concept: ",concept_id,
                      "\nConcept Name: ",concept_name,
                      "\nSite: ",site,
                      "\nValue: ",!!sym(output_value),
                      "\nTime Point: ", time_start))
  
  
  ref_tbl <- generate_ref_table(tbl = dat_to_plot %>% 
                                  mutate(concept_id=as.integer(concept_id)) %>% 
                                  group_by(site),
                                id_col = 'concept_id',
                                denom = 'ct_concept',
                                name_col = 'concept_name',
                                time = TRUE)
  
  p <-dat_to_plot %>% filter(variable %in% filtered_var)  %>%
    mutate(concept_id=as.character(concept_id)) %>% 
    ggplot(aes(y = !!sym(output_value), x = time_start, color = concept_id,
               group=concept_id, text=text)) +
    geom_line() +
    facet_wrap((facet)) +
    labs(title = 'Concepts per Variable Over Time',
         color = 'concept_id') +
    theme_minimal() +
    scale_color_ssdqa()
  
  plot <- ggplotly(p, tooltip = "text")

  
  output <- list(plot, ref_tbl)
  
  
 
}

#' *Multi Site, Exploratory, Across Time*
#' 
#' Facets by main code (cdm or source) by default, with each line representing
#' a mapping code. using plotly so the legend is interactive and codes can be isolated
#' 
#' 
#' @param process_output dataframe output by `csd_process`
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' @param filtered_var the variable to perform the anomaly detection for
#' @param facet the variables by which you would like to facet the graph; defaults to NULL
#' 
#' @return a line graph with one facet per code displaying the proportion of mapped codes
#'         across the user selected time period
#' @return a reference table with total counts of each code across the entire user selected
#'         time period
#' 
csd_ms_exp_at <- function(process_output,
                             facet=NULL,
                             filtered_var = c('ibd','spondyloarthritis'),
                             filtered_concept = c(81893),
                             #multi_or_single_site = 'multi',
                             #vocab_tbl = vocabulary_tbl('concept'),
                             output_value='prop_concept'){
  
  output_value <- output_value
  
  site_num <- 
    process_output %>% ungroup() %>%select(site) %>% distinct() %>% pull()
  
  if(length(site_num)>1){
    facet <- facet %>% append('concept_id')
  } else {
    facet <- facet %>% append('variable')
  }
  
  
  dat_to_plot <- process_output %>% filter(variable %in% filtered_var,
                                           concept_id %in% filtered_concept) %>% 
    mutate(text=paste("Concept: ",concept_id,
                      "\nConcept Name: ",concept_name,
                      "\nSite: ",site,
                      "\nValue: ",!!sym(output_value),
                      "\nTime Point: ", time_start))
  
  
  ref_tbl <- generate_ref_table(tbl = dat_to_plot %>% 
                                  mutate(concept_id=as.integer(concept_id)) %>% 
                                  group_by(site),
                                id_col = 'concept_id',
                                denom = 'ct_concept',
                                name_col = 'concept_name',
                                time = TRUE)
  
  p <-dat_to_plot %>% 
    mutate(concept_id=as.character(concept_id)) %>% 
    ggplot(aes(y = !!sym(output_value), x = time_start, color = site,
               group=site, text=text)) +
    geom_line() +
    facet_wrap((facet)) +
    labs(title = 'Concepts per Site Over Time',
         color = 'Site') +
    theme_minimal() +
    scale_color_ssdqa()
  
  plot <- ggplotly(p, tooltip = "text")
  
  
  output <- list(plot, ref_tbl)
  
  
  
}


#' *Multi Site, Exploratory, No Time*
#' 
#' @param process_output dataframe output by `csd_process`
#' @param vocab_tbl OPTIONAL: the location of an external vocabulary table containing concept names for
#'                  the provided codes. if not NULL, concept names will be available in either a reference
#'                  table or in a hover tooltip
#' @param facet the variables by which you would like to facet the graph
#' @param num_codes the number of top codes of code_type that should be displayed in the graph
#' 
#' @return a searchable and filterable table with mappings, proportion of representation, and
#'         denominator counts for the number of codes selected
#'         in @num_codes
#'         concept name will be included if @vocab_tbl is not NULL
#' 
csd_ms_exp_nt <- function(process_output,
                          facet,
                          #vocab_tbl = vocabulary_tbl('concept'),
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
  
  table <- 
    final_filt %>%
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
    data_color(palette = ssdqa_colors_standard, columns = c(all_of(facet))) %>%
    tab_header(title = paste0('All Available Mappings for Top ', num_codes, ' Variables')) %>%
    opt_interactive(use_search = TRUE,
                    use_filters = TRUE) 
    
  
  return(table)
  
}


#' *Multi-Site Anomaly No Time*
#' 
#' @param process_output output from `csd_process`
#' @param vocab_tbl if desired, the destination of an external vocabulary table to pull in
#'                  concept names
#' @param text_wrapping_char the number of characters for the `concept_name` or `concept_id` to 
#'                           display on heatmap; limited to 80
#' @param filtered_var the variable to perform the analysis on from the data frame; column name
#'                     that contains the variable names should be labeled `variable`
#' @param comparison_col the column that computes the quantitative value for comparison across sites;
#'                       in `csd` check, it is the `prop_concept`
#' @param grouped_vars a vector containing the variables to group by to compute the mean across the sites;
#'                     in `csd` check, defaulted to c(`variable`, `concept_id`)
#'                  
#'                  

csd_ms_anom_nt<-function(process_output,
                        #vocab_tbl,
                        text_wrapping_char=80,
                        filtered_var='ibd',
                        comparison_col='prop_concept',
                        grouped_vars=c('variable','concept_id')){
  
  grouped_vars <- grouped_vars %>% append('variable') %>% append('concept_id') %>% unique()
  
  mean_tbl <- 
    compute_dist_mean_median(tbl=process_output,
                             grp_vars = grouped_vars,
                             var_col = comparison_col,
                             num_sd=2,
                             num_mad=2)
  
  cname_samp <- mean_tbl %>% head(1) %>% select(concept_name) %>% pull()
  
  if(cname_samp == 'No vocabulary table input'){
    concept_label <- 'concept_id'
    }else{concept_label <- 'concept_name'}
  
  comparison_col = comparison_col
  
  dat_to_plot <- mean_tbl %>% filter(variable == filtered_var) %>% 
    mutate(text=paste("Concept: ",!!sym(concept_label),
                      "\nSite: ",site,
                      "\nProportion: ",round(!!sym(comparison_col),2),
                      "\nMean proportion:",round(mean,2),
                      "\nMedian proportion: ",round(median,2),
                      "\nSD: ", round(sd,4)))
  
  
  mid<-(max(dat_to_plot[[comparison_col]],na.rm=TRUE)+min(dat_to_plot[[comparison_col]],na.rm=TRUE))/2
  
  plt<-ggplot(dat_to_plot %>% filter(variable == filtered_var), 
              aes(x=site, y=as.character(concept_id), text=text))+
    geom_point(aes(size=abs_diff_mean,colour=!!sym(comparison_col),shape=anomaly_yn))+
    #aes(stringr::str_wrap(!!sym(concept_label), 20)) +
    scale_shape_manual(values=c(20,8))+
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    #scale_color_gradient2(midpoint=mid,low='#8c510a',mid='#f5f5f5', high='#01665e')+
    scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
    theme_minimal() +
    labs(colour="Proportion",
         shape="Anomaly",
         y = "Concept",
         size="")+
    theme(axis.text.x = element_text(angle=60)) 
  
  ggplotly(plt, tooltip="text")
}

#' **Multi-Site Across Time Anomaly**
#' Produces graphs showing AUCs
#' 
#' @param process_output_graph output from `csd_process`
#' @param filter_concept the concept_id that should be used for the output
#' @return two graphs:
#'    1) line graph that shows the proportion of a 
#'    code across time computation with the AUC associated with each line
#'    2) bar graph with each site on the x-axis, and the y-axis the AUC value, 
#'    with a dotted line showing the all-site average
#' 
#' THIS GRAPH SHOWS ONLY ONE CONCEPT AT A TIME!
#' 

csd_ms_anom_at <- function(process_output,
                           filter_concept){
  
  filt_op <- process_output %>% filter(concept_id == filter_concept)
  
  allsites <- 
    filt_op %>% 
    select(time_start,concept_id,mean_allsiteprop) %>% distinct() %>% 
    rename(prop_concept=mean_allsiteprop) %>% 
    mutate(site='all site average') %>% 
    mutate(text_smooth=paste0("Site: ", site,
                       "\n","Proportion: ",prop_concept),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",prop_concept)) 
  
  dat_to_plot <- 
    filt_op %>% 
    mutate(text_smooth=paste0("Site: ", site,
                       "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop_concept,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean)) 
  
  p <- dat_to_plot %>%
    ggplot(aes(y = prop_concept, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    scale_color_ssdqa() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion of ', filter_concept, ' Across Time'))
  
  q <- dat_to_plot %>%
    ggplot(aes(y = prop_concept, x = time_start, color = site,
               group=site, text=text_raw)) +
    scale_color_ssdqa() +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion of ', filter_concept, ' Across Time'))
  
  t <- dat_to_plot %>% 
    distinct(site, dist_eucl_mean, site_loess) %>% 
    group_by(site, dist_eucl_mean) %>% 
    summarise(mean_site_loess = mean(site_loess)) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess)) + 
    geom_col() + 
    geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
              show.legend = FALSE) +
    coord_radial(r_axis_inside = FALSE, rotate_angle = TRUE) + 
    guides(theta = guide_axis_theta(angle = 0)) +
    #scale_y_continuous(limits = c(-1,ylim_max)) + 
    theme_minimal() + 
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme(legend.position = 'bottom',
          axis.text.x = element_text(face = 'bold')) + 
    labs(fill = 'Avg. Proportion \n(Loess)', 
         y ='Euclidean Distance', 
         x = '', 
         title = paste0('Euclidean Distance for ', filter_concept))
  
  plotly_p <- ggplotly(p,tooltip="text")
  plotly_q <- ggplotly(q,tooltip="text")
  
  output <- list(plotly_p,
                 plotly_q,
                 t)
  
  return(output)
}