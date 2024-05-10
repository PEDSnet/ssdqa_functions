

#' Compute difference between attrition steps
#'
#' @param attrition_tbl CSV file with attrition information - should include the following columns:
#'                      
#'                      num_pts | step_number | attrition_step | site
#' @param start_step_num integer indicating the number of the "start" step against which other steps should
#'                       be compared
#' @param site_col the column in the attrition table with the name(s) of the site(s)
#'
#' @return the attrition information plus columns that describe the patient drop & percent difference between
#'         each step and between each step and step 0
#'

compute_attrition_diff <- function(attrition_tbl,
                                   start_step_num = 0,
                                   site_col = 'site'){
  
  #if(! site_col %in% colnames(attrition_tbl)){attrition_tbl$site <- 'attrition_site'}
  
  pct_prior <- attrition_tbl %>%
    filter(step_number >= start_step_num) %>%
    group_by(!!sym(site_col)) %>%
    arrange(step_number) %>%
    mutate(
      prop_retained_prior = num_pts / lag(num_pts),
      ct_diff_prior = lag(num_pts) - num_pts,
      prop_diff_prior = (lag(num_pts) - num_pts) / lag(num_pts)
    ) %>%
    ungroup()
  
  step0_cts <- attrition_tbl %>%
    filter(step_number == start_step_num) %>%
    rename('step0_pts' = num_pts) %>%
    select(qry_site, step0_pts)
  
  pct_step0 <- attrition_tbl %>%
    left_join(step0_cts) %>%
    mutate(prop_retained_start = num_pts / step0_pts) %>%
    select(-step0_pts)
  
  final_attrition <- attrition_tbl %>%
    filter(step_number >= start_step_num) %>%
    left_join(pct_prior) %>%
    left_join(pct_step0) %>%
    arrange(step_number)
  
  return(final_attrition)
}


#' Combine attritions from multiple sites
#' 
#' This function reads in CSV files with a naming structure of site_file_suffix.csv and
#' combines them into a master file with data from all provided sites
#'
#' @param site_list list of all sites for which there are attrition files
#' @param file_directory the directory holding all of the attrition files
#' @param file_suffix the suffix of the attrition files
#'
#' @return a combined dataframe with attrition information from all of the sites
#'         provided in site_list
#'
combine_attritions <- function(site_list = c('seattle', 'colorado', 'chop', 'cchmc'),
                               file_directory = paste0(base_dir, '/results/'),
                               file_suffix = '_diabetes_attrition'){
  
  attrition_list <- list()
  
  for(i in 1:length(site_list)){
    
    file_name <- paste0(site_list[i], file_suffix, '.csv')
    
    file <- read_csv(file = paste0(file_directory, file_name))
    
    
    attrition_list[[i]] <- file
    
  }
  
  combined_attrition <- reduce(.x = attrition_list,
                               .f = dplyr::union)
  
  return(combined_attrition)
  
}


#' Cohort Attrition Single Site Exploratory No Time
#'
#' @param process_output output from compute_attrition_diff
#' @param log_scale logical to determine whether a log transform should be applied
#'                  to the y axis
#' @param output the column that should be used as the y-axis:
#'               
#'               options are: 
#'               `num_pts` (raw patient count), 
#'               `prop_retained_start` (proportion patients retained from starting step),
#'               `prop_retained_prior` (proportion patients retained from prior step),
#'               `prop_diff_prior` (proportion difference between each step and the prior step)
#'
#' @return a line graph with the output value for each step and an accompanying 
#'         table with the full descriptions of each step
#' 
ca_ss_exp_nt <- function(process_output,
                         log_scale = FALSE,
                         output = 'num_pts'){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output == 'num_pts'){
    title = 'Patient Count'
  }else if(output == 'prop_retained_start'){
    title = 'Proportion Retained from Start'
  }else if(output == 'prop_retained_prior'){
    title = 'Proportion Retained from Prior Step'
  }else if(output == 'prop_diff_prior'){
    title = 'Proportion Difference from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts}, {.code prop_retained_start}, {.code prop_retained_prior}, 
                       or {.code prop_diff_prior}")}
  
  min_step <- process_output %>% filter(step_number == min(step_number)) %>% pull(step_number)
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% pull(step_number)
    
    grph <- ggplot(process_output %>% mutate(text = paste0('Step: ', attrition_step,
                                                           '\nPatient Count: ', formatC(num_pts, format = 'd', big.mark = ','),
                                                           '\n',output, ': ', round(!!sym(output), 4))), 
                   aes(y = !!sym(output), x = step_number)) +
      geom_line(color = 'gray') +
      geom_point_interactive(aes(color = as.character(step_number), tooltip = text), show.legend = FALSE) +
      #scale_y_continuous(transform = 'log2') +
      scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
      labs(y = title,
           x = 'Step') +
      theme_minimal() +
      scale_color_ssdqa()
    
    if(log_scale){grph <- grph + scale_y_continuous(transform = 'log') + ggtitle(paste0(title, ' per Attrition Step (Log)'))}
    if(!log_scale){grph <- grph + ggtitle(paste0(title, ' per Attrition Step'))}
    
    grph_int <- girafe(ggobj = grph)
    
    ## Trying tornado plot
    
    process_output <- process_output %>% mutate(step_number = as.character(step_number))
    
    lvls <- stringr::str_sort(unique(process_output$step_number), numeric = TRUE, decreasing = TRUE)
    process_output$step_number <- factor(process_output$step_number, levels = lvls)
    
    tornado <- ggplot(process_output %>% mutate(text = paste0('Step: ', attrition_step,
                                                              '\nPatient Count: ', formatC(num_pts, format = 'd', big.mark = ','),
                                                              '\n',output, ': ', round(!!sym(output), 4))), 
                      aes(x = !!sym(output), y = step_number, fill = step_number)) +
      geom_col_interactive(aes(tooltip = text)) +
      scale_fill_ssdqa() +
      #scale_y_reverse(breaks = seq(min_step, max_step, 1)) +
      labs(x = title,
           y = 'Step') +
      theme_minimal()
    
    if(log_scale){tornado <- tornado + scale_x_continuous(transform = 'log') + ggtitle(paste0(title, ' per Attrition Step (Log)'))}
    if(!log_scale){tornado <- tornado + ggtitle(paste0(title, ' per Attrition Step'))}
    
    tornado_int <- girafe(ggobj = tornado)
  
  tbl <- process_output %>%
    distinct(step_number, attrition_step) %>%
    gt() %>%
    cols_label('step_number' = 'Step Number',
               'attrition_step' = 'Description') %>%
    opt_stylize(style = 2) %>%
    #opt_interactive() %>%
    tab_header('Attrition Step Reference')
  
  output <- list(grph_int,
                 tornado_int,
                 tbl)
  
  
  return(output)
  
}



#' Cohort Attrition Multi Site Exploratory No Time
#'
#' @param process_output output from compute_attrition_diff
#' @param log_scale logical to determine whether a log transform should be applied
#'                  to the y axis
#' @param output the column that should be used as the y-axis:
#'               
#'               options are: 
#'               `num_pts` (raw patient count), 
#'               `prop_retained_start` (proportion patients retained from starting step),
#'               `prop_retained_prior` (proportion patients retained from prior step),
#'               `prop_diff_prior` (proportion difference between each step and the prior step)
#'
#' @return a line graph with the output value for each step and an accompanying 
#'         table with the full descriptions of each step
#'         
ca_ms_exp_nt <- function(process_output,
                         log_scale = FALSE,
                         output = 'num_pts'){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output == 'num_pts'){
    title = 'Patient Count'
  }else if(output == 'prop_retained_start'){
    title = 'Proportion Retained from Start'
  }else if(output == 'prop_retained_prior'){
    title = 'Proportion Retained from Prior Step'
  }else if(output == 'prop_diff_prior'){
    title = 'Proportion Difference from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts}, {.code prop_retained_start}, {.code prop_retained_prior}, 
                       or {.code prop_diff_prior}")}
  
  min_step <- process_output %>% filter(step_number == min(step_number)) %>% pull(step_number) %>% unique()
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% pull(step_number) %>% unique()
  
  
  allsite_med <- process_output %>%
    group_by(step_number, attrition_step) %>%
    summarise(allsite_median = median(!!sym(output)),
              site = 'All Site Median',
              text = paste0('Site: ', site,
                            '\nMedian Value: ', allsite_median)) %>%
    rename(!!output := allsite_median)
    
  grph <- ggplot(process_output %>% mutate(text = paste0('Site: ', site,
                                                         '\nStep: ', attrition_step,
                                                         '\nPatient Count: ', formatC(num_pts, format = 'd', big.mark = ','),
                                                         '\n',output,': ', round(!!sym(output), 4))), 
                 aes(y = !!sym(output), x = step_number, color = site, group = site)) +
    geom_line() +
    geom_line(data = allsite_med, linewidth = 1.1) +
    geom_point_interactive(aes(tooltip = text), show.legend = FALSE) +
    scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
    labs(y = title,
         x = 'Step',
         color = 'Site') +
    theme_minimal() +
    scale_color_ssdqa()
  
  if(log_scale){grph <- grph + scale_y_continuous(transform = 'log') + ggtitle(paste0(title, ' per Attrition Step (Log)'))}
  if(!log_scale){grph <- grph + ggtitle(paste0(title, ' per Attrition Step'))}
  
  grph_int <- girafe(ggobj = grph)
    
  
  tbl <- process_output %>%
    distinct(step_number, attrition_step) %>%
    gt() %>%
    cols_label('step_number' = 'Step Number',
               'attrition_step' = 'Description') %>%
    opt_stylize(style = 2) %>%
    #opt_interactive() %>%
    tab_header('Attrition Step Reference')
  
  output <- list(grph_int,
                 tbl)
  
  
  return(output)
  
}



#' Cohort Attrition Multi Site Anomaly No Time (option 1)
#'
#' @param process_output output from compute_attrition_diff
#' @param output the column that should be used as the y-axis:
#'               
#'               options are: 
#'               `num_pts` (raw patient count), 
#'               `prop_retained_start` (proportion patients retained from starting step),
#'               `prop_retained_prior` (proportion patients retained from prior step),
#'               `prop_diff_prior` (proportion difference between each step and the prior step)
#'
#' @return a dot plot indicating anomalous sites based on the user selected output value
#' 
#'         anomalies are indicated by STARS, the color of each dot represents the raw output value,
#'         and the size of each dot represents the mean output value per attrition step
#' 
ca_ms_anom_nt1 <-function(process_output,
                          output){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output == 'num_pts'){
    title = 'Patient Count'
  }else if(output == 'prop_retained_start'){
    title = 'Proportion Retained from Start'
  }else if(output == 'prop_retained_prior'){
    title = 'Proportion Retained from Prior Step'
  }else if(output == 'prop_diff_prior'){
    title = 'Proportion Difference from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts}, {.code prop_retained_start}, {.code prop_retained_prior}, 
                       or {.code prop_diff_prior}")}
  
  min_step <- process_output %>% filter(step_number == min(step_number)) %>% pull(step_number) %>% unique()
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% pull(step_number) %>% unique()
  
  dat_to_plot <- process_output %>%
    mutate(text=paste("Step: ",attrition_step,
                      "\nSite: ",site,
                      "\n",output,": ",round(!!sym(output),4),
                      "\nMean: ",round(mean_val,2),
                      '\nSD: ', round(sd_val,2),
                      "\nMedian: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2)))
  
  plt<-ggplot(dat_to_plot %>% filter(anomaly_yn != 'no outlier in group'),
              aes(x=site, y=step_number, text=text, color=!!sym(output)))+
    geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
    geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'), 
                           aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c(19,8))+
    #scale_y_continuous(breaks = seq(min_step, max_step, 1)) +
    scale_y_reverse(breaks = seq(min_step, max_step, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=60)) +
    labs(y = "Variable",
         size="",
         title=paste0('Anomalous ', title, ' per Attrition Step'),
         subtitle = 'Dot size is the mean value per step') +
    guides(color = guide_colorbar(title = 'Proportion'),
           shape = guide_legend(title = 'Anomaly'),
           size = 'none')
  
  girafe(ggobj = plt)
}


#' Cohort Attrition Multi Site Anomaly No Time (option 2)
#'
#' @param process_output output from compute_attrition_diff
#' @param log_scale logical to determine whether a log transform should be applied
#'                  to the y axis
#' @param output the column that should be used as the y-axis:
#'               
#'               options are: 
#'               `num_pts` (raw patient count), 
#'               `prop_retained_start` (proportion patients retained from starting step),
#'               `prop_retained_prior` (proportion patients retained from prior step),
#'               `prop_diff_prior` (proportion difference between each step and the prior step)
#'
#' @return a line graph with the output value for each step and an accompanying 
#'         table with the full descriptions of each step
#'         
#'         anomalous values are indicated with a STAR
#'         
ca_ms_anom_nt2 <- function(process_output,
                           log_scale = FALSE,
                           output = 'num_pts'){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output == 'num_pts'){
    title = 'Patient Count'
  }else if(output == 'prop_retained_start'){
    title = 'Proportion Retained from Start'
  }else if(output == 'prop_retained_prior'){
    title = 'Proportion Retained from Prior Step'
  }else if(output == 'prop_diff_prior'){
    title = 'Proportion Difference from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts}, {.code prop_retained_start}, {.code prop_retained_prior}, 
                       or {.code prop_diff_prior}")}
  
  min_step <- process_output %>% filter(step_number == min(step_number)) %>% pull(step_number) %>% unique()
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% pull(step_number) %>% unique()
  
    
  grph <- ggplot(process_output %>% mutate(text = paste0(text=paste("Step: ",attrition_step,
                                                                    "\nSite: ",site,
                                                                    "\n",output,": ",round(!!sym(output),4),
                                                                    "\nMean: ",round(mean_val,2),
                                                                    '\nSD: ', round(sd_val,2),
                                                                    "\nMedian: ",round(median_val,2),
                                                                    "\nMAD: ", round(mad_val,2)))) %>%
                   filter(anomaly_yn != 'no outlier in group'), 
                 aes(y = !!sym(output), x = step_number, color = site, group = site)) +
    geom_line() +
    #geom_line(data = allsite_med, linewidth = 1.1) +
    geom_point_interactive(aes(tooltip = text, shape = anomaly_yn), show.legend = FALSE) +
    geom_point(data = process_output %>% filter(anomaly_yn == 'outlier'), shape = 8, size = 4) +
    scale_shape_manual(values=c(19,8))+
    scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
    labs(y = title,
         x = 'Step',
         color = 'Site') +
    theme_minimal() +
    scale_color_ssdqa()
  
  if(log_scale){grph <- grph + scale_y_continuous(transform = 'log') + ggtitle(paste0(title, ' per Attrition Step (Log)'))}
  if(!log_scale){grph <- grph + ggtitle(paste0(title, ' per Attrition Step'))}
    
  grph_int <- girafe(ggobj = grph)
    
  
  tbl <- process_output %>%
    distinct(step_number, attrition_step) %>%
    arrange(step_number) %>%
    gt() %>%
    cols_label('step_number' = 'Step Number',
               'attrition_step' = 'Description') %>%
    opt_stylize(style = 2) %>%
    #opt_interactive() %>%
    tab_header('Attrition Step Reference')
  
  output <- list(grph_int,
                 tbl)
  
  
  return(output)
  
}