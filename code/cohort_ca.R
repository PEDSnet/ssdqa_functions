

#' Compute difference between attrition steps
#'
#' @param attrition_tbl CSV file with attrition information - should include the following columns:
#'                      
#'                      num_pts | step_number | attrition_step | qry_site
#' @param start_step_num integer indicating the number of the "start" step against which other steps should
#'                       be compared
#'
#' @return the attrition information plus columns that describe the patient drop & percent difference between
#'         each step and between each step and step 0
#'

compute_attrition_diff <- function(attrition_tbl,
                                   start_step_num = 0){
  
  if(! 'qry_site' %in% colnames(attrition_tbl)){attrition_tbl$site <- 'attrition_site'}
  
  pct_prior <- attrition_tbl %>%
    filter(step_number >= start_step_num) %>%
    group_by(qry_site) %>%
    arrange(step_number) %>%
    mutate(
      percent_retained_prior = 100 * num_pts / lag(num_pts),
      ct_diff_prior = lag(num_pts) - num_pts,
      percent_diff_prior = 100 * (lag(num_pts) - num_pts) / lag(num_pts)
    ) %>%
    ungroup()
  
  step0_cts <- attrition_tbl %>%
    filter(step_number == start_step_num) %>%
    rename('step0_pts' = num_pts) %>%
    select(qry_site, step0_pts)
  
  pct_step0 <- attrition_tbl %>%
    left_join(step0_cts) %>%
    mutate(percent_retained_start = 100 * num_pts / step0_pts) %>%
    select(-step0_pts)
  
  final_attrition <- attrition_tbl %>%
    filter(step_number >= start_step_num) %>%
    left_join(pct_prior) %>%
    left_join(pct_step0) %>%
    arrange(step_number)
  
  return(final_attrition)
}


combine_attritions <- function(site_list = c('seattle', 'colorado', 'chop'),
                               file_directory = paste0(base_dir, '/results/')){
  
  attrition_list <- list()
  
  for(i in 1:length(site_list)){
    
    file_name <- paste0(site_list[i], '_diabetes_attrition.csv')
    
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
#'               options are: num_pts, percent_retained_prior, percent_retained_start
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
  }else if(output == 'percent_retained_start'){
    title = 'Percent Retained from Start'
  }else if(output == 'percent_retained_prior'){
    title = 'Percent Retained from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts} or {.code percent_step0}")}
  
  min_step <- process_output %>% filter(step_number == min(step_number)) %>% pull(step_number)
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% pull(step_number)
  
  if(log_scale){
    
    grph <- ggplot(process_output %>% mutate(text = paste0('Step: ', attrition_step,
                                                           '\nCount: ', num_pts,
                                                           '\n % Retained: ', percent_retained_start)), 
                   aes(y = !!sym(output), x = step_number)) +
      geom_line(color = 'gray') +
      geom_point_interactive(aes(color = as.character(step_number), tooltip = text), show.legend = FALSE) +
      scale_y_continuous(transform = 'log2') +
      scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
      labs(y = paste0(title, ' (Log)'),
           x = 'Step',
           title = paste0(title, ' per Attrition Step')) +
      theme_minimal() +
      scale_color_ssdqa()
    
    grph_int <- girafe(ggobj = grph)
    
  }else{
    grph <- ggplot(process_output %>% mutate(text = paste0('Step: ', attrition_step,
                                                           '\nCount: ', num_pts,
                                                           '\n % Retained: ', round(percent_retained_start, 2), '%')), 
                   aes(y = !!sym(output), x = step_number)) +
      geom_line(color = 'gray') +
      geom_point_interactive(aes(color = as.character(step_number), tooltip = text), show.legend = FALSE) +
      scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(y = title,
           x = 'Step',
           title = paste0(title, ' per Attrition Step')) +
      theme_minimal() +
      scale_color_ssdqa()
    
    grph_int <- girafe(ggobj = grph)
  }
  
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



ca_ms_exp_nt <- function(process_output,
                         log_scale = FALSE,
                         output = 'num_pts'){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))
  
  if(output == 'num_pts'){
    title = 'Patient Count'
  }else if(output == 'percent_retained_start'){
    title = 'Percent Retained from Start'
  }else if(output == 'percent_retained_prior'){
    title = 'Percent Retained from Prior Step'
  }else{cli::cli_abort("Please select a valid output option: {.code num_pts} or {.code percent_step0}")}
  
  min_step <- process_output %>% filter(step_number == min(step_number)) %>% pull(step_number) %>% unique()
  max_step <- process_output %>% filter(step_number == max(step_number)) %>% pull(step_number) %>% unique()
  
  
  allsite_med <- process_output %>%
    group_by(step_number, attrition_step) %>%
    summarise(allsite_median = median(!!sym(output)),
              qry_site = 'All Site Median',
              text = paste0('Site: ', qry_site,
                            '\nMedian Value: ', allsite_median)) %>%
    rename(!!output := allsite_median)
  
  if(log_scale){
    
    grph <- ggplot(process_output %>% mutate(text = paste0('Site: ', qry_site,
                                                           '\nStep: ', attrition_step,
                                                           '\nCount: ', num_pts,
                                                           '\n % Retained Start: ', percent_retained_start)), 
                   aes(y = !!sym(output), x = step_number, color = qry_site, group = qry_site)) +
      geom_line() +
      geom_line(data = allsite_med, linewidth = 1.1) +
      geom_point_interactive(aes(tooltip = text), show.legend = FALSE) +
      scale_y_continuous(transform = 'log2') +
      scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
      labs(y = paste0(title, ' (Log)'),
           x = 'Step',
           color = 'Site',
           title = paste0(title, ' per Attrition Step')) +
      theme_minimal() +
      scale_color_ssdqa()
    
    grph_int <- girafe(ggobj = grph)
    
  }else{
    grph <- ggplot(process_output %>% mutate(text = paste0('Site: ', qry_site,
                                                           '\nStep: ', attrition_step,
                                                           '\nCount: ', num_pts,
                                                           '\n % Retained Start: ', percent_retained_start)), 
                   aes(y = !!sym(output), x = step_number, color = qry_site, group = qry_site)) +
      geom_line() +
      geom_line(data = allsite_med, linewidth = 1.1) +
      geom_point_interactive(aes(tooltip = text), show.legend = FALSE) +
      scale_x_continuous(breaks = seq(min_step, max_step, 1)) +
      scale_y_continuous(labels = scales::comma) +
      labs(y = title,
           x = 'Step',
           color = 'Site',
           title = paste0(title, ' per Attrition Step')) +
      theme_minimal() +
      scale_color_ssdqa()
    
    grph_int <- girafe(ggobj = grph)
  }
  
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
