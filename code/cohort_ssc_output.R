

#' 
#'
#' @param process_output LIST output from processing function
#'
#' @return
#' @export
#'
#' @examples
ssc_ss_exp_nt <- function(process_output,
                          alt_cohort_filter){
  
  if(length(alt_cohort_filter) > 3){
    cli::cli_abort('Please limit alternate cohort definitions to three or less')}
  
  upset_prep <- process_output[[2]] %>%
    ungroup() %>%
    select(cohort_group, group_ct) %>%
    deframe()
  
  ug <- upset(fromExpression(upset_prep),
              empty.intersections = TRUE,
              main.bar.color = ssdqa_colors_standard[8],
              sets.bar.color = ssdqa_colors_standard[6],
              matrix.color = ssdqa_colors_standard[6])
  
  
  bc_cts <- process_output[[1]] %>%
    filter(cohort_id == 'base_cohort') %>%
    pivot_longer(cols = !c(site, cohort_id)) %>%
    pivot_wider(names_from = cohort_id,
                values_from = value)
    
  alt_cts <- process_output[[1]] %>%
    filter(cohort_id %in% alt_cohort_filter) %>%
    pivot_longer(cols = !c(site, cohort_id)) %>%
    left_join(bc_cts) %>%
    mutate(diff_base = value - base_cohort,
           val_type = case_when(grepl('prop', name) ~ 'Proportions',
                                grepl('median', name) ~ 'Medians')) %>%
    filter(!is.na(val_type))
  
  
  bg <- ggplot(alt_cts, aes(x = diff_base, y = name, fill = cohort_id)) +
    geom_col(position = 'dodge') +
    geom_label(aes(label = round(diff_base, 2)), position = position_dodge(width = 0.9),
               size = 3) +
    facet_wrap(~val_type, scales = 'free') +
    theme_minimal() +
    scale_fill_ssdqa() +
    labs(x = 'Difference from Base Cohort',
         y = 'Cohort Characteristic',
         fill = 'Alternate Cohort',
         title = 'Difference between Alternate and Base Cohorts')
  
  otpt <- list(ug, bg)
  
  return(otpt)
  
  
}



ssc_ss_anom_nt <- function(process_output){
  
  
  ggplot(process_output, aes(x = cohort_id, y = cohort_characteristic, 
                             fill = smd_vs_baseline)) +
    geom_tile() +
    geom_text(aes(label = round(smd_vs_baseline, 2))) +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme_minimal() +
    labs(y = 'Cohort Characteristic',
         x = 'Alternate Cohort Definition',
         fill = 'SMD',
         title = 'Standardized Mean Deviation Compared to Base Cohort')
  
  
}



ssc_ms_exp_nt <- function(process_output){
  
  cat_vars <- process_output %>% select(site, cohort_id, contains('prop_'))
  
  cont_vars <- process_output %>% select(site, cohort_id, contains('median_')) 
  
  cont_hm <- cont_vars %>%
    pivot_longer(cols = !c(site, cohort_id)) %>%
    mutate(name = str_remove(name, 'median_'),
           tooltip = paste0('Site: ', site,
                            '\nCharacteristic: ', name,
                            '\nMedian PPY: ', value)) %>%
    ggplot(aes(x = site, y = name, fill = value, tooltip = tooltip)) +
    geom_tile_interactive() +
    geom_text(aes(label = round(value, 2)), size = 3) +
    facet_wrap(~cohort_id) +
    scale_fill_ssdqa(discrete = FALSE, palette = 'diverging') +
    theme_minimal() +
    labs(y = 'Cohort Characteristic',
         x = 'Site',
         fill = 'Median (PPY)',
         title = 'Distribution of Median Facts PPY per Site')
  
  hm <- girafe(ggobj = cont_hm)
  
  cat_bars <- cat_vars %>%
    pivot_longer(cols = !c(site, cohort_id)) %>%
    mutate(name = str_remove(name, 'prop_'),
           tooltip = paste0('Site: ', site,
                            '\nCharacteristic: ', name,
                            '\nProportion: ', value)) %>%
    ggplot(aes(x = value, y = site, fill = name, tooltip = tooltip)) +
    geom_col_interactive() +
    facet_wrap(~cohort_id) +
    scale_fill_ssdqa() +
    theme_minimal()
    
  bg <- girafe(ggobj = cat_bars)
  
  otpt <- list(hm, bg)
  
  return(otpt)
  
}



ssc_ms_anom_nt <- function(process_output){
  
  dat_to_plot <- process_output %>%
    mutate(tooltip = paste0('Site: ', site,
                            '\nCharacteristic: ', cohort_char,
                            '\nMSE (vs. Base): ', mse))
  
  grph <- ggplot(dat_to_plot, aes(x = site, y = cohort_char, fill = mse,
                                  tooltip = tooltip)) + 
    geom_tile_interactive(color = 'gray') + 
    facet_wrap(~cohort_id) +
    theme_minimal() +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    labs(title = 'MSE of Alternate Cohorts compared to Base',
         x = 'Site',
         y = 'Cohort Characteristic',
         fill = 'MSE')
  
  girafe(ggobj = grph)
  
}