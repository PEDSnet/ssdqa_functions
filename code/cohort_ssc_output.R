

#' 
#'
#' @param process_output LIST output from processing function
#' @param cohort_overlap 
#' @param alt_cohort_filter
#'
#' @return
#' @export
#'
#' @examples
ssc_ss_exp_nt <- function(summary_output,
                          cohort_overlap,
                          alt_cohort_filter){
  
  if(length(alt_cohort_filter) > 3){
    cli::cli_abort('Please limit alternate cohort definitions to three or less')}
  
  upset_prep <- cohort_overlap %>%
    ungroup() %>%
    mutate(cohort_group = str_remove_all(cohort_group, 'alt_cohort_')) %>%
    select(cohort_group, group_ct) %>%
    deframe()
  
  ug <- upset(fromExpression(upset_prep),
              empty.intersections = TRUE,
              main.bar.color = ssdqa_colors_standard[8],
              sets.bar.color = ssdqa_colors_standard[6],
              text.scale = 1.7,
              matrix.color = ssdqa_colors_standard[6])
  
  
  bc_cts <- summary_output %>%
    filter(cohort_id == 'base_cohort') %>%
    pivot_wider(names_from = cohort_id,
                values_from = fact_summary) %>%
    select(-cohort_total_pt)
    
  alt_cts <- summary_output %>%
    filter(cohort_id %in% alt_cohort_filter) %>%
    left_join(bc_cts) %>%
    mutate(diff_base = fact_summary - base_cohort,
           val_type = case_when(grepl('prop', cohort_characteristic) ~ 'Proportions',
                                grepl('median', cohort_characteristic) ~ 'Medians')) %>%
    filter(!is.na(val_type)) %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'prop_|median_'))
  
  
  dat_to_plot <- alt_cts %>%
    mutate(cohort_characteristic = case_when(cohort_characteristic == 'fu' ~ 'follow-up',
                                             TRUE ~ cohort_characteristic),
           cohort_characteristic = str_remove(cohort_characteristic, 'median_|prop_'),
           cohort_characteristic = str_remove(cohort_characteristic, '_ppy'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_id = str_remove(cohort_id, 'alt_cohort_')) %>%
    arrange(fact_group) %>% mutate(cf = factor(cohort_characteristic, levels = unique(cohort_characteristic)))
  
  
  bg <- ggplot(dat_to_plot, 
               aes(x = diff_base, y = cf, fill = cohort_id)) +
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
  
  
  dat_to_plot <- process_output %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'median_|prop_'),
           cohort_characteristic = str_remove(cohort_characteristic, '_ppy'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_characteristic = case_when(cohort_characteristic == 'fu' ~ 'follow-up',
                                             TRUE ~ cohort_characteristic),
           cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = paste0('Alternate Cohort: \n', cohort_id))
  
  ggplot(dat_to_plot, aes(x = cohort_id, y = cohort_characteristic, 
                             fill = smd_vs_baseline)) +
    geom_tile() +
    geom_text(aes(label = round(smd_vs_baseline, 2))) +
    facet_grid(rows = vars(fact_group), 
               scales = 'free_y',switch = 'y', space = 'free_y',
               labeller = label_wrap_gen()) +
    theme_minimal() +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_y_discrete(expand = c(0,0)) +
    theme(panel.spacing = unit(0, 'lines'), 
          strip.background = element_rect(),
          strip.placement = "outside") +
    labs(title = 'Standardized Mean Difference versus Base Cohort',
         x = '',
         y = 'Cohort Characteristic',
         fill = 'SMD')
  
  
}



ssc_ms_exp_nt <- function(process_output){
  
  cat_vars <- process_output %>% filter(grepl('prop', cohort_characteristic))
  
  cont_vars <- process_output %>% filter(grepl('median', cohort_characteristic))
  
  cont_hm <- cont_vars %>%
    #pivot_longer(cols = !c(site, cohort_id)) %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'median_'),
           cohort_characteristic = str_remove(cohort_characteristic, '_ppy'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_characteristic = case_when(cohort_characteristic == 'fu' ~ 'follow-up',
                                             TRUE ~ cohort_characteristic),
           cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = case_when(cohort_id == 'base_cohort' ~ 'Base Cohort',
                                 cohort_id != 'base_cohort' ~ paste0('Alternate Cohort: ', cohort_id)),
           tooltip = paste0('Site: ', site,
                            '\nCharacteristic: ', cohort_characteristic,
                            '\nMedian PPY: ', fact_summary)) %>%
    arrange(fact_group) %>% mutate(cf = factor(cohort_characteristic, levels = unique(cohort_characteristic))) %>%
    ggplot(aes(x = site, y = cf, fill = fact_summary, tooltip = tooltip)) +
    geom_tile_interactive() +
    geom_text(aes(label = round(fact_summary, 2)), size = 2) +
    facet_wrap(~cohort_id, labeller = label_wrap_gen()) +
    scale_fill_ssdqa(discrete = FALSE, palette = 'diverging') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(y = 'Cohort Characteristic',
         x = 'Site',
         fill = 'Median (PPY)',
         title = 'Distribution of Median Facts PPY per Site')
  
  hm <- girafe(ggobj = cont_hm)
  
  cat_bars <- cat_vars %>%
    #pivot_longer(cols = !c(site, cohort_id)) %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'prop_'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = case_when(cohort_id == 'base_cohort' ~ 'Base Cohort',
                                 cohort_id != 'base_cohort' ~ paste0('Alternate Cohort: ', cohort_id)),
           tooltip = paste0('Site: ', site,
                            '\nCharacteristic: ', cohort_characteristic,
                            '\nProportion: ', round(fact_summary, 3))) %>%
    arrange(fact_group) %>% mutate(cf = factor(cohort_characteristic, levels = unique(cohort_characteristic))) %>%
    ggplot(aes(x = fact_summary, y = site, fill = cf, tooltip = tooltip)) +
    geom_col_interactive() +
    facet_wrap(~cohort_id, labeller = label_wrap_gen()) +
    scale_fill_ssdqa() +
    theme_minimal() +
    labs(fill = 'Cohort \nCharacteristic',
         y = 'Site',
         x = 'Proportion',
         title = 'Proportion of Patients with Characteristics')
    
  bg <- girafe(ggobj = cat_bars)
  
  otpt <- list(hm, bg)
  
  return(otpt)
  
}



ssc_ms_anom_nt <- function(process_output){
  
  dat_to_plot <- process_output %>%
    mutate(cohort_characteristic = str_remove(cohort_characteristic, 'median_|prop_'),
           cohort_characteristic = str_remove(cohort_characteristic, '_ppy'),
           cohort_characteristic = str_replace_all(cohort_characteristic, '_', ' '),
           cohort_characteristic = case_when(cohort_characteristic == 'fu' ~ 'follow-up',
                                             TRUE ~ cohort_characteristic),
           cohort_id = str_remove(cohort_id, 'alt_cohort_'),
           cohort_id = paste0('Alternate Cohort: ', cohort_id),
           val_type = ifelse(fact_group %in% c('Demographics', 'Outcomes'), 'Proportion', 'Median PPY'),
           tooltip = paste0('Site: ', site,
                            '\nCharacteristic: ', cohort_characteristic,
                            '\nValue Type: ', val_type,
                            '\nMSE (vs. Base): ', mse)) %>%
    arrange(fact_group) %>% 
    mutate(cf = factor(cohort_characteristic, levels = unique(cohort_characteristic)))
  
  grph <- ggplot(dat_to_plot, aes(x = site, y = cf, fill = mse,
                                  tooltip = tooltip)) + 
    geom_tile_interactive(color = 'gray') + 
    facet_grid(cols = vars(cohort_id), rows = vars(fact_group), 
               scales = 'free_y',switch = 'y', space = 'free_y',
               labeller = label_wrap_gen()) +
    theme_minimal() +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_y_discrete(expand = c(0,0)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          panel.spacing = unit(0, 'lines'), 
          strip.background = element_rect(),
          strip.placement = "outside") +
    labs(title = 'MSE of Alternate Cohorts compared to Base',
         x = 'Site',
         y = 'Cohort Characteristic',
         fill = 'MSE')
  
  girafe(ggobj = grph,
         height_svg = 8)
  
}