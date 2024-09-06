
#' *Single Site, Exploratory, No Time*
#' 
prc_ss_exp_nt <- function(process_output){
  
  expand_cts <- process_output %>%
    uncount(pt_ct)
  
  avgs <- expand_cts %>%
    summarise(eventb_mean = mean(event_b_num),
              eventa_mean = mean(event_a_num))
  
  stat_labs <- expand_cts %>%
    mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                 event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                 event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                 event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
    group_by(site, event_a_name, event_b_name, total_pts, stat_type) %>%
    summarise(stat_ct = n()) %>%
    mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                               stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                               stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                               stat_type == 'Both Events' ~ 'Event Name: Both'),
           prop_event = stat_ct / total_pts,
           tooltip = paste0(tooltip, 
                            '\nProportion: ', round(prop_event, 3),
                            '\nPatient Count: ', format(stat_ct, big.mark = ','),
                            '\nAvg No. Event A: ', round(avgs$eventa_mean, 3),
                            '\nAvg No. Event B: ', round(avgs$eventb_mean, 3)))
  
  total_mult <- stat_labs %>% distinct(total_pts) %>% pull()
  
  g <- ggplot(stat_labs, aes(x = stat_type, y = prop_event, fill = stat_type)) +
    geom_col_interactive(aes(tooltip = tooltip), show.legend = FALSE) +
    scale_fill_ssdqa() +
    theme_minimal() +
    scale_y_continuous(sec.axis = sec_axis(~.*total_mult, name="Patient Count")) +
    labs(y = 'Proportion Patients',
         x = '',
         title = 'Proportion Patients with Each Event')
    
  girafe(ggobj = g)
  
}


#' *Multi Site, Exploratory, No Time*
#' 
prc_ms_exp_nt <- function(process_output){
  
  expand_cts <- process_output %>%
    uncount(pt_ct)
  
  avgs <- expand_cts %>%
    group_by(site) %>%
    summarise(eventb_mean = mean(event_b_num),
              eventa_mean = mean(event_a_num))
  
  stat_labs <- expand_cts %>%
    left_join(avgs) %>%
    mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                 event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                 event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                 event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
    group_by(site, event_a_name, event_b_name, total_pts, stat_type,
             eventa_mean, eventb_mean) %>%
    summarise(stat_ct = n()) %>%
    mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                               stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                               stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                               stat_type == 'Both Events' ~ 'Event Name: Both'),
           prop_event = stat_ct / total_pts,
           tooltip = paste0(tooltip, 
                            '\nProportion: ', round(prop_event, 3),
                            '\nAvg No. Event A: ', round(eventa_mean, 3),
                            '\nAvg No. Event B: ', round(eventb_mean, 3)))
  
  g <- ggplot(stat_labs, aes(y = site, x = prop_event, fill = site)) +
    geom_col_interactive(aes(tooltip = tooltip), show.legend = FALSE) +
    facet_wrap(~stat_type, ncol = 2) +
    scale_fill_ssdqa() +
    theme_minimal() +
    labs(y = 'Site',
         x = 'Proportion Patients',
         title = 'Proportion Patients with Each Event per Site')
  
  girafe(ggobj = g)
  
}


#' *Single Site, Exploratory, Across Time*
#' 
prc_ss_exp_at <- function(process_output){
  
  expand_cts <- process_output %>%
    uncount(pt_ct)
  
  avgs <- expand_cts %>%
    group_by(site, time_start, time_increment) %>%
    summarise(eventb_mean = mean(event_b_num),
              eventa_mean = mean(event_a_num))
  
  stat_labs <- expand_cts %>%
    left_join(avgs) %>%
    mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                 event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                 event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                 event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
    group_by(site, time_start, time_increment, event_a_name, event_b_name, total_pts, stat_type, 
             eventa_mean, eventb_mean) %>%
    summarise(stat_ct = n()) %>%
    mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                               stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                               stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                               stat_type == 'Both Events' ~ 'Event Name: Both'),
           prop_event = stat_ct / total_pts,
           tooltip = paste0(tooltip, 
                            '\nSite: ',site,
                            '\nProportion: ', round(prop_event, 3),
                            '\nAvg No. Event A: ', round(eventa_mean, 3),
                            '\nAvg No. Event B: ', round(eventb_mean, 3)))
  
  g <- ggplot(stat_labs, aes(x = time_start, y = prop_event, color = stat_type,
                             group = stat_type, text = tooltip)) +
    geom_line() +
    scale_color_ssdqa() +
    theme_minimal() +
    labs(y = 'Proportion Patients',
         x = '',
         title = 'Proportion Patients with Each Event')
  
  ggplotly(g, tooltip = 'text')
  
}


#' *Multi Site, Exploratory, Across Time*
#' 
prc_ms_exp_at <- function(process_output,
                          dist_from_stat = 'mean'){
  
  expand_cts <- process_output %>%
    uncount(pt_ct)
  
  avgs <- expand_cts %>%
    group_by(site, time_start, time_increment) %>%
    summarise(eventb_mean = mean(event_b_num),
              eventa_mean = mean(event_a_num))
  
  stat_labs <- expand_cts %>%
    left_join(avgs) %>%
    mutate(stat_type = case_when(event_a_num == 0 & event_b_num == 0 ~ 'Neither Event',
                                 event_a_num == 0 & event_b_num != 0 ~ 'Event B Only',
                                 event_a_num != 0 & event_b_num == 0 ~ 'Event A Only',
                                 event_a_num != 0 & event_b_num != 0 ~ 'Both Events')) %>%
    group_by(site, time_start, time_increment, event_a_name, event_b_name, total_pts, stat_type, 
             eventa_mean, eventb_mean) %>%
    summarise(stat_ct = n()) %>%
    mutate(tooltip = case_when(stat_type == 'Neither Event' ~ 'Event Name: Neither',
                               stat_type == 'Event A Only' ~ paste0('Event Name: ', event_a_name),
                               stat_type == 'Event B Only' ~ paste0('Event Name: ', event_b_name),
                               stat_type == 'Both Events' ~ 'Event Name: Both'),
           prop_event = stat_ct / total_pts)
  
  if(dist_from_stat == 'mean'){
    plot_dat <- stat_labs %>%
      group_by(time_start, time_increment, stat_type) %>%
      mutate(allsite_mean = mean(prop_event),
             dist_col = prop_event - allsite_mean,
             tooltip = paste0(tooltip, 
                            '\nDist. from Mean: ', round(dist_col, 3),
                            '\nRaw Proportion: ', round(prop_event, 3),
                            '\nAvg No. Event A: ', round(eventa_mean, 3),
                            '\nAvg No. Event B: ', round(eventb_mean, 3)))
    
    title_str <- 'All-Site Mean'
  }else if(dist_from_stat == 'median'){
    plot_dat <- stat_labs %>%
      group_by(time_start, time_increment, stat_type) %>%
      mutate(allsite_median = median(prop_event),
             dist_col = prop_event - allsite_median,
             tooltip = paste0(tooltip, 
                              '\nDist. from Median: ', round(dist_col, 3),
                              '\nRaw Proportion: ', round(prop_event, 3),
                              '\nAvg No. Event A: ', round(eventa_mean, 3),
                              '\nAvg No. Event B: ', round(eventb_mean, 3)))
    
    title_str <- 'All-Site Median'
  }else(cli::cli_abort('Invalid dist_from_stat: please choose either `mean` or `median`'))
  
  g <- ggplot(plot_dat, aes(x = time_start, y = dist_col, color = site,
                             group = site, text = tooltip)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = 'dotted', 
               alpha = 0.5) +
    facet_wrap(~stat_type, ncol = 2) +
    scale_color_ssdqa() +
    theme_minimal() +
    labs(y = 'Distance',
         x = '',
         title = paste0('Distance from ', title_str,' Proportion of Patients with Each Event'))
  
  ggplotly(g, tooltip = 'text')
  
}


#' *Single Site, Anomaly Detection, No Time*
#'

prc_ss_anom_nt <- function(process_output,
                           facet = NULL){
  
  
  dat_to_plot <- process_output %>%
    mutate(tooltip = paste0('Jaccard Index: ', round(jaccard_index, 3),
                            '\nEvent A: ', concept2,
                            '\nEvent B: ', concept1,
                            '\nPatients w/ Both: ', cocount,
                            '\nPatients w/ Either: ', concept_count_union))
  
  grph <- ggplot(dat_to_plot, aes(x = fu_bin, y = jaccard_index, fill = fu_bin,
                                  tooltip = tooltip)) +
    geom_col_interactive(show.legend = FALSE) +
    scale_fill_ssdqa() +
    theme_minimal() +
    labs(x = 'Length of F/U',
         y = 'Jaccard Similarity Index',
         title = 'Co-Occurrence of Events per Years of F/U')
  
  p <- girafe(ggobj=grph)
  
  return(p)
}


#' *Multi Site, Anomaly Detection, No Time*
#' 

prc_ms_anom_nt <- function(process_output){
  
  comparison_col = 'jaccard_index'
  
  check_n <- process_output %>%
    filter(anomaly_yn != 'no outlier in group')
  
  if(nrow(check_n) > 0){
    
    dat_to_plot <- process_output %>%
      mutate(text=paste("Years of F/U: ",fu_bin,
                        "\nSite: ",site,
                        "\nJaccard Index: ",round(!!sym(comparison_col),2),
                        "\nPatients w/ Both: ",cocount,
                        "\nPatients w/ Either: ",concept_count_union,
                        "\nMean Index:",round(mean_val,2),
                        #'\nSD: ', round(sd_val,2),
                        "\nMedian Index: ",round(median_val,2)
                        #"\nMAD: ", round(mad_val,2)
             )) %>%
      filter(anomaly_yn != 'no outlier in group')
  
    #mid<-(max(dat_to_plot[[comparison_col]],na.rm=TRUE)+min(dat_to_plot[[comparison_col]],na.rm=TRUE))/2
    
    plt<-ggplot(dat_to_plot,
                aes(x=site, y=fu_bin, text=text, color=!!sym(comparison_col)))+
      geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
      geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'), 
                             aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
      scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
      scale_shape_manual(values=c(19,8))+
      scale_y_discrete(labels = label_wrap_gen()) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=60),
            axis.text.y = element_text(size = 5)) +
      labs(y = "Years of F/U",
           size="",
           title=paste0('Anomalous Event Co-Occurrence per Years of F/U by Site'),
           subtitle = 'Dot size is the mean Jaccard index per F/U group') +
      guides(color = guide_colorbar(title = 'Jaccard Index'),
             shape = guide_legend(title = 'Anomaly'),
             size = 'none')
    
    girafe(ggobj = plt)
  
  }else{
    
    dat_to_plot <- process_output %>%
      mutate(text=paste("Years of F/U: ",fu_bin,
                        "\nSite: ",site,
                        "\nJaccard Index: ",round(!!sym(comparison_col),2),
                        "\nPatients w/ Both: ",cocount,
                        "\nPatients w/ Either: ",concept_count_union,
                        "\nMean Index:",round(mean_val,2),
                        #'\nSD: ', round(sd_val,2),
                        "\nMedian Index: ",round(median_val,2)
                        #"\nMAD: ", round(mad_val,2)
      )) 
    
    plt <- ggplot(dat_to_plot, aes(x = site, y = fu_bin, fill = jaccard_index,
                                   tooltip = text)) +
      geom_tile_interactive() +
      theme_minimal() +
      scale_fill_ssdqa(discrete = FALSE, palette = 'diverging')
    
    #' potential site anom identifiers:
    #' - average distance from mean/median across groups (average vs standard deviation)
    #'     - here would need to find distance from mean for all points, sqaure the distances
    #'       and average them, then take the square root of that average
    #' - try trinetx scoring method (will have to see how it performs for the tiny groups)
    #' - make a table of these or make a dot plot? dot plot would be mean at 0, outlying sites could
    #'   be 1/2 SDs away from mean?
    
    # Test Site Score using SD Computation
    test_site_score <- process_output %>%
      mutate(dist_mean = (!!sym(comparison_col) - mean_val)^2) %>%
      group_by(site) %>%
      summarise(n_grp = n(),
                dist_mean_sum = sum(dist_mean),
                overall_sd = sqrt(dist_mean_sum / n_grp)) %>%
      mutate(tooltip = paste0('Site: ', site,
                              '\nStandard Deviation: ', round(overall_sd, 3)))
    
    ylim_max <- test_site_score %>% filter(overall_sd == max(overall_sd)) %>% pull(overall_sd) + 1
    ylim_min <- test_site_score %>% filter(overall_sd == min(overall_sd)) %>% pull(overall_sd) - 1
    
    g2 <- ggplot(test_site_score, aes(y = overall_sd, x = site, color = site,
                                      tooltip = tooltip)) +
      geom_point_interactive(show.legend = FALSE) +
      theme_minimal() +
      scale_color_ssdqa() +
      geom_hline(yintercept = 0, linetype = 'solid') +
      #geom_hline(yintercept = 1, linetype = 'dotted', color = 'gray', linewidth = 1) +
      #geom_hline(yintercept = -1, linetype = 'dotted', color = 'gray', linewidth = 1) +
      #ylim(ylim_min, ylim_max) +
      labs(title = 'Average Standard Deviation per Site')
    
    
    # Test Site Score using TriNetX anomaly detection
    
    test_site_score2 <- trinetx_anom_detect(dat = process_output,
                                            var_col = comparison_col,
                                            grp_vars = c('fu_bin')) %>%
      mutate(tooltip = paste0('Site: ', site,
                              '\nSite Anomaly Score: ', round(site_score, 3)))
    
    g3 <- ggplot(test_site_score2, aes(y = site_score, x = site, color = site,
                                       tooltip = tooltip)) +
      geom_point_interactive(show.legend = FALSE) +
      theme_minimal() +
      scale_color_ssdqa() +
      labs(title = 'Site Outlier Score (TriNetX Method)')
    
    opt <- list(girafe(ggobj = plt),
                girafe(ggobj = g2),
                girafe(ggobj = g3))
    
    return(opt)
    
  }
  
  
}


#' *Single Site, Anomaly Detection, Across Time*
#' 

prc_ss_anom_at <- function(process_output,
                           event_filter,
                           facet = NULL){
  
  time_inc <- process_output %>% ungroup() %>% distinct(time_increment) %>% pull()
  
  dat_to_plot <- process_output %>%
    mutate(filt_col = case_when(tolower(event_filter) == 'a' ~ 'Event A Only',
                                tolower(event_filter) == 'b' ~ 'Event B Only',
                                tolower(event_filter) == 'neither' ~ 'Neither Event',
                                tolower(event_filter) == 'both' ~ 'Both Events')) %>%
    filter(filt_col == stat_type)
  
  title <- dat_to_plot %>% ungroup() %>% distinct(filt_col) %>% pull()
  
  if(time_inc == 'year'){
      
    facet <- facet %>% append('stat_type') %>% unique()
    
    dat_to_plot <- dat_to_plot %>%
      unite(facet_col, !!!syms(facet), sep = '\n') %>%
      rename('ycol' = stat_ct,
             'denom' = total_pts)
    
    pp_qi <-  qic(data = dat_to_plot, x = time_start, y = ycol, chart = 'pp', facet = ~facet_col,
                  title = paste0('Control Chart: Proportion of Patients with ', title), 
                  ylab = 'Proportion', xlab = 'Time',
                  show.grid = TRUE, n = denom)
    
    op_dat <- pp_qi$data
    
    new_pp <- ggplot(op_dat,aes(x,y)) +
      geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
      geom_line(colour = ssdqa_colors_standard[[12]], size = .5) +  
      geom_line(aes(x,cl)) +
      geom_point(colour = ssdqa_colors_standard[[6]] , fill = ssdqa_colors_standard[[6]], size = 1) +
      geom_point(data = subset(op_dat, y >= ucl), color = ssdqa_colors_standard[[3]], size = 2) +
      geom_point(data = subset(op_dat, y <= lcl), color = ssdqa_colors_standard[[3]], size = 2) +
      facet_wrap(~facet1, scales = 'free_y', ncol = 2) +
      ggtitle(label = paste0('Control Chart: Proportion of Patients with ', title)) +
      labs(x = 'Time',
           y = 'Proportion')+
      theme_minimal()
    
    output <- ggplotly(new_pp)
  
  }else{
    
    anomalies <- 
      plot_anomalies(.data=dat_to_plot,
                     .date_var=time_start) %>% 
      layout(title = paste0('Anomalies for ', title))
    
    decomp <- 
      plot_anomalies_decomp(.data=process_output,
                            .date_var=time_start) %>% 
      layout(title = paste0('Anomalies for ', title))
    
    output <- list(anomalies, decomp)
    
  }
  
  return(output)
  
}


#' *Multi Site, Anomaly Detection, Across Time*
#' 

prc_ms_anom_at <- function(process_output,
                           event_filter){
  
  filt_op <- process_output %>%
    mutate(filt_col = case_when(tolower(event_filter) == 'a' ~ 'Event A Only',
                                tolower(event_filter) == 'b' ~ 'Event B Only',
                                tolower(event_filter) == 'neither' ~ 'Neither Event',
                                tolower(event_filter) == 'both' ~ 'Both Events')) %>%
    filter(filt_col == stat_type) %>%
    mutate(prop_col = prop_event)
  
  title <- filt_op %>% ungroup() %>% distinct(filt_col) %>% pull()
  
  allsites <- 
    filt_op %>% 
    select(time_start,stat_type,mean_allsiteprop) %>% distinct() %>% 
    rename(prop_col=mean_allsiteprop) %>% 
    mutate(site='all site average') %>% 
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Proportion: ",prop_col),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",prop_col)) 
  
  dat_to_plot <- 
    filt_op %>% 
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop_col,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean)) 
  
  p <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    scale_color_ssdqa() +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion of ', title, ' Across Time'))
  
  q <- dat_to_plot %>%
    ggplot(aes(y = prop_col, x = time_start, color = site,
               group=site, text=text_raw)) +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    scale_color_ssdqa() +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion of ', title, ' Across Time'))
  
  t <- dat_to_plot %>% 
    distinct(site, dist_eucl_mean, site_loess) %>% 
    group_by(site, dist_eucl_mean) %>% 
    summarise(mean_site_loess = mean(site_loess)) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess)) + 
    geom_col() + 
    geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
              show.legend = FALSE) +
    coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) + 
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() + 
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme(legend.position = 'bottom',
          legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
          axis.text.x = element_text(face = 'bold')) + 
    labs(fill = 'Avg. Proportion \n(Loess)', 
         y ='Euclidean Distance', 
         x = '', 
         title = paste0('Euclidean Distance for ', title))
  
  plotly_p <- ggplotly(p,tooltip="text")
  plotly_q <- ggplotly(q,tooltip="text")
  
  output <- list(plotly_p,
                 plotly_q,
                 t)
  
  return(output)
  
  
  
}