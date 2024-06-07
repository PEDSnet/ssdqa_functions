#' * Single Site, Exploratory, No Time*
#' Function to produce output for clinical event concordance with specialty 
#'      for single site, exploratory, not over time
#'      
#' @param data_tbl table with the data to plot
#' @param facet list of one or more variables to facet the plot on
#' @param x_var variable to plot on the x axis
#' @param y_var variable to plot on the y axis
#' @param fill_var variable to fill bars with
#' @param pal_map list of colors to associate with each of the values in `fill_var`
#' @return a plotly bar plot based on the values of `x_var`, `y_var`, `fill_var`, `facet`
plot_cnc_sp_ss_exp_nt <- function(data_tbl,
                            facet,
                            x_var,
                            y_var,
                            fill_var,
                            pal_map,
                            top_n) {
  
  data_tbl <- data_tbl %>%
    mutate(text=paste("Specialty: ", specialty_name,
                      "\nNumber of Visits: ",format(n,big.mark=","),
                      "\nProportion: ",round(prop,2)))
  facet_name<-str_remove_all(as.character(deparse(facet)),"\\(|\\\"|\\)")
  plt <- ggplot(data_tbl,
         aes(x=!!sym(x_var), y=!! sym(y_var), fill=!!sym(fill_var), text=text)) +
    geom_bar(stat='identity') + 
    facet_wrap((facet))+
    #scale_fill_manual(values=pal_map) +
    scale_fill_ssdqa()+
    coord_flip() +
    theme_classic() +
    theme(panel.grid.major = element_line(size=0.4, linetype = 'solid'),
          panel.grid.minor = element_line(size=0.2, linetype = 'dashed'))+
    labs(title=paste0("Proportion of visits with each of the top ",top_n," Specialties\nby ",facet_name))
  
  ggplotly(plt,
           tooltip="text")
  
  
}

#' Generate a color palette
#'   based on the length of distinct values to be mapped to a color
#' @param distinct_list a list of values for the variable to be color-fied
#' @return list with colors assigned to each value of variable
generate_color_pal <- function(distinct_list){
  ramp_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(length(distinct_list))
  randomized_palette <- ramp_palette[sample(1:length(ramp_palette))]
  setNames(randomized_palette, distinct_list)
}

#' Should be able to use this for other checks,
#' but naming this way to differentiate from
#' the existing `compute_dist_mean` function
#' @param tbl table with at least the vars specified in `grp_vars` and `var_col`
#' @param grp_vars variables to group by when computing summary statistics
#' @param var_col column to compute summary statistics on
#' @param num_sd (integer) number of standard deviations away from the mean
#'               from which to compute the sd_lower and sd_upper columns
#' @return a table with the `grp_vars` | mean | sd | sd_lower | sd_upper | 
#'                                      anomaly_yn: indicator of whether data point is +/- num_sd from mean
#'                                      abs_diff_mean: absolute value of difference between mean for group and observation
compute_dist_mean_conc <- function(tbl,
                                   grp_vars,
                                   var_col,
                                   num_sd,
                                   num_mad){
  stats <- tbl %>%
    group_by(!!!syms(grp_vars))%>%
    summarise(mean=mean(!!!syms(var_col)),
              median=median(!!!syms(var_col)),
              sd=sd(!!!syms(var_col), na.rm=TRUE),
              mad=mad(!!!syms(var_col),center=median)) %>%
    ungroup() %>%
    mutate(sd_lower=mean-num_sd*sd,
           sd_upper=mean+num_sd*sd,
           mad_lower=median-num_mad*mad,
           mad_upper=median+num_mad*mad)
  
  tbl %>%
    inner_join(stats)%>%
    mutate(anomaly_yn=case_when(!!sym(var_col)<sd_lower|!!sym(var_col)>sd_upper~TRUE,
                                TRUE~FALSE),
           abs_diff_mean=abs(!!sym(var_col)-mean),
           abs_diff_median=abs(!!sym(var_col)-median),
           n_mad=abs_diff_median/mad)
  
}



#' @param tbl table with at least the vars specified in `grp_vars` and `var_col`
#' @param grp_vars variables to group by when computing summary statistics
#' @param var_col column to compute summary statistics on
#' @param num_mad (integer) number of MAD from the MAD
#'               from which to compute the mad_lower and mad_upper columns
#' @return a table with the `grp_vars` | mean | sd | sd_lower | sd_upper | 
#'                                      anomaly_yn: indicator of whether data point is +/- num_mad from median
#'                                      abs_diff_median: absolute value of difference between median for group and observation
compute_dist_median_conc <- function(tbl,
                                   grp_vars,
                                   var_col,
                                   num_mad){
  stats <- tbl %>%
    group_by(!!!syms(grp_vars))%>%
    summarise(median=median(!!!syms(var_col)),
              mad=mad(!!!syms(var_col),center=median)) %>%
    ungroup() %>%
    mutate(mad_lower=median-num_mad*mad,
           mad_upper=median+num_mad*mad)
  
  tbl %>%
    inner_join(stats)%>%
    mutate(anomaly_yn=case_when(!!sym(var_col)<mad_lower|!!sym(var_col)>mad_upper~TRUE,
                                TRUE~FALSE),
           abs_diff_median=abs(!!sym(var_col)-median),
           n_mad=abs_diff_median/mad)
  
}

#' Function to limit output from `compute_dist_mean_conc`
#'     to only the specialties where there is at least
#'     one site with anomolous distance from the mean
#' @param tbl table with at least the columns:
#'              codeset_name | specialty_name
#' @param facet_vars variables to facet the plot by
#'                    if one of the facet vars is visit_type, will group by visit_type
#' @param distinct_vars variables to 
#' @return table with codeset_name | specialty_name | all columns in `tbl`
#'          for only the specialties with at least one anomaly
flag_anomaly<- function(tbl,
                        facet_vars,
                        distinct_vars){
  anomaly_tbl <- tbl %>%
    # anomaly_yn is created within compute_dist_median_conc
    filter(anomaly_yn) %>%
    distinct(!!!syms(distinct_vars)) 
  anomaly_all <- anomaly_tbl %>%
    inner_join(tbl)#%>%
    # mutate(anomaly=case_when(prop<sd_lower|prop>sd_upper~TRUE,
    #                          TRUE~FALSE))
  
  # if('visit_type'%in%facet_vars){
  #   anomaly_tbl <- tbl %>%
  #     filter(prop<sd_lower|prop>sd_upper) %>%
  #     distinct(codeset_name, specialty_name, visit_type)
  #   anomaly_all <- anomaly_tbl %>%
  #     inner_join(tbl, by = c('codeset_name', 'specialty_name', 'visit_type'))
  # }else{
  #   anomaly_tbl <- tbl %>%
  #     filter(prop<sd_lower|prop>sd_upper) %>%
  #     distinct(codeset_name, specialty_name)
  #   anomaly_all <- anomaly_tbl %>%
  #     inner_join(tbl, by = c('codeset_name', 'specialty_name'))
  # }
  # return(anomaly_all)
}

#' *Multi-Site, Exploratory, No Time*
#' Function to produce output for clinical event concordance with specialty 
#'      for multi site, exploratory, not over time
#'      
#' @param data_tbl table with the data to plot
#' @param pal_map list of colors to associate with each specialty name
#' @return a plotly dot plot of specialty against proportion of visits with that specialty
#'         at each site, with dot color representing site
plot_cnc_sp_ms_exp_nt <- function(data_tbl,
                                     pal_map){
  dat_to_plot<-data_tbl%>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nProportion: ",round(prop,2),
                      "\nSite: ",site))
  plt<-ggplot(dat_to_plot, aes(x=specialty_name,
                  y=prop,
                  colour=site,
                  text=text))+
    geom_point()+
    #scale_color_manual(values=pal_map)+
    scale_color_ssdqa()+
    coord_flip()+
    theme_bw()
  ggplotly(plt, tooltip = "text")
  
}

#' *Single Site, Exploratory, Across Time*
#' Function to produce output for clinical event concordance with specialty 
#'      for single site, exploratory, across time
#'      
#' @param data_tbl table with the data to plot
#' @param facet list of one or more variables to facet the plot on
#' @param pal_map list of colors to associate with each specialty
#' @return a plotly line plot of the proportion of visits with each specialty
#'          against time, with line color representing specialty
plot_cnc_sp_ss_exp_at <- function(data_tbl,
                                facet=NULL,
                                pal_map){
  dat_to_plot<-data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nProportion: ",round(prop,2),
                      "\nTime Start: ",time_start))
  
  if(is.null(facet)){
  plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=specialty_name,text=text))+
    geom_line(group=1)+
    #scale_color_manual(values=pal_map)+
    scale_color_ssdqa()+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))
  }else{  
    plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=specialty_name,text=text))+
      geom_line(group=1)+
      #scale_color_manual(values=pal_map)+
      scale_color_ssdqa()+
      facet_wrap(facets = eval(facet), scales = 'free')+
      theme_classic()+
      theme(axis.text.x = element_text(angle=90))
    }
  
  ggplotly(plt, tooltip="text")
}

#' Function to insert a column into a table
#'  indicating whether the record is in the top n
#'  for the given group
#' @param dat table containing the data
#' @param gp_cols columns to group by
#'                  the top n will be taken from each group, so the total indicated as "top" hits will be # of groups * n
#' @param val_col column to order by when determining the top n
#' @param n number of records to indicate as top within the group
#' @return the original `dat` table with all original columns,
#'         plus a column `top_n_indicator` which is TRUE if the record is in the top n
#'         and FALSE if not
insert_top_n_indicator<-function(dat,
                                 gp_cols,
                                 val_col,
                                 n,
                                 sum_first=FALSE){
  if(sum_first){
    top_hits <- dat %>%
      ungroup()%>%
      group_by(!!!syms(gp_cols))%>%
      summarise(sumn=sum(!!sym(val_col)))%>%
      ungroup()%>%
      slice_max(order_by = sumn,n=n)%>%
      ungroup()%>%
      mutate(top_n_indicator=TRUE)%>%
      select(-sumn)
  }else{
  top_hits <- dat %>%
    group_by(!!!syms(gp_cols))%>%
    slice_max(order_by = !!sym(val_col),n=n) %>%
    ungroup()%>%
    mutate(top_n_indicator=TRUE)
  }
    
  dat %>%
    left_join(top_hits)%>%
    mutate(top_n_indicator=case_when(is.na(top_n_indicator)~FALSE,
                                     TRUE~TRUE))
}

#' * Multi-Site, Exploratory, Across Time*
#' Function to produce output for clinical event concordance with specialty
#'      for multi site, exploratory, across time

#' @param data_tbl table which must contain the cols: time_start | codeset_name | specialty_name | site
#' @param facet if supplied, variable to facet the plot by
#' @param pal_map color palette for the variable that will be used to color the line
#' @return line plot, with time on x axis, proportion on y, line color determined by site
#'              with a dotted line for the all-site mean
plot_cnc_sp_ms_exp_at <- function(data_tbl,
                                facet=NULL,
                                pal_map){
  # compute all site mean
   all_site_mean <- data_tbl%>%
     group_by(time_start, codeset_name,specialty_name)%>%
     summarise(prop=mean(prop, na.rm=TRUE))%>%
     ungroup()%>%
     mutate(site="all")
   
   # set up scheme for line types: differentiate "all" from site names
   site_names<-data_tbl%>%distinct(site)%>%pull()
   n_sites<-length(site_names)
   line_vals<-c("dotted",rep("solid",n_sites))
   line_breaks<-c("all",site_names)
  
  dat_to_plot<-data_tbl %>%
    bind_rows(all_site_mean)%>%
    mutate(text=paste("Site: ",site,
                      "\nProportion of site's visits: ",round(prop,2),
                      "\nTime Start: ",time_start))

  
  if(is.null(facet)){
    plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=site,text=text))+
      geom_line(group=1,aes(linetype=site))+
      #scale_color_manual(values=pal_map)+
      scale_color_ssdqa()+
      scale_linetype_manual(values=line_vals,breaks=line_breaks)+
      theme_classic()+
      theme(axis.text.x = element_text(angle=90))
  }else{  
    plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=site,text=text))+
      geom_line(group=1,aes(linetype=site))+
      #scale_color_manual(values=pal_map)+
      scale_color_ssdqa()+
      facet_wrap(facets = eval(facet), scales = 'free')+
      scale_linetype_manual(values=line_vals,breaks=line_breaks)+
      theme_classic()+
      theme(axis.text.x = element_text(angle=90))
  }
  
  ggplotly(plt, tooltip="text")
}

#' Function to produce output for clinical event concordance with specialty
#'      for multi site, anomaly, across time

#' @param data_tbl table which must contain the cols: time_start | specialty_name | site
#' @return ggplot heatmap with time on the x axis, site on the y,
#'          and fill color representing the number of MAD from the across-site median
#'          for that specialty for the given year
plot_cnc_sp_ms_an_at_old<- function(data_tbl){
  plt<-ggplot(data_tbl, aes(x=time_start,y=site,fill=n_mad))+
    geom_tile()+
    facet_wrap(~specialty_name)+
    #scale_fill_gradient2(low='pink',high='maroon')+
    scale_fill_ssdqa(palette = "sequential", discrete=FALSE)+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))
  ggplotly(plt)
}

#' Function to produce output for clinical event concordance with specialty
#'      for multi site, anomaly, not over time

#' @param data_tbl table which must contain the cols: specialty_name | prop | median | n_mad
#' @return plotly dot plot with site on x axis, specialty on y axis
#'         with size of dot representing number of MAD from median
#'              color of dot representing proportion of visits with specialty
#'              shape of dot representing whether point is an anomaly
plot_cnc_sp_ms_an_nt<-function(data_tbl){
  dat_to_plot <- data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nSite: ",site,
                      "\nProportion: ",round(prop,2),
                      "\nMedian proportion: ",round(median,2),
                      "\nNo. MAD from median: ", round(n_mad,2)))
  
  mid<-(max(dat_to_plot$n_mad,na.rm=TRUE)+min(dat_to_plot$n_mad,na.rm=TRUE))/2
  
  plt<-ggplot(dat_to_plot, aes(x=site, y=specialty_name, text=text))+
    geom_point(aes(size=n_mad,colour=prop,shape=anomaly_yn))+
    scale_color_ssdqa(palette = "diverging", discrete=FALSE)+
    scale_shape_manual(values=c(20,8))+
    #scale_color_gradient2(midpoint=mid,low='#8c510a',mid='#f5f5f5', high='#01665e')+
    #scale_color_gradient(low='#dfc27d',high='#01665e')+
    theme_bw()+
    labs(colour="Proportion",
         shape="Anomaly",
         size="")+
    theme(axis.text.x = element_text(angle=90))
  
  
  ggplotly(plt, tooltip="text")
}

#' Function to produce output for clinical event concordance with specialty
#'      for single site, anomaly, not over time
#' @param data_tbl table which must contain the cols: specialty_name | prop | median | n_mad
#' @return plotly dot plot with specialty on x axis, cluster on y axis
#'         with size of dot representing number of MAD from median
#'              color of dot representing proportion of visits with specialty
#'              shape of dot representing whether point is an anomaly
plot_cnc_sp_ss_an_nt<- function(data_tbl){
  dat_to_plot <- data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nProportion: ",round(prop,2),
                      "\nMedian proportion: ",round(median,2),
                      "\nNo. MAD from median: ", round(n_mad,2)))
  #mid<-(max(dat_to_plot$prop,na.rm=TRUE)+min(dat_to_plot$prop,na.rm=TRUE))/2
  mid <- median(dat_to_plot$prop, na.rm=TRUE)
  #mid<-(max(dat_to_plot$n_mad,na.rm=TRUE)+min(dat_to_plot$n_mad,na.rm=TRUE))/2
  
  
  plt<-ggplot(dat_to_plot, aes(x=specialty_name, y=cluster, text=text))+
    geom_point(aes(size=n_mad,colour=prop,shape=anomaly_yn))+
    scale_shape_manual(values=c(20,8))+
    #scale_color_gradient2(midpoint=mid,low='#8c510a',mid='#f5f5f5', high='#01665e')+
    #scale_color_gradient(low='#dfc27d',high='#01665e')+
    scale_color_ssdqa(palette="diverging", discrete=FALSE)+
    theme_bw()+
    labs(colour="Proportion",
         shape="Anomaly",
         size="")+
    theme(axis.text.x = element_text(angle=90))
  
  ggplotly(plt, tooltip="text")
  
}

#' *Single Site, Anomaly, Across Time*
#' 
#' Control chart looking at proportion of visits with specialty over time
#'     and a reference table
#' 
#' 
#' @param process_output dataframe output by the corresponding ssdqa check
#' @param filt_list a named list with names equal to the column name/s that must exist in the `process_output`
#'                                    and values equal to the values on which to filter
#'                                    e.g. filt_list=list(concepts=c(first_concept, second_concept),
#'                                                        another_column_name=c('some_value_found_in_another_column_name'))
#' @param facet the variables by which you would like to facet the graph; defaults to NULL
#' @param plot_title_text text to display as the plot title. Will be tacked on to the text 'Control Chart: '
#' 
#' @return a list where:
#'      the first element is a P prime control chart that highlights points in time that are anomalous
#'      the second element is a gt table with the total counts based on the specified stratification 
#'        
#' 
plot_cnc_sp_ss_an_at <- function(process_output,
                           # filtered_var='ibd',
                           # filter_concept=81893,
                           filt_list=NULL,
                           ct_col,
                           denom_col,
                           id_col,
                           name_col,
                           facet=NULL,
                           plot_title_text){
  
  time_inc <- process_output %>% filter(!is.na(time_increment)) %>% distinct(time_increment) %>% pull()
  if(!is.null(filt_list)){
    # applying all defined filters
    for(i in 1:length(filt_list)){
      var_name<-names(filt_list[i])
      var_value<-filt_list[[i]]
      
      if(exists('c_added')){
        c_added<-c_added%>%filter(!!sym(var_name)%in%var_value)
      }else{
        # first time around, construct new df
        c_added<-process_output%>%filter(!!sym(var_name)%in%var_value)
      }
    }}else(c_added<-process_output)
  
  
  if(time_inc == 'year'){
    # can we pass into the function this additional variable in a list for this check? (i.e. not something the user would have to enter, but something that is tacked on when feeding into this output function) 
    # facet <- facet %>% append('concept_id') %>% unique()
    
    # can we define the name of the filtering column/s and values for filters in a list instead (filt_list) so that it can be used across inputs with different column names? (filt_list)
    # c_added <- process_output %>% filter(variable == filtered_var,
    #                                      concept_id == filter_concept)
    
    c_final <- c_added %>% group_by(!!!syms(facet), time_start, !!sym(ct_col)) %>%
      unite(facet_col, !!!syms(facet), sep = '\n') 
    # figure out here how to reference ct_col instead of hard coded value
    #y_col<-!!sym(ct_col)
    c_plot <- qic(data = c_final, x = time_start, y = n, chart = 'pp', facet = ~facet_col,
                  title = 'Control Chart: Code Usage Over Time', show.grid = TRUE, n = total,
                  ylab = 'Proportion', xlab = 'Time')
    
    op_dat <- c_plot$data
    
    new_pp <- ggplot(op_dat,aes(x,y)) +
      geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
      geom_line(colour = ssdqa_colors_standard[[12]], size = .5) +  
      geom_line(aes(x,cl)) +
      geom_point(colour = ssdqa_colors_standard[[6]] , fill = ssdqa_colors_standard[[6]], size = 1) +
      geom_point(data = subset(op_dat, y >= ucl), color = ssdqa_colors_standard[[3]], size = 2) +
      geom_point(data = subset(op_dat, y <= lcl), color = ssdqa_colors_standard[[3]], size = 2) +
      facet_wrap(~facet1, scales="free_y") +
      ggtitle(label = paste('Control Chart: ', plot_title_text)) +
      labs(x = 'Time',
           y = 'Proportion')+
      theme_minimal()
    
    output_int <- ggplotly(new_pp)
    
    # same here with passing in name of column
    # is there a need to apply an additional filter here, since c_added already has filtering applied?
    if(!'site'%in%colnames(c_added)){
      c_added<-c_added%>%mutate(site='combined')
    }
    ref_tbl <- generate_ref_table(tbl = c_added,# %>% filter(specialty_name == filtered_var,
                                                #           cluster == filter_concept), #%>% 
                                   # mutate(concept_id=as.integer(concept_id)),
                                  #id_col = 'concept_id',
                                  id_col=id_col,
                                  #denom = 'ct_concept',
                                  denom=denom_col,
                                  #name_col = 'concept_name',
                                  name_col=name_col,
                                  #vocab_tbl = vocab_tbl,
                                  time = TRUE)
    
    output <- list(output_int, ref_tbl)
    
  }else{
    
    # concept_nm <- process_output %>% 
    #   filter(!is.na(concept_name), concept_id == filter_concept) %>% 
    #   distinct(concept_name) %>% pull()
    # add in an if statement for if data is already anomalized?
    concept_nm<-process_output%>%distinct(specialty_name)%>%pull()
    anom_to_plot<-anomalize(.data=c_added%>%
                              group_by(!!!syms(facet)),
                            .date_var=time_start,
                            .value=!!sym(ct_col))
    anomalies <-
      plot_anomalies(.data=anom_to_plot,# %>% filter(concept_id == filter_concept),
                     .date_var=time_start) %>%
      layout(title = paste0('Anomalies for ', plot_title_text))

    decomp <-
      plot_anomalies_decomp(.data=anom_to_plot,# %>% filter(concept_id == filter_concept),
                            .date_var=time_start) %>%
      layout(title = paste0('Anomalies for ', plot_title_text))

    output <- list(anomalies, decomp)
    
  }
  
  return(output)

}

#' **Multi-Site Across Time Anomaly**
#' Function to generate output displaying the Euclidean distance between two time series: the smoothed (Loess) proportion of a user-selected specialty for a given site and the all-site average proportion for each time point.
#' Three graphs are output:
#'        a line graph displaying the smoothed proportion of variable specified at each site over time, with the Euclidean distance available in the tooltip when hovering over the line
#'        a line graph displaying the raw variable specified at each site over time
#'        a circular bar graph displaying the Euclidean distance from the all-site mean where the fill represents the average Loess proportion over time
#' @param process_output output from the associated ssdqa check
#'          should contain the columns specified in `grp_vars`
#'                                      and in `filt_list`
#' @param filt_list a named list 
#'        with names that must exist in the `process_output`
#'        and values equal to the values on which to filter
#'        e.g. filt_list=list(concepts=c(first_concept, second_concept),
#'        another_column_name=c('some_value_found_in_another_column_name'))

# want to also add in the name of the column to use as the proportions


plot_cnc_sp_ms_an_at <- function(process_output,
                                 grp_vars,
                                 filt_list=NULL){
  
  # apply defined filters, if supplied
  if(!is.null(filt_list)){# applying all defined filters
    for(i in 1:length(filt_list)){
      var_name<-names(filt_list[i])
      var_value<-filt_list[[i]]
      
      if(exists('filt_op')){
        filt_op<-filt_op%>%filter(!!sym(var_name)%in%var_value)
      }else{
        # first time around, construct new df
        filt_op<-process_output%>%filter(!!sym(var_name)%in%var_value)
      }
    }
  }else{filt_op<-process_output}
  
  
  
  allsites <-
    filt_op %>%
    select(!!!syms(grp_vars))%>%#time_start,concept_id,mean_allsiteprop) 
    distinct() %>%
    rename(prop=mean_allsiteprop) %>%
    mutate(site='all site average') %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Proportion: ",prop),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",prop))

  dat_to_plot <-
    filt_op %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean))

  p <- dat_to_plot %>%
    ggplot(aes(y = prop, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    scale_color_ssdqa() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion Across Time'))

  q <- dat_to_plot %>%
    ggplot(aes(y = prop, x = time_start, color = site,
               group=site, text=text_raw)) +
    scale_color_ssdqa() +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion Across Time'))

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
    theme_minimal() +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    theme(legend.position = 'bottom',
          legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
          axis.text.x = element_text(face = 'bold')) +
    labs(fill = 'Avg. Proportion \n(Loess)',
         y ='Euclidean Distance',
         x = '',
         title = paste0('Euclidean Distance'))

  plotly_p <- ggplotly(p,tooltip="text")
  plotly_q <- ggplotly(q,tooltip="text")

  output <- list(plotly_p,
                 plotly_q,
                 t)

  return(output)
}

#' *Multi-Site, Anomaly, No Time*
#' @param process_output the output from the cnc_sp check, summarized to be used in the multi site anomaly detection check
#' @param title text containing the title for the plot
#' @param text_wrapping_char the number of characters for the specialty names to be displayed on the plot
#' @return a dot plot with site on the x axis, specialty on the y axis, with size of dots indicating the MAD value, color indicating proportion of visits with the given specialty at the site, and shape indicating whether or not the combination is an anomaly
cnc_sp_ms_anom_nt<-function(process_output,
                            title,
                         text_wrapping_char = 60){
  
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  
  dat_to_plot <- process_output %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nSite: ",site,
                      "\nProportion: ",round(prop,2),
                      "\nMean proportion:",round(mean_val,2),
                      "\nMedian proportion: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2)))

  
  plt<-ggplot(dat_to_plot %>% filter(anomaly_yn != 'no outlier in group'),
              aes(x=site, y=specialty_name, text=text, color=prop))+
    geom_point_interactive(aes(size=mad_val,shape=anomaly_yn, tooltip = text))+
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c(20,8))+
    scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=60)) +
    labs(y = "Specialty",
         size="",
         title=paste0('Anomalous Variables per ', title, ' by Site')) +
    guides(color = guide_colorbar(title = 'Proportion'),
           shape = guide_legend(title = 'Anomaly'),
           size = 'none')
  
  girafe(ggobj = plt)
}
