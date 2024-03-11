
plot_conc_ss_exp <- function(input,
                             facet){
  if(is.null(facet)){
    plt <- ggplot(input, aes(x=specialty_name, y=num_visits))+
      geom_bar(stat='identity')
  }else{
    plt <- ggplot(input, aes(x=specialty_name, y=num_visits))+
      geom_bar(stat='identity')+
      facet_wrap(facets = eval(facet), scales = 'free')
  }
  return(plt)
  
}

conc_ss_exp_nt <- function(data_tbl,
                            facet,
                            x_var,
                            y_var,
                            fill_var,
                            pal_map) {
  
  data_tbl <- data_tbl %>%
    mutate(text=paste("Specialty: ", specialty_name,
                      "\nNumber of Visits: ",format(n,big.mark=","),
                      "\nProportion: ",round(prop,2)))
  plt <- ggplot(data_tbl,
         aes(x=!!sym(x_var), y=!! sym(y_var), fill=!!sym(fill_var), text=text)) +
    geom_bar(stat='identity') + 
    facet_wrap((facet))+
    # facet_wrap((facet), scales="free_y") + 
    # labs(y=y_title,
    #      x='Domain') +
    scale_fill_manual(values=pal_map) +
    coord_flip() +
    theme_classic() +
    theme(panel.grid.major = element_line(size=0.4, linetype = 'solid'),
          panel.grid.minor = element_line(size=0.2, linetype = 'dashed'))
  
  ggplotly(plt,
           tooltip="text")
  
  
}

#' Function to plot single site exploratory over time:
#'    line plot with years on x axis and proportion on y axis
#'    can facet as specified
#'    each line can be colored as specified
#' @param data_tbl table with the data to plot. Should contain the columns:
#'                      year
#'                      prop
#'                      column names as specified by facet (if supplied) and color_var
#' @param facet optional list of variables to facet by
#' @param color_var name of the variable in `data_tbl` to assign colors to lines based off of
#' @param pal_map palette color assignment to apply to `color_var`
#' @return ggplot object with line plot showing proportion over time
plot_ss_exp_ot <- function(data_tbl,
                           facet,
                           color_var,
                           pal_map){
  
  
  if(is.null(facet)){
    plt <- ggplot(data_tbl, aes(x=year, y=prop, color=!!sym(color_var)))+
      geom_line()+
      theme_classic()+
      scale_color_manual(values=pal_map)
  }else{
    plt <- ggplot(data_tbl, aes(x=year, y=prop, color=!!sym(color_var)))+
      geom_line()+
      theme_classic()+
      facet_wrap(facets = eval(facet), scales = 'free')+
      scale_color_manual(values=pal_map)
  }
  ggplotly(plot)
}

#' Function to plot multi-site exploratory with no time component
#' @param data_tbl table with the columns specified as `x_var`, `y_var`, `fill_var` and anything in `facet`, if applicable
#' @param x_var string containing name of variable to plot on x axis
#' @param y_var string containing name of variable to plot on y axis
#' @param fill_var string containing name of variable to fill the heatmap with, expected to be a numeric value
#' @param facet either NULL if no facet required or a vector containing variable to facet by
#' @return a heatmap with the specified x and y axes and fill
plot_ms_exp_nt <- function(data_tbl,
                           x_var,
                           y_var,
                           fill_var,
                           facet=NULL){
  if(is.null(facet)){
    plt <- ggplot(data_tbl,aes(x=!!sym(x_var), y=!! sym(y_var), fill=!!sym(fill_var)))+
    geom_tile(color='white',lwd=0.5,linetype=1)+
      scale_fill_gradient2(low='pink', high='maroon')+
      theme_classic()
  }else{
    plt <- ggplot(data_tbl,aes(x=!!sym(x_var), y=!! sym(y_var), fill=!!sym(fill_var)))+
    geom_tile(color='white',lwd=0.5,linetype=1)+
      scale_fill_gradient2(low='pink', high='maroon')+
      theme_classic()+
      facet_wrap(facets = eval(facet), scales = 'free')
  }
  return(plt)
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
#' Function to generate a plot for anomaly detection with no time component
#' @param data_tbl table with at least the variables specified in:
#'          x_var, y_var, fill_var, facet (if applicable)
#' @param x_var variable to plot on the x axis
#'                should be a qualitative variable
#' @param y_var variable to plot on the y axis 
#'                should be a quantitative variable
#' @param fill_var variable to fill bars with
#' @param facet vector of strings of variable names to facet by
#' @param pal_map map matching fill variables to colors to fill by
#' @return bar plot filled and faceted as specified with error bars with lower and upper bounds and mean marked
plot_ss_an_nt_conc <- function(data_tbl,
                          x_var,
                          y_var,
                          fill_var,
                          facet=NULL,
                          pal_map){
  data_tbl <- data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nAnomaly: ",anomaly_yn,
                      "\nProportion: ",round(prop,2),
                      "\nMedian proportion: ",round(median,2)))
  if(!is.null(facet)){
  plt<-ggplot(data_tbl,
         aes(x=!!sym(x_var), y=!! sym(y_var), text=text))+
    geom_bar(stat='identity', position=position_dodge(width=1), aes(fill=!!sym(fill_var), color=anomaly_yn)) +
    scale_fill_manual(values=pal_map)+
    geom_point(aes(x=!!sym(x_var),fill=!!sym(fill_var),y=median), position=position_dodge(width=1),shape=4)+
    coord_flip()+
    facet_wrap((facet), scales="free_y")+
    theme_classic()
  }else{
    plt<-ggplot(data_tbl,
                aes(x=!!sym(x_var), y=!! sym(y_var), text=text)) +
      geom_bar(stat='identity', position=position_dodge(width=1), aes(fill=!!sym(fill_var),color=anomaly_yn)) +
      scale_fill_manual(values=pal_map)+
      #geom_errorbar(aes(x=!!sym(x_var),ymin=sd_lower,ymax=sd_upper))+
      geom_point(aes(x=!!sym(x_var),fill=!!sym(fill_var),y=median), position=position_dodge(width=1),shape=4)+
      coord_flip()+
      theme_classic()
  }
  ggplotly(plt, tooltip="text")
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


plot_ss_an_nt_conc_hm <- function(data_tbl){
  data_tbl <- data_tbl %>%
    mutate(text=paste("Specialty: ", specialty_name,
                      "\nCluster: ", cluster,
                      "\nProportion: ", round(prop,2),
                      "\nAnomaly: ", anomaly_yn))
  plt<-ggplot(data_tbl,
              aes(x=cluster,
                  y=specialty_name,
                  fill=prop,
                  text=text))+
    geom_tile(aes(color=as.factor(anomaly_yn)),lwd=0.5,linetype=1)+
    theme_classic()+
    scale_colour_manual(values=c("white", "red"))
  ggplotly(plt, tooltip = "text")
}


plot_conc_ms_exp_dotplot <- function(data_tbl,
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
    scale_color_manual(values=pal_map)+
    coord_flip()+
    theme_bw()
  ggplotly(plt, tooltip = "text")
  
}

plot_ss_exp_ot_conc <- function(data_tbl,
                                facet=NULL,
                                pal_map){
  dat_to_plot<-data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nProportion: ",round(prop,2),
                      "\nTime Start: ",time_start))
  
  if(is.null(facet)){
  plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=specialty_name,text=text))+
    geom_line(group=1)+
    scale_color_manual(values=pal_map)+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))
  }else{  
    plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=specialty_name,text=text))+
      geom_line(group=1)+
      scale_color_manual(values=pal_map)+
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

#' Function to plot specialty concordance: multi-site, exploratory, over time
#' @param data_tbl table which must contain the cols: time_start | codeset_name | specialty_name | site
#' @param facet if supplied, variable to facet the plot by
#' @param pal_map color palette for the variable that will be used to color the line
#' @return line plot, with time on x axis, proportion on y, line color determined by site
#'              with a dotted line for the all-site mean
plot_ms_exp_ot_conc <- function(data_tbl,
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
      scale_color_manual(values=pal_map)+
      scale_linetype_manual(values=line_vals,breaks=line_breaks)+
      theme_classic()+
      theme(axis.text.x = element_text(angle=90))
  }else{  
    plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=site,text=text))+
      geom_line(group=1,aes(linetype=site))+
      scale_color_manual(values=pal_map)+
      facet_wrap(facets = eval(facet), scales = 'free')+
      scale_linetype_manual(values=line_vals,breaks=line_breaks)+
      theme_classic()+
      theme(axis.text.x = element_text(angle=90))
  }
  
  ggplotly(plt, tooltip="text")
}

compute_mad<-function(){
  
}

plot_ms_an_nt_conc<-function(data_tbl){
  dat_to_plot <- data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nProportion: ",round(prop,2),
                      "\nNo. MAD from median: ", round(n_mad,2),
                      "\nAnomaly: ",anomaly_yn))
  plt<-ggplot(dat_to_plot, aes(x=site,y=specialty_name,fill=n_mad, text=text))+
    geom_tile()+
    scale_fill_gradient(colors=c('#8c510a','#d8b365','#f6e8c3', '#c7eae5', '#5ab4ac', '#01665e'))
  
  ggplotly(plt, tooltip="text")
}

plot_ms_an_ot_conc<- function(data_tbl){
  plt<-ggplot(data_tbl, aes(x=time_start,y=site,fill=n_mad))+
    geom_tile()+
    facet_wrap(~specialty_name)+
    scale_fill_gradient2(low='pink',high='maroon')+
    theme_classic()+
    theme(axis.text.x = element_text(angle=90))
  ggplotly(plt)
}

plot_ms_an_nt_conc_alt<-function(data_tbl){
  dat_to_plot <- data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nSite: ",site,
                      "\nProportion: ",round(prop,2),
                      "\nMedian proportion: ",round(median,2),
                      "\nNo. MAD from median: ", round(n_mad,2)))
  
  mid<-(max(dat_to_plot$n_mad,na.rm=TRUE)+min(dat_to_plot$n_mad,na.rm=TRUE))/2
  
  plt<-ggplot(dat_to_plot, aes(x=site, y=specialty_name, text=text))+
    geom_point(aes(size=n_mad,colour=prop,shape=anomaly_yn))+
    scale_shape_manual(values=c(20,8))+
    #scale_color_gradient2(midpoint=mid,low='#8c510a',mid='#f5f5f5', high='#01665e')+
    scale_color_gradient(low='#dfc27d',high='#01665e')+
    theme_bw()+
    labs(colour="Proportion",
         shape="Anomaly",
         size="")+
    theme(axis.text.x = element_text(angle=90))
  
  ggplotly(plt, tooltip="text")
}

plot_ss_an_nt_conc_alt<- function(data_tbl){
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
    scale_color_gradient(low='#dfc27d',high='#01665e')+
    theme_bw()+
    labs(colour="Proportion",
         shape="Anomaly",
         size="")+
    theme(axis.text.x = element_text(angle=90))
  
  ggplotly(plt, tooltip="text")
  
}

#' Function for concordance: clinical events and specialties:
#'        single site, anomaly detection, over time
#' @param data_tbl table from cnc_sp output grouped accordingly within the `conc_output_gen` function, with at least the columns time_start | n | specialty_name
#' @return a control chart with time on the x axis, number of visits on the y axis, faceted by specialty_name
plot_cnc_sp_ss_an_at<-function(data_tbl){
  qic(data=data_tbl, x=time_start, y = n, chart='c', facets=~specialty_name, #can also do cluster~specialty_name
      scales='free_y',
      x.angle=45,
      title="Control Chart: Specialty Concordance for Clusters Over Time",
      show.grid = TRUE,
      xlab='Time')
}
