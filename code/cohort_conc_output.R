
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

pf_ss_exp_nt_v2 <- function(data_tbl,
                            facet,
                            x_var,
                            y_var,
                            fill_var,
                            pal_map) {
  
  # if(output=='median_site_with0s') {y_title='Median for All Patients'}
  # if(output=='median_site_without0s') {y_title='Median for Patients with Fact'}
  # if(output=='prop_all_w_fact') {y_title='Proportion of Patients with Fact'}
  # 
  
  ggplot(data_tbl,
         aes(x=!!sym(x_var), y=!! sym(y_var), fill=!!sym(fill_var))) +
    geom_bar(stat='identity') + 
    facet_wrap((facet), scales="free_y") + 
    # labs(y=y_title,
    #      x='Domain') +
    scale_fill_manual(values=pal_map) +
    coord_flip() +
    theme_classic() +
    theme(panel.grid.major = element_line(size=0.4, linetype = 'solid'),
          panel.grid.minor = element_line(size=0.2, linetype = 'dashed'),
          legend.position = "none")
  
  
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
  return(plt)
  
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
      theme_classic()
  }else{
    plt <- ggplot(data_tbl,aes(x=!!sym(x_var), y=!! sym(y_var), fill=!!sym(fill_var)))+
    geom_tile(color='white',lwd=0.5,linetype=1)+
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
