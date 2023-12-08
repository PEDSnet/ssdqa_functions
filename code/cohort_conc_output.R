
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

#' Generate a color palette
#'   based on the length of distinct values to be mapped to a color
#' @param distinct_list a list of values for the variable to be color-fied
#' @return list with colors assigned to each value of variable
generate_color_pal <- function(distinct_list){
  ramp_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(length(distinct_list))
  randomized_palette <- ramp_palette[sample(1:length(ramp_palette))]
  setNames(randomized_palette, distinct_list)
}
