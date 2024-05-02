

compute_attrition_diff <- function(attrition_tbl){
  
  if(! site %in% colnames(attrition_tbl)){attrition_tbl$site <- 'attrition_site'}
  
  pct_prior <- attrition_tbl %>%
    group_by(site) %>%
    arrange(stepnum) %>%
    mutate(
      percent_prior_step = 100 * num_pats / lag(num_pats),
      drop_prior_step = lag(num_pats) - num_pats,
      drop_as_percent_prior = 100 * (lag(num_pats) - num_pats) / lag(num_pats)
    ) %>%
    ungroup()
  
  step0_cts <- attrition_tbl %>%
    filter(stepnum == 0) %>%
    rename('step0_pts' = num_pats)
  
  pct_step0 <- attrition_tbl %>%
    left_join(step0_cts, by = "site") %>%
    mutate(percent_step0 = 100 * num_pats / step0_pts)
  
  final_attrition <- attrition_tbl %>%
    left_join(pct_prior) %>%
    left_join(pct_step0)
  
}
