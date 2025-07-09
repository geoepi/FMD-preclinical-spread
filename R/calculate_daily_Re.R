calculate_daily_Re <- function(iteration_Re) {
  
  require(dplyr)
  require(here)
  require(tidyr)

  iteration_mean_Re <- iteration_Re %>%
    group_by(region, scenario_type, preclinical, iteration, infect_day) %>%
    summarize(Re = mean(num_transmissions), .groups = "drop")
  
  ## summarize daily Re across iterations
  daily_Re_summary <- iteration_mean_Re %>%
    group_by(region, scenario_type, preclinical, infect_day) %>%
    summarize(
      mean = mean(Re, na.rm = TRUE),
      q05 = quantile(Re, probs = 0.05, na.rm = TRUE),
      q25 = quantile(Re, probs = 0.25, na.rm = TRUE),
      q50 = quantile(Re, probs = 0.50, na.rm = TRUE),
      q75 = quantile(Re, probs = 0.75, na.rm = TRUE),
      q95 = quantile(Re, probs = 0.95, na.rm = TRUE),
      .groups   = "drop")
  
  ## Daily summary
  daily_Re <- daily_Re_summary %>%
    group_by(region, scenario_type, preclinical) %>%
    complete(infect_day = 10:365, 
             fill = list(mean = 0, q05 = 0, 
                         q25 = 0, q50 = 0, q75 = 0, q95 = 0)) %>%
    ungroup()
  
  return(daily_Re)
}