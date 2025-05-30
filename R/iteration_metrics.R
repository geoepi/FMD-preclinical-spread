iteration_metrics <- function(daily_distances, offset = 0.001) {

  # Iteration metrics:
  #  - auc_log: cumulative log-transformed spread (trapezoidal rule)
  #  - peak_spread: maximum log-transformed mean distance within the iteration
  #  - peak_day: corresponding day of maximum spread (peak_spread)
  
  iteration_summary <- daily_distances %>%
    group_by(scenario, iteration) %>%
    arrange(infect_day) %>%
    summarize(
      auc_log = if(n() > 1) {
        sum(diff(infect_day) * ((log(mean_distance + offset)[-1] + log(mean_distance + offset)[-n()]) / 2))
      } else NA_real_,
      peak_spread = max(log(mean_distance + offset), na.rm = TRUE),
      peak_day = infect_day[which.max(log(mean_distance + offset))],
      .groups = "drop"
    ) %>%
    # get scenario name
    mutate(
      preclinical_delay = as.numeric(str_extract(scenario, "\\d+")),
      scenario_type = str_extract(scenario, "^[^/]+")
    )
  
  return(iteration_summary)
}
