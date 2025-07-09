compile_daily_summary <- function(scenarios_df, reference, impute_zero = TRUE) {
  
  require(dplyr)
  require(geosphere)
  require(here)
  require(tidyr)
  
  # function to extract scenario names
  extract_scenario_name <- function(path) {
    scenario <- sub("^.*outputs_(western|central|eastern)/", "", path)
    scenario <- sub("/Outputs_Infection.txt$", "", scenario)
    return(scenario)
  }
  
  summary_list <- list()
  daily_distances_list <- list()
  
  # loop through each file
  for(i in seq_along(scenarios_df)) {
    
    scenario_tmp <- read.table(here(scenarios_df[i]))
    
    names(scenario_tmp) <- c("iteration",
                             "infect_day",
                             "premises",
                             "source",
                             "pathway",
                             "x",
                             "y",
                             "cattle")
    
    # join with the reference coordinates
    scenario_tmp <- left_join(scenario_tmp, reference, by = "premises")
    
    # exclude index cases
    df_transmissions <- scenario_tmp %>% 
      filter(source != 0)
    
    # Lookup table
    df_sources <- scenario_tmp %>%
      select(iteration, premises, x, y, longitude, latitude) %>%
      distinct()
    
    # combine with source coordinates
    df_transmissions <- df_transmissions %>%
      left_join(df_sources,
                by = c("iteration" = "iteration", "source" = "premises"),
                suffix = c("", "_source"))
    
    # compute distance using the Haversine formula then convert to km
    df_transmissions <- df_transmissions %>%
      mutate(distance = geosphere::distHaversine(
        cbind(longitude, latitude), 
        cbind(longitude_source, latitude_source)
      ) / 1000) %>%
      filter(distance > 0)  # in case of no movement
    
    # mean distance per iteration and day
    daily_distances <- df_transmissions %>%
      group_by(iteration, infect_day) %>%
      summarize(mean_distance = mean(distance, na.rm = TRUE), .groups = "drop")
    
    # get scenario name
    scenario_name <- extract_scenario_name(scenarios_df[i])
    daily_distances <- daily_distances %>%
      mutate(scenario = scenario_name)
    
    daily_distances_list[[i]] <- daily_distances
    
    # summarize daily across iterations
    daily_summary <- daily_distances %>%
      group_by(infect_day) %>%
      summarize(
        mean = mean(mean_distance, na.rm = TRUE),
        q05  = quantile(mean_distance, probs = 0.05, na.rm = TRUE),
        q25   = quantile(mean_distance, probs = 0.25, na.rm = TRUE),
        q50   = quantile(mean_distance, probs = 0.50, na.rm = TRUE),
        q75   = quantile(mean_distance, probs = 0.75, na.rm = TRUE),
        q95  = quantile(mean_distance, probs = 0.975, na.rm = TRUE),
        min_spread = min(mean_distance, na.rm = TRUE),
        max_spread = max(mean_distance, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      mutate(scenario = scenario_name)
    
    summary_list[[i]] <- daily_summary
  }
  
  # combine the summaries
  combined_summary <- bind_rows(summary_list)
  combined_daily_distances <- bind_rows(daily_distances_list)
  
  # impute zeros for missing infect_day through 365
  if(impute_zero) {
    combined_summary <- combined_summary %>%
      group_by(scenario) %>%
      complete(infect_day = 10:365, 
               fill = list(mean = 0, q05 = 0, q25 = 0, q50 = 0,
                           q75 = 0, q95 = 0, min_spread = 0, max_spread = 0)) %>%
      ungroup()
  }
  
  return(list(combined_summary = combined_summary, daily_distances = combined_daily_distances))
}
