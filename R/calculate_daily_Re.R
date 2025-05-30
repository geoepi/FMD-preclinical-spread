calculate_daily_Re <- function(scenarios_df, reference, impute_zero = TRUE) {
  
  require(dplyr)
  require(here)
  require(tidyr)
  
  # get scenario names
  extract_scenario_name <- function(path) {
    scenario <- sub("^.*outputs_(western|central|eastern)/", "", path)
    scenario <- sub("/Outputs_Infection.txt$", "", scenario)
    return(scenario)
  }
  
  # to store outputs
  Re_summary_list <- list()
  iteration_Re_list <- list()
  
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
    
    # join with the reference data
    scenario_tmp <- left_join(scenario_tmp, reference, by = "premises")
    
    # eExclude index cases (source == 0)
    df_transmissions <- scenario_tmp %>% 
      filter(source != 0)

    # find the number of transmissions per source for each iteration and infect_day,

    iteration_Re <- df_transmissions %>%
      group_by(iteration, infect_day, source) %>%
      summarize(num_transmissions = n(), .groups = "drop") %>%
      group_by(iteration, infect_day) %>%
      summarize(Re = mean(num_transmissions), .groups = "drop")
    
    # scenario name
    scenario_name <- extract_scenario_name(scenarios_df[i])
    iteration_Re <- iteration_Re %>%
      mutate(scenario = scenario_name)
    
    # summarize daily across iterations
    daily_Re_summary <- iteration_Re %>%
      group_by(infect_day) %>%
      summarize(
        cred_50   = mean(Re, na.rm = TRUE),
        cred_025  = quantile(Re, probs = 0.025, na.rm = TRUE),
        cred_25   = quantile(Re, probs = 0.25, na.rm = TRUE),
        cred_75   = quantile(Re, probs = 0.75, na.rm = TRUE),
        cred_975  = quantile(Re, probs = 0.975, na.rm = TRUE),
        .groups   = "drop") %>%
      mutate(scenario = scenario_name)
    
    iteration_Re_list[[i]] <- iteration_Re
    Re_summary_list[[i]] <- daily_Re_summary
  }
  
  # combine
  combined_daily_Re <- bind_rows(Re_summary_list)
  combined_iteration_Re <- bind_rows(iteration_Re_list)
  
  # if imputation is enabled, add rows for missing infect_day values
  if(impute_zero) {
    combined_daily_Re <- combined_daily_Re %>%
      group_by(scenario) %>%
      complete(infect_day = 1:365, 
               fill = list(cred_50 = 0, cred_025 = 0, 
                           cred_25 = 0, cred_75 = 0, cred_975 = 0)) %>%
      ungroup()
  }
  
  return(list(combined_daily_Re = combined_daily_Re, iteration_Re = combined_iteration_Re))
}