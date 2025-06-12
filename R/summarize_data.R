
summarize_infections <- function(infection) {
  
  infect_summary <- infection %>% 
    group_by(region, response_type, preclinical, iteration) %>% 
    summarize(
      farms_infected = n(),
      cattle_infected = sum(cattle, na.rm = FALSE),
      first_infect_day = min(infect_day, na.rm = TRUE),
      last_infect_day = max(infect_day, na.rm = TRUE)
    ) %>%
    arrange(preclinical) %>%
    arrange(response_type) %>%
    arrange(region)
  
  return(infect_summary)
}

generate_infect_statistics <- function(summary) {
  
  config_long <-pivot_longer(summary,
                             cols = farms_infected:last_infect_day,
                             names_to = "summary",
                             values_to = "n")
  
  config_long <- config_long %>% 
    ungroup() %>% 
    mutate(summary=factor(summary, levels=c("farms_infected", "cattle_infected", "first_infect_day", "last_infect_day")))
  
  infect_summary_statistics <- config_long %>% 
    group_by(summary, region, response_type, preclinical) %>% 
    summarize(
      iterations=n(),
      mean= mean(n, na.rm = TRUE),
      sd = sd(n, na.rm = TRUE),
      q05= quantile(n, 0.05, na.rm = TRUE),
      q25= quantile(n, 0.25, na.rm = TRUE),
      q50= quantile(n, 0.50, na.rm = TRUE),
      q75= quantile(n, 0.75, na.rm = TRUE),
      q95= quantile(n, 0.95, na.rm = TRUE)
    ) %>%
    arrange(preclinical) %>%
    arrange(response_type) %>% 
    arrange(region) %>%
    arrange(summary)
  
  return(infect_summary_statistics)
}


summarize_detections <- function(detection) {
  
  detect_summary <- detection %>% 
    group_by(region, response_type, preclinical, iteration) %>% 
    summarize(
      farms_detected = n(),
      first_detect = min(detect_day, na.rm = TRUE),
      last_detect = max(detect_day, na.rm = TRUE),
      duration = last_detect - first_detect
    ) %>%
    arrange(preclinical) %>%
    arrange(response_type) %>%
    arrange(region)
  
  return(detect_summary)
}

generate_detect_statistics <- function(summary) {
  
  config_long <-pivot_longer(summary,
                             cols = farms_detected:duration,
                             names_to = "summary",
                             values_to = "n")
  
  config_long <- config_long %>% 
    ungroup() %>% 
    mutate(summary=factor(summary, levels=c("farms_detected", "first_detect", "last_detect", "duration")))
  
  detect_summary_statistics <- config_long %>% 
    group_by(summary, region, response_type, preclinical) %>% 
    summarize(
      iterations=n(),
      mean= mean(n, na.rm = TRUE),
      sd = sd(n, na.rm = TRUE),
      q05= quantile(n, 0.05, na.rm = TRUE),
      q25= quantile(n, 0.25, na.rm = TRUE),
      q50= quantile(n, 0.50, na.rm = TRUE),
      q75= quantile(n, 0.75, na.rm = TRUE),
      q95= quantile(n, 0.95, na.rm = TRUE)
    ) %>%
    arrange(preclinical) %>%
    arrange(response_type) %>% 
    arrange(region) %>%
    arrange(summary)
  
  return(detect_summary_statistics)
}


