
plot_wave_velocity <- function(summary) {
  
  # Prepare data
  summary <- summary %>%
    mutate(
      scenario_type = str_to_title(word(scenario, 1, sep = "/")),
      preclinical = word(scenario, 2, sep = "/") %>% 
        str_replace_all("-days?-preclinical", "")
    )
  
  summary <- summary %>%
    mutate(
      preclinical = recode(preclinical,
                           "0" = "None",
                           "1" = "1 day",
                           "2" = "2 days",
                           "3" = "3 days",
                           "6" = "6 days"))  
  
  summary <-   summary %>%
    mutate(
      scenario_type = recode(scenario_type,
                             "Optimal" = "Optimal Detection",
                             "Suboptimal" = "Suboptimal Detection",
                             "Low-Virulence" = "Low-Virulence")) 
  
  summary$preclinical <- ordered(factor(summary$preclinical), 
                                 c("None", "1 day", "2 days", "3 days", "6 days"))
  
  summary$scenario_type <- ordered(factor(summary$scenario_type), 
                                 c("Optimal Detection", "Suboptimal Detection", "Low-Virulence"))
  
  # Assign colors
  myCols <- kelly()
  
  # Plot wave velocity
  velocity_plot <- ggplot(summary, aes(infect_day, log(q50), group = scenario, col = preclinical)) +
    geom_line(linewidth = 0.4) +
    ylim(0, 6) +
    scale_color_manual(values = c(
      "None" = myCols[3],
      "1 day" = myCols[4],
      "2 days" = myCols[6],
      "3 days" = myCols[7],
      "6 days" = myCols[10]
    )) +
    facet_wrap(~ scenario_type, ncol=1) +
    labs(
      x = "Simulation Day",
      y = "Distance (log km)",
      col = "Preclinical Infectious Duration",
      title = " "
    ) +
    theme_minimal() +
    theme(
      plot.margin    = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
      legend.position = "bottom",
      strip.text     = element_text(size = 18, face = "bold", color = "gray40"),
      axis.title.x   = element_text(size = 22, face = "bold"),
      axis.title.y   = element_text(size = 22, face = "bold"),
      axis.text.x    = element_text(size = 18, face = "bold"),
      axis.text.y    = element_text(size = 18, face = "bold"),
      legend.key.width = unit(3, "line"),
      legend.key.height = unit(1, "line"),
      legend.text = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      plot.title     = element_text(size = 22, face = "bold", hjust = 0.5)
    )
  
  return(velocity_plot)
}