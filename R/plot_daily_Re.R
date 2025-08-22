
plot_daily_Re <- function(summary) {
  
  # Prepare data
  summary <- summary %>%
    mutate(
      preclinical = recode(preclinical,
                           "0" = "None",
                           "1" = "1 day",
                           "2" = "2 days",
                           "3" = "3 days",
                           "6" = "6 days"))  
  
  summary <- summary %>%
    mutate(
      scenario_type = recode(scenario_type,
                             "optimal" = "Optimal Detection",
                             "suboptimal" = "Suboptimal Detection",
                             "low-virulence" = "Low-Virulence"))  
  
  summary$preclinical <- ordered(factor(summary$preclinical), 
                                  c("None", "1 day", "2 days", "3 days", "6 days"))
  
  # Assign colors
  myCols <- kelly()
  
  # 
  Re_plot <- ggplot(summary, aes(infect_day, q50, group = preclinical, col = preclinical)) +
    geom_hline(yintercept = 1, col="black", linewidth=0.5, linetype = "dashed") +
    geom_smooth(method = "loess", span=0.05, se=FALSE) +
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
      y = "Effective Reproduction (Re)",
      col = "Preclinical Infectious Duration",
      title = " "
    ) +
    theme_minimal() +
    theme(
      plot.margin    = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
      legend.position = "bottom",
      strip.text     = element_text(size = 18, face = "bold", color = "gray40"),
      axis.title.x   = element_text(size = 22, face = "bold"),
      axis.title.y   = element_text(size = 20, face = "bold"),
      axis.text.x    = element_text(size = 18, face = "bold"),
      axis.text.y    = element_text(size = 18, face = "bold"),
      legend.key.width = unit(3, "line"),
      legend.key.height = unit(1, "line"),
      legend.text = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      plot.title     = element_text(size = 22, face = "bold", hjust = 0.5)
    )
  
  return(Re_plot)
}