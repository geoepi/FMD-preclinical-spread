
plot_epidemic_duration <- function(summary, region) {

require(scales)
require(patchwork)
  
  # Convert `region` to title case 
  region_title <- str_to_title(region)

  # Pre-format the label vector with commas
  summary <- summary %>%
    mutate(label_vector = comma(round(q50, 0)))
  
  # Create a common set of x-axis labels and ordering
  x_labels <- c("0" = "None", "1" = "1 day", "2" = "2 days", "3" = "3 days", "6" = "6 days")
  
  summary$label_vector <- as.character(round(summary$q50, 0))
  
  # optimal
  opt_set <- summary %>%
    filter(response_type == "optimal")
  
  # suboptimal
  subopt_set <- summary %>%
    filter(response_type == "suboptimal")
  
  # low-virulence
  lowV_set <- summary %>%
    filter(response_type == "low-virulence")
  
  # "optimal" (Optimal Detection)
  p_optimal <- ggplot(opt_set, aes(x = preclinical, y = q50)) +
    geom_bar(stat="identity", width = 0.9, fill = "#74add1") +
    scale_x_discrete(labels = x_labels) +
    geom_text(aes(label = label_vector),
              size = 4.5, position = position_dodge(width = 0.9), hjust = -0.5) +
    scale_y_continuous(
      limits = c(1, 350), 
      breaks = seq(0, 350, 50),
      labels = seq(0, 350, 50),
      oob = scales::squish
    ) +
    coord_flip() +
    labs(x = " ", 
         y = " ",
         title = "Optimal Detection") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "cm"),
      legend.position = "none",
      axis.title.x = element_text(size = 20, face = "bold", vjust = -2),
      axis.title.y = element_text(size = 20, face = "bold", vjust = 3),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 22, face = "bold", colour="gray50", hjust = 0.5)
    )
  
  # suboptimal
  p_suboptimal <- ggplot(subopt_set, aes(x = preclinical, y = q50)) +
    geom_bar(stat="identity", width = 0.9, fill = "orange2") +
    scale_x_discrete(labels = x_labels) +
    geom_text(aes(label = label_vector),
              size = 4.5, position = position_dodge(width = 0.9), hjust = -0.5) +
    scale_y_continuous(
      limits = c(1, 350), 
      breaks = seq(0, 350, 50),
      labels = seq(0, 350, 50),
      oob = scales::squish
    ) +
    coord_flip() +
    labs(x = "Duration of Incubation Phase Transmission", 
         y = " ",
         title = "Suboptimal Detection") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "cm"),
      legend.position = "none",
      axis.title.x = element_text(size = 20, face = "bold", vjust = -2),
      axis.title.y = element_text(size = 20, face = "bold", vjust = 3),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 22, face = "bold", colour="gray50", hjust = 0.5)
    )
  
  # "lowV"
  p_lowV <- ggplot(lowV_set, aes(x = preclinical, y = q50)) +
    geom_bar(stat="identity", width = 0.3, fill="red4") +
    scale_x_discrete(labels = x_labels) +
    geom_text(aes(label = label_vector),
              size = 4.5, position = position_dodge(width = 0.9), hjust = -0.5) +
    scale_y_continuous(
      limits = c(1, 350), 
      breaks = seq(0, 350, 50),
      labels = seq(0, 350, 50),
      oob = scales::squish
    ) +
    coord_flip() +
    labs(x = " ", 
         y = "Median Epidemic Duration (days)",
         title = "Low-Virulence") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "cm"),
      legend.position = "none",
      axis.title.x = element_text(size = 20, face = "bold", vjust = -2),
      axis.title.y = element_text(size = 20, face = "bold", vjust = 3),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y =element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 22, face = "bold", colour="gray50", hjust = 0.5)
    )

  # combine plot using patchwork package
  combined_plot <- p_optimal + 
                   p_suboptimal + 
                   p_lowV + 
    plot_layout(ncol = 1) + 
    plot_annotation(
      title = sprintf("Duration of Simulated Outbreaks in the %s U.S.", region_title), 
      theme = theme(
        plot.margin = unit(c(1, 0.5, 0.75, 0.75), "cm"), 
        plot.title = element_text(size = 28, face = "bold", hjust = 0.5)
      ))
  
  return(combined_plot)
}