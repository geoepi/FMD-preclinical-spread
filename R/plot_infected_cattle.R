
plot_infected_cattle <- function(summary, region) {

require(scales)
require(patchwork)
 
  # Convert `region` to title case 
  region_title <- str_to_title(region)

  # Pre-format the label vector with commas
  summary <- summary %>%
    mutate(label_vector = comma(round(q50, 0)))
  
  # Create a common set of x-axis labels and ordering
  x_labels <- c("0" = "None", "1" = "1 day", "2" = "2 days", "3" = "3 days", "6" = "6 days")
  
  # optimal
  opt_set <- summary %>%
    filter(scenario_type == "optimal")
  
  # suboptimal
  subopt_set <- summary %>%
    filter(scenario_type == "suboptimal")
  
  # low-virulence
  lowV_set <- summary %>%
    filter(scenario_type == "low-virulence")
  
  # "optimal" (optimal detection)
  p_optimal <- ggplot(opt_set, aes(x = preclinical, y = q50)) +
    geom_bar(stat="identity", width = 0.9, fill = "#74add1") +
    scale_fill_manual(values = scenario_colors) +
    scale_x_discrete(labels = x_labels) +
    geom_text(aes(label = label_vector),
              size = 4.5, position = position_dodge(width = 0.9), vjust = -0.5) +
    scale_y_log10(
      limits = c(1, 3e6), 
      breaks = c(1, 10, 100, 1e3, 1e4, 1e5, 1e6),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      oob = scales::squish
    ) +
    labs(x = " ", 
         y = "Median Infected Cattle",
         title = "Optimal Detection") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "cm"),
      legend.position = "none",
      axis.title.x = element_text(size = 20, face = "bold", vjust = -2),
      axis.title.y = element_text(size = 20, face = "bold", vjust = 3),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
    )
  
  # suboptimal
  p_suboptimal <- ggplot(subopt_set, aes(x = preclinical, y = q50)) +
    geom_bar(stat="identity", width = 0.9, fill = "orange2") +
    scale_fill_manual(values = scenario_colors) +
    scale_x_discrete(labels = x_labels) +
    geom_text(aes(label = label_vector),
              size = 4.5, position = position_dodge(width = 0.9), vjust = -0.5) +
    scale_y_log10(
      limits = c(1, 3e6), 
      breaks = c(1, 10, 100, 1e3, 1e4, 1e5, 1e6),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      oob = scales::squish
    ) +
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
      axis.text.y = element_blank(),
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
    )
  
  # "low-virulence"
  p_lowV <- ggplot(lowV_set, aes(x = preclinical, y = q50)) +
    geom_bar(stat="identity", width = 0.3, fill="red4") +
    scale_fill_manual(values = scenario_colors) +
    scale_x_discrete(labels = x_labels) +
    geom_text(aes(label = label_vector),
              size = 4.5, position = position_dodge(width = 0.9), vjust = -0.5) +
    scale_y_log10(
      limits = c(1, 3e6), 
      breaks = c(1, 10, 100, 1e3, 1e4, 1e5, 1e6),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      oob = scales::squish
    ) +
    labs(x = " ", 
         y = " ",
         title = "Low-Virulence") +
    theme_minimal() +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.75, 0.75), "cm"),
      legend.position = "none",
      axis.title.x = element_text(size = 20, face = "bold", vjust = -2),
      axis.title.y = element_text(size = 20, face = "bold", vjust = 3),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y =element_blank(),
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5)
    )
  
  # combine plot using patchwork package
  combined_plot <- p_optimal + 
                   p_suboptimal + 
                   p_lowV + 
    plot_layout(ncol = 3) + 
    plot_annotation(
      title = sprintf("Magnitude of Simulated Outbreaks in the %s U.S.", region_title), 
      theme = theme(
        plot.margin = unit(c(1, 0.5, 0.75, 0.75), "cm"), 
        plot.title = element_text(size = 28, face = "bold", hjust = 0.5)
      ))
  
  return(combined_plot)
}