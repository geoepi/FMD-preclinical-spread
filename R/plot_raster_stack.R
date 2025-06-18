plot_raster_stack <- function(raster_stack, boundaries) {
  
  require(terra)
  require(ggplot2)
  require(tidyr)
  require(sf)
  require(scales)
  require(viridis)
  require(dplyr)
  require(stringr)
  
  raster_stack_geo <- raster_stack
  boundaries_geo <- st_as_sf(boundaries)
  
  # to a data frame
  df <- as.data.frame(raster_stack_geo, xy = TRUE)
  
  df_long <- pivot_longer(df, 
                          cols = -c(x, y), 
                          names_to = "scenario", 
                          values_to = "avg_count")
  
  # split the scenario names 
  df_long <- df_long %>%
    mutate(
      scenario_type = str_to_title(word(scenario, 1, sep = "/")),
      preclinical_delay = word(scenario, 2, sep = "/") %>% 
        str_replace_all("-days?-preclinical", "") %>% 
        str_trim() %>% 
        paste("Days Preclinical", .) %>% 
        str_to_title()
    )
  
  # force scale to start at 1
  df_long <- df_long %>% 
    mutate(fill_value = ifelse(avg_count < 1, NA, avg_count))
  
  # maximum nonzero value 
  max_val <- max(df_long$fill_value, na.rm = TRUE)
  
  breaks_vec <- 10^(seq(0, ceiling(log10(max_val)), by = 1))
  
  # custom log1p transformation to handle zeros
  log1p_trans <- trans_new("log1p", transform = log1p, inverse = expm1)
  
  df_long$scenario_type[df_long$scenario_type == "Delayed"] <- "Suboptimal"
  df_long$preclinical_delay[df_long$preclinical_delay == "Days Preclinical 0"] <- "None"
  df_long$preclinical_delay[df_long$preclinical_delay == "Days Preclinical 1"] <- "1 Day"
  df_long$preclinical_delay[df_long$preclinical_delay == "Days Preclinical 2"] <- "2 Days"
  df_long$preclinical_delay[df_long$preclinical_delay == "Days Preclinical 3"] <- "3 Days"
  df_long$preclinical_delay[df_long$preclinical_delay == "Days Preclinical 6"] <- "6 Days"
  
  if(length(unique(df_long$preclinical_delay)) > 1){
    
    df_long$preclinical_delay <- ordered(factor(df_long$preclinical_delay,
                                              c("None", "1 Day", "2 Days", "3 Days")))
  }
  
  p <- ggplot() +
    geom_tile(data = filter(df_long, !is.na(fill_value)),
                aes(x = x, y = y, fill = fill_value)) +
    geom_tile(data = filter(df_long, avg_count < 1),
                aes(x = x, y = y), fill = "white") +
    facet_grid(scenario_type ~ preclinical_delay) +
    scale_fill_viridis_c(
      option = "turbo",
      trans = log1p_trans,
      na.value = "white",
      name = "Detections",
      limits = c(1, max_val),
      breaks = breaks_vec,
      labels = label_comma()
    ) +
    geom_sf(data = boundaries_geo, inherit.aes = FALSE, fill = NA, color = "black", size = 0.5) +
    coord_sf() +
    scale_x_continuous(name = "Easting (km)", labels = function(x) x/1000) +
    scale_y_continuous(name = "Northing (km)", labels = function(x) x/1000) +
    labs(title = " ",
         x = "Easting",
         y = "Northing") +
    theme_minimal() +
    theme(
      #plot.margin    = unit(c(0.05, 0.25, 0.05, 0.25), "cm"),
      legend.position = "bottom",
      strip.text     = element_text(size = 18, face = "bold", color = "gray40"),
      axis.title.x   = element_text(size = 18, face = "bold"),
      axis.title.y   = element_text(size = 18, face = "bold"),
      axis.text.x    = element_text(size = 12, face = "bold"),
      axis.text.y    = element_text(size = 12, face = "bold"),
      legend.key.width = unit(3, "line"),
      legend.key.height = unit(1, "line"),
      legend.text = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 18, face = "bold"),
      plot.title     = element_text(size = 22, face = "bold", hjust = 0.5)
    )
  
  return(p)
}
