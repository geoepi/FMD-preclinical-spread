
import_data <- function(file_path, data_type, columns) {
  
  require(tidyverse)
  require(here)
  
  file_check <- list.files(path = here(file_path), recursive = T, 
                           pattern = data_type, full.names=TRUE)
  
  file_check_df <- read.csv(file_check[1])
  
  head(file_check_df)
  
  data <- list.files(path = here(file_path), recursive = T, pattern = data_type, full.names = TRUE) %>%
    map_dfr(function(path) {
      folders <- str_extract(path, "(?<=outputs/).+(?=/)")
      regional_scenario <- str_extract(path, "(?<=outputs_).+(?=/.+/.+/)")
      response_type <- str_extract(path,(sprintf("(?<=outputs/outputs_%s/).+(?=/.-day)", regional_scenario)))
      preclinical <- str_extract(path, ".(?=-day)")
      filename <- str_extract(path, "(?<=/Outputs_).+(?=\\.txt$)")
      read.table(path,
                 quote="\"",
                 comment.char="",
                 col.names= columns) %>%
        mutate(region) %>%
        mutate(response_type) %>%
        mutate(preclinical)
    })
  
  return(data)
}

import_infection_data <- function(file_path) {
  
  infection_outputs <- "Outputs_Infection"

  infect_columns <- c("iteration",
                      "infect_day",
                      "premises",
                      "source",
                      "pathway",
                      "x",
                      "y",
                      "cattle")
  
  infection <- import_data(file_path, infection_outputs, infect_columns)
  
  return(infection)
}

import_detection_data <- function(file_path) {

  detection_outputs <- "Outputs_Detection"
    
  detect_columns <- c("iteration",
                      "detect_day",
                      "premises",
                      "surveillance",
                      "x",
                      "y",
                      "cattle")
  
  detection <- import_data(file_path, detection_outputs, detect_columns)
  
  return(detection)
}