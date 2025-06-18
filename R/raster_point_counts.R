raster_point_counts <- function(western_paths, reference, west_r, max_iteration = NA) {

  require(dplyr)
  require(terra)
  require(here)
  
  # to store the rasters for each scenario
  scenario_avg_list <- list()
  
  # loop over each scenario file
  for(i in seq_along(western_paths)) {
    
    points_tmp <- read.table(here(western_paths[i]))
    
    # column names
    names(points_tmp) <- c("iteration",
                           "detect_day",
                           "premises",
                           "surveillance",
                           "x",
                           "y",
                           "cattle")
    
    # join with the reference coordinates
    points_tmp <- left_join(points_tmp, reference, by = "premises")
    
    # set number of iterations to use
    if(is.na(max_iteration)){ 
      iterations <- sort(unique(points_tmp$iteration))
    } else{
      iterations <- max_iteration
    }
    
    # to store raster counts for each iteration
    iter_rasters <- list()
    
    # loop through iterations
    for(j in iterations) {
   
      tmp_iter <- points_tmp %>% filter(iteration == j)
      
      tmp_vect <- vect(tmp_iter, geom = c("x", "y"), crs = crs(west_r))
      
      # rasterize the points onto the template raster
      point_counts_r <- rasterize(tmp_vect, west_r, fun = "length", background = 0)
      
      # mask to set oceans to NA
      point_counts_r <- mask(point_counts_r, west_r)
      
      iter_rasters[[as.character(j)]] <- point_counts_r
    }
    
    if(length(iter_rasters) > 0) {
      iter_stack <- rast(iter_rasters)  
      avg_raster <- app(iter_stack, fun = sum, na.rm = TRUE)  # count points
    } else {

      avg_raster <- rast(west_r)
      values(avg_raster) <- NA
    }
    
    # get scenario name
    scenario_name <- sub("^.*outputs_(western|central|eastern)/", "", western_paths[i])
    scenario_name <- sub("/Outputs_Detection.txt$", "", scenario_name)
    
    # rename the layer
    names(avg_raster) <- scenario_name
    
    scenario_avg_list[[i]] <- avg_raster
  }
  
  # combine scenarios rasters into a single stack
  count_rasts <- do.call(c, scenario_avg_list)
  
  return(count_rasts)
}

