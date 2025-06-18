smooth_raster <- function(r_stack, disagg_factor = 2, window_size = 3, fun = mean, na.rm = TRUE) {

  if (!inherits(r_stack, "SpatRaster")) {
    stop("Input must be a terra SpatRaster object.")
  }
  
  # focal window
  focal_mat <- matrix(1, nrow = window_size, ncol = window_size)
  
  # convert to list
  layer_list <- as.list(r_stack)
  
  # disaggregation and focal smoothing to each layer
  smoothed_layers <- lapply(layer_list, function(layer) {
    disagg_layer <- disagg(layer, fact = disagg_factor, method = "bilinear")
    smoothed_layer <- focal(disagg_layer, w = focal_mat, fun = fun, na.rm = na.rm)
    return(smoothed_layer)
  })
  
  # recombine into a SpatRaster stack
  smoothed_stack <- rast(smoothed_layers)
  names(smoothed_stack) <- names(r_stack)
  
  return(smoothed_stack)
}
