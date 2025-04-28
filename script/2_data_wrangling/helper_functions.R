# ----------------------------------------
# R Script to store helper functions used in the data_wrangling module
# Author: Xindi Hu
# Last edited: 2025-04-22
# -- ---------------------------------------- 

library(tidyverse)
library(sf)
library(exactextractr)
library(terra)
library(readxl)
library(units)
library(future)
library(future.apply)
library(progressr)

# Use all available cores
plan(multisession)

#' Compute Spatial Impact Score with Exponential Decay, Weights, and Custom Output Name
#'
#' @param points_sf An `sf` object of spatial points (e.g., pollution sources).
#' @param grid_sf An `sf` object of polygons representing the grid cells.
#' @param threshold_m Numeric. Threshold distance in meters beyond which points are ignored. Default is 5000 (5 km).
#' @param decay_function A function to apply to distances (in kilometers). Default is `function(d_km) 1 / exp(d_km)`.
#' @param weight_var Optional. Character string indicating a column in `points_sf` to use as a weight. If NULL, all points have equal weight.
#' @param output_var Character. The name of the column in the output grid that stores the impact score. Default is "impact".
#' @param crs_proj Numeric or string. EPSG code of a projected CRS (in meters). Default is 3358 (NAD83(HARN) / North Carolina).
#'
#' @return An `sf` object with the grid and a new column named by `output_var`.
#'
#' @export
compute_impact_score <- function(points_sf, grid_sf, 
                                 threshold_m = 5000, 
                                 decay_function = function(d_km) 1 / exp(d_km), 
                                 weight_var = NULL,
                                 output_var = "impact",
                                 crs_proj = 3358) {
  # Reproject to common projected CRS
  points_proj <- st_transform(points_sf, crs_proj)
  grid_proj <- st_transform(grid_sf, crs_proj)
  
  # Calculate centroids of grid cells
  grid_centroids <- st_centroid(st_geometry(grid_proj))
  
  # Convert threshold to units
  threshold <- set_units(threshold_m, "m")
  
  # Setup progress
  p <- progressor(steps = length(grid_centroids))
  
  # Compute impact score for each grid cell
  impact_scores <- future_lapply(seq_len(length(grid_centroids)), function(i) {
    p()  # update progress
    
    centroid <- grid_centroids[i, ]
    # Distance from this centroid to all points, projected CRS does not need spherical distance, s2 = FALSE
    dists <- st_distance(centroid, points_proj, s2 = FALSE)
    
    # Filter distances within threshold
    within_thresh <- dists < threshold
    d <- dists[within_thresh]
    
    if (length(d) == 0) {
      return(0)
    } else {
      d_km <- as.numeric(d) / 1000  # Convert to km
      decay_vals <- decay_function(d_km)
      
      if (!is.null(weight_var)) {
        weights <- points_proj[[weight_var]][within_thresh]
        return(sum(decay_vals * weights, na.rm = TRUE))
      } else {
        return(sum(decay_vals))
      }
    }
  })
  
  # Add results to grid under the specified output_var name
  grid_proj[[output_var]] <- unlist(impact_scores)
  return(grid_proj)
}
