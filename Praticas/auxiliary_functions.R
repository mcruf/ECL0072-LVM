####################################
#                                  #
#        Auxiliary functions       #
#                                  #
####################################

# This script contains a collection of auxiliary functions used throughout the practical lessons of the course.
# Last update: August 2026



#~~~~~~~~~~~~~~~~~~~
# Load R packages
#~~~~~~~~~~~~~~~~~~~
# Ensures that all packages are loaded (and installed, if necessary!)

## Package list
required_pkgs <- c("terra", "sf", "dplyr", "lubridate" )

## Missing packages
missing_pkgs <- required_pkgs[ !required_pkgs %in% installed.packages()[, "Package"]]

## Install missing packages
if(length(missing_pkgs) > 0){
  
  message("Installing missing packages...")
  
  install.packages(missing_pkgs)
  
}

## Load packages
invisible(lapply(required_pkgs, library, character.only = TRUE))


## Removing obsolete objects
rm(list = c("missing_pkgs", "required_pkgs"))


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#####################################
#             FUNCTIONS             #
##################################### 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract dynamic raster values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ATENTION: Function currenlty applies only to dynamic oceanographic layers extracted from Copernicus (https://data.marine.copernicus.eu/products)
# FIXME: Function needs to be extended for time resolutions other than monthly (e.g, daily)
# FIXME: Adapt functions for datasets with actual year information 

# ARGUMENTS:
#' @param points_sf - sf object containing point data with 'Year' and 'Month' columns
#' @param env_raster - SpatRaster containing environmental data with time attributes
#' @param var_name - Character string specifying the variable name prefix to filter (e.g., "thetao"); variable name needs to match copernicus naming convention
#' @param year_map - Named vector mapping numeric Year values in points to actual calendar years. Example: c("1" = 2000, "2" = 2001)


extract_env_dynamic <- function(points_sf, env_raster, var_name, year_map = NULL) {
  
  ## Match CRS
  if (st_crs(points_sf) != crs(env_raster)) {
    points_sf <- st_transform(points_sf, crs(env_raster))
  }
  
  ## Prepare MonthYear column in points dataset
  pts_processed <- points_sf
  
  if (!is.null(year_map)) {
    pts_processed <- pts_processed %>%
                     mutate(Year_cal = year_map[as.character(Year)])
  } else {
    pts_processed <- pts_processed %>%
                     mutate(Year_cal = Year)
  }
  
  pts_processed <- pts_processed %>%
                   mutate(MonthYear = paste(Year_cal, sprintf("%02d", as.numeric(Month)), sep = "-"))
  
  
  ## Filter raster by variable name
  cidx <- grep(paste0("^", var_name), names(env_raster)) #get col index
  
  if (length(cidx) == 0) {
    stop(paste("Variable prefix", var_name, "not found in raster layer names."))
  }
  rast_sub <- env_raster[[cidx]]
  
  
  ## Extract time attributes and set formatted names
  r_times <- time(rast_sub)
  
  if (any(is.na(r_times))) {
    stop("Oops! Seems like raster layers lack proper time metadata. Ensure time() is defined.")
  }
  time_labels <- paste(year(r_times), sprintf("%02d", month(r_times)), sep = "-")
  names(rast_sub) <- time_labels
  
  ## Extract values matching MonthYear
  extracted_results <- vector("list", nlyr(rast_sub))
  
  for (j in seq_len(nlyr(rast_sub))) {
    target_time <- time_labels[j]
    rast_j <- rast_sub[[j]]
    
    
    ## Filter points for current month-year
    pts_j <- pts_processed %>% filter(MonthYear == target_time)
    
    if (nrow(pts_j) > 0) {
      
      ## Extract values
      vals <- terra::extract(rast_j, pts_j)
      
      ## Store extracted value alongside point row ID
      extracted_results[[j]] <- data.frame(row_id = which(pts_processed$MonthYear == target_time),
                                           extracted_val = vals[, 2]) # column 1 is ID, column 2 is layer value

    }
  }
  
  ## Bind results and append to original sf object
  res_df <- bind_rows(extracted_results) %>% arrange(row_id)
  
  ## Re-align with original points order
  output_col <- rep(NA_real_, nrow(points_sf))
  output_col[res_df$row_id] <- res_df$extracted_val
  
  ## Add dynamic column name to sf dataset
  points_sf[[var_name]] <- output_col
  
  return(points_sf)
}


