#' Wildfire Hazard Raster Generator
#'
#'
#'
#' @param stats_file Character string to the location of the statistics file, if there have been multiple runs aggregated, the aggregate statistics file.
#' @param median_pattern
#' @param mean_pattern
#' @param burn_count_pattern
#' @param intensity_directory
#' @param burn_count_directory
#'
#' @importFrom raster raster stack
#'
#' @return
#' @export
#'
#' @examples
hazard_rast <- function(stats_file,median_pattern,mean_pattern,burn_count_pattern,intensity_directory,burn_count_directory){

  bp_stats <- read.csv(stats_file)

  median_fire_intensity <- terra::rast(
    list.files(intensity_directory,
               pattern = "FIMed",
               full.names = T,
               recursive = T)
  )

  mean_fire_intensity <- terra::rast(
    list.files(intensity_directory,
               pattern = "FIAvg",
               full.names = T,
               recursive = T)
  )

  burn_count <- terra::rast(
    list.files(burn_count_directory,
               pattern = "Probability.tif$|Iteration.tif$|Count.tif$",
               full.names = T,
               recursive = T)
  )

  out_list <- list(bp_stats,
                   median_fire_intensity,
                   mean_fire_intensity,
                   burn_count)
  names(out_list) <- c("bp_stats",
                       "median_fire_intensity",
                       "mean_fire_intensity",
                       "burn_count")

  return(out_list)

}


