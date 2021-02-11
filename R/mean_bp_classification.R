#' Mean Burn Probability Classification
#'
#' This function creates an output raster that represents the devisation from the mean burn probability on a landscape. This is intended for use when presenting burn probability to better demonstrate the deviance from the mean across the landscape and draw attention to areas that are considerably above and below the mean burn probability.
#'
#' @param input Required as a SpatRast or a character string for the directory of the raster information.
#' @param output_filename A character sting that defines the output directory and filename for the output raster.
#'
#' @return
#' @export
#'
#' @examples
mean_bp_classification <- function(input,output_filename){

  if ( grepl("SpatRast", class(input)) ) { bp <- input }
  if ( grepl("character", class(input)) ) { bp <- terra::rast(input) }
  if ( !grepl("SpatRast|character", class(input)) ) { message("Reference Grid must be the directory of the raster or a raster object.") }

  bp[][bp[] == 0] <- NA
  mean_bp <- mean(x = bp[],
                  na.rm = T)
  mean_bp.r <- bp/mean_bp
  bp_vals <- terra::values(mean_bp.r)
  bp_vals[bp_vals[] < 1 & bp_vals[] > 0 & !is.na(bp_vals)] <- (1/bp_vals[bp_vals[] < 1 & bp_vals[] > 0 & !is.na(bp_vals)])*-1
  mean_bp.r <- terra::setValues(x = mean_bp.r,
                                values = bp_vals
                                )
  mean_bp.r <- terra::classify(mean_bp.r,
                               rcl = matrix(ncol = 3,
                                            byrow = T,
                                            data = c(-Inf,-10,-11,10,Inf,11)
                                            )
                               )

  terra::writeRaster(x = mean_bp.r,
                     filename = output_filename,
                     overwrite = T,
                     wopts = list(filetype = "GTiff",
                                  datatype = "INT2S",
                                  gdal = c("COMPRESS=LZW",
                                           "TFW=YES"),
                                  NAflag = -9999
                                  )
                     )
}
