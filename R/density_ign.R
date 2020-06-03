#' Kernal Density Calculator -----------------------------------------------
#'
#' Generate kernal density layers from a list of layers and write the rasters out
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.
#' @param layers_list A point layer or list of point layers of which you are interested in the point layer.
#' @param output_location The location the output density rasters will be placed.
#' @param output_name The name of the files to be output. Typically these will be a vector of the names of your list of layers.
#'
#' @details This function utilizes the kernal density estimation algorithm from spatialEco to create raster layers for use in ignition grid calculation.
#'
#' @return RasterLayer
#'
#' @seealso \code{\link[spatialEco]{sp.kde}}
#'

density_ign <- function(reference_grid,layers_list,output_location,output_name){

  if( class(reference_grid)[1] == "RasterLayer"){grast <- reference_grid}
  if( class(reference_grid) == "character"){grast <- raster(reference_grid)}
  if( class(reference_grid) %ni% c("RasterLayer","character") ){message("Reference Grid must be the directory of the raster or a raster object.")}

  density.r <- sp.kde(crop(layers_list,grast),bw = 1000,newdata = grast)
  writeRaster(mask(density.r, grast),
              paste0(output_location,"/",output_name,"_density.tif"),
              format="GTiff",
              overwrite =T)
}
