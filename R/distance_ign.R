# Euclidean Distance Calculator -------------------------------------------

distance_ign <- function(reference_grid, rasters_list,output_location,output_name){
  
  if( class(reference_grid)[1] == "RasterLayer"){grast <- reference_grid}
  if( class(reference_grid) == "character"){grast <- raster(reference_grid)}
  if( class(reference_grid) %ni% c("RasterLayer","character") ){message("Reference Grid must be the directory of the raster or a raster object.")}
  
  distance.r <- distance(rasters_list)
  writeRaster(mask(distance.r,grast),
              paste0(output_location,"/",output_name,"_distance.tif"),
              format="GTiff",
              overwrite =T)
}