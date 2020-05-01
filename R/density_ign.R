# Kernal Density Calculator -----------------------------------------------

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