# Function to call TPI, TRI and flowdir -----------------------------------

tpi <- function(elevation_grid, window_size){
  
  if( class(elevation_grid)[1] == "RasterLayer"){elev <- elevation_grid}
  if( class(elevation_grid) == "character"){elev <- raster(elevation_grid)}
  if( class(elevation_grid) %ni% c("RasterLayer","character") ){message("Reference Grid must be the directory of the raster or a raster object.")}
  
  tpi <- tpi_w(x=elev,w=window_size)
  tri <- TRI(x=elev, window_size)
  flow <- terrain(elev,opt="flowdir")
  out.r <- stack(tpi,tri,flow)
  names(out.r) <- c("tpi","tri","flowdir")
  return(out.r)
}