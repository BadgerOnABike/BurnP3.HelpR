# Spatial Data Importer ---------------------------------------------------

spatdat_ign_layer <- function(reference_grid,input_layer_names,input_shapes){
  if ( class(reference_grid)[1] == "RasterLayer") {grast <- reference_grid}
  if ( class(reference_grid) == "character") {grast <- raster(reference_grid)}
  if ( class(reference_grid) %ni% c("RasterLayer","character") ) {message("Reference Grid must be the directory of the raster or a raster object.")}
  
  if (class(input_layer_names) != "character") {
    x <- st_transform(input_layer_names,crs=CRS(proj4string(grast)))
    y <- st_geometry_type(x)
    if(length(unique(y)) > 1 & gregexpr("SURFACE",unique(y))[[1]][1] == -1){
      x <- x[-which(y == "MULTISURFACE"),]
    }
    if(length(st_is_empty(x)) > 0){
      x <- x[!st_is_empty(x),]
    }
    layers_list <- as(x,"Spatial")
  }else{
    
    layers_list <- lapply(ogrListLayers(dsn = input_shapes)[grep(input_layer_names,
                                                                 ogrListLayers(dsn = input_shapes),
                                                                 ignore.case = T)],
                          FUN = function(x){
                            x <- st_transform(read_sf(dsn = input_shapes, 
                                                      layer = x),
                                              crs = CRS(proj4string(grast)))
                            
                            x <- st_zm(x)
                            y <- st_geometry_type(x)
                            if (length(unique(y)) > 1 & gregexpr("SURFACE",unique(y))[[1]][1] == -1){
                              x <- x[-which(y == "MULTISURFACE"),]
                            }
                            if (length(st_is_empty(x)) > 0){
                              x <- x[!st_is_empty(x),]
                            }
                            x <- as(x,"Spatial")
                            x <- crop(x,grast)
                          }
    )
  }
  return(layers_list)
}