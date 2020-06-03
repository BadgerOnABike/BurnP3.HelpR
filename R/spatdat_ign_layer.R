#' Spatial Data Importer
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.
#' @param layer Names of the input layers for import, allows the system to function if there are multiple shapes at a single data source name.
#' @param dsn Locations of the shapefiles to be loaded in and operated upon. Also accepts a list containing sf objects.
#'
#' @return List
#' @export
#'
#' @import raster
#' @import rgdal
#' @import sf
#'
#' @examples
#'
#' ## Load example data
#' ref_grid <- raster(system.file("extdata/fuel.tif",package = "BurnP3"))
#'
#' out <- spatdat_ign_layer(reference_grid = ref_grid,
#'                          layer = "roads",
#'                          dsn = system.file("extdata",package="BurnP3"))
spatdat_ign_layer <- function(reference_grid,layer,dsn){

  if( grepl("RasterLayer", class(reference_grid)) ){grast <- reference_grid}
  if( grepl("character", class(reference_grid)) ){grast <- raster(reference_grid)}
  if( !grepl("RasterLayer|character", class(reference_grid)) ){message("Reference Grid must be the directory of the raster or a raster object.")}


  if (class(layer) != "character") {
    x <- st_transform(layer,crs=CRS(proj4string(grast)))
    y <- st_geometry_type(x)
    if(length(unique(y)) > 1 & gregexpr("SURFACE",unique(y))[[1]][1] == -1){
      x <- x[-which(y == "MULTISURFACE"),]
    }
    if(length(st_is_empty(x)) > 0){
      x <- x[!st_is_empty(x),]
    }
    layers_list <- as(x,"Spatial")
  }else{

    if(!grepl("gdb",dsn,ignore.case = T)){
      x <- st_transform(read_sf(dsn = dsn,
                                layer = layer),
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
      layers_list <- x
    }

    if(grepl("gdb",dsn,ignore.case = T)){
    layers_list <- lapply(ogrListLayers(dsn = dsn)[grep(layer,
                                                        ogrListLayers(dsn = dsn),
                                                        ignore.case = T)],
                          FUN = function(x){
                            x <- st_transform(read_sf(dsn = dsn,
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
    )}
  }
  return(layers_list)
}
