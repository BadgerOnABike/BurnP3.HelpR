# Spatial Data Rasterizer -------------------------------------------------

spatdat_ign_rast <- function(reference_grid, layers_list){
  
  if ( class(reference_grid)[1] == "RasterLayer") {grast <- reference_grid}
  if ( class(reference_grid) == "character") {grast <- raster(reference_grid)}
  if ( class(reference_grid) %ni% c("RasterLayer","character") ) {message("Reference Grid must be the directory of the raster or a raster object.")}
  
  rasters_list <- lapply(X = layers_list, 
                         FUN = function(x) {
                           out.r = setValues(grast,0)
                           if (grepl("line",class(x)[1],ignore.case = T)) {
                             cells_in <- unlist(
                               cellFromLine(object = setValues(grast,0), 
                                            lns = x)
                             )
                           }
                           if (grepl("polygon",class(x)[1],ignore.case = T)) {
                             cells_in <- unlist(
                               cellFromPolygon(object = setValues(grast,0), 
                                               p = x,
                                               weights = F)
                             )
                             if (is.null(cells_in)) x <- SpatialPoints(centroid(x),proj4string = CRS(proj4string(grast)))
                           }
                           
                           if (grepl("point",class(x)[1],ignore.case = T)) {
                             cells_in <- cellFromXY(setValues(grast,0), x)
                           }
                           out.r[cells_in] <- 1
                           out.r <- mask(out.r,grast)
                         }
  )
  
  if(length(rasters_list) == 1){
    rasters.out <- reclassify(rasters_list[[1]],rcl = c(-Inf,0,NA))
  } else{
    rasters.out <- reclassify(
      sum(
        stack(rasters_list),
        na.rm = T),
      rcl = c(-Inf,0,NA)
    )
  }
  
  points.out <- SpatialPointsDataFrame(rasterToPoints(rasters.out)[,c("x","y")],
                                       data = data.frame(rasterToPoints(rasters.out)),
                                       proj4string = CRS(proj4string(grast))
  )
  
  return(list(points = points.out,
              rasters = rasters.out)
  )
}