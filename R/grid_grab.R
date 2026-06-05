#' Gridded Data Grabber - From NTS Grids
#'
#' This function uses your reference layer to define the NTS grids necessary to pull elevation from the CDEM layers and the national FBP Fuel grid provided by the Government of Canada.
#' @note Requires an internet connection
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object.
#' @param output_directory Output directory where your elevation and Wind Ninja elevation grids will be stored.
#' @param aoi_e Polygon or numeric coordinate pair ("longitude","latitude") for the area of interest to be intersected with the NTS grid layer. _(Default: NULL)_ If Default is used the full extent under the reference_grid will be returned.
#' @param buffer A buffer in meters to define an area of interest around a point if coordinates are given to the `aoi_e` parameter. _(Default: NULL)_
#' @param fuel If you want the tool to process a fuel grid, set this to TRUE. _(Default: TRUE)_
#' @param ref_is_fuel If the reference grid is the fuel grid se this to TRUE. _(Default: FALSE)_
#'
#' @details The purpose of this function is to generate a common and rapid elevation layer that is sampled and masked to the reference grid for use within Burn-P3. A second elevation grid is also generated for use in Wind Ninja as that software will fail with NA values in the elevation grid. Fuel data from:
#'
#' @importFrom terra rast crop merge writeRaster as.polygons project mask crs vect
#' @importFrom sf st_as_sf st_crop st_transform st_crs st_agr st_sfc st_point st_buffer st_intersection
#'
#' @return SpatRast
#'
#' @export
#'
#' @references [National FBP Fuels](https://open.canada.ca/data/en/dataset/4e66dd2f-5cd0-42fd-b82c-a430044b31de/resource/f645a02d-fbb2-4c35-9bec-8d4ff516f5f8)
#' @references [Open Data Canada](https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333)
#' @references [Open Data Canada FTP](http://ftp.maps.canada.ca/pub/nrcan_rncan/vector/index/)
#' @references [Wind Ninja](https://www.firelab.org/project/windninja)
#'
#'
#' @examples
#' ## Load example data
#' ref_grid <- terra::rast(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#' temp_dir <- tempdir()
#' output_directory <- paste0(temp_dir,"\\")
#' test <- grid_grab(reference_grid = ref_grid,
#'                   output_directory = output_directory)
#'
#' ## use with a coordinate pair
#' test2 <- grid_grab(aoi_e = c(-110, 54),
#'                   buffer = 50000,
#'                   reference_grid = ref_grid,
#'                   output_directory = output_directory)
#'
#' ## use with a coordinate pair but no buffer
#' test3 <- grid_grab(aoi_e = c(-110, 54),
#'                   reference_grid = ref_grid,
#'                   output_directory = output_directory)
#'
#' ## use with a coordinate pair
#' test4 <- grid_grab(reference_grid = ref_grid,
#'                   output_directory = output_directory,
#'                   ref_is_fuel=T)
#'
#' ## If the reference grid is outside the location desired, it will return NAN
#' ## use with a coordinate pair
#' test5 <- grid_grab(aoi_e = c(-110, 54),
#'                   buffer = 50000,
#'                   reference_grid = ref_grid,
#'                   output_directory = output_directory,
#'                   ref_is_fuel=T)
#'
#' unlink(temp_dir)

grid_grab <- function(aoi_e = NULL,buffer = NULL, reference_grid = NULL,output_directory, fuel=TRUE, ref_is_fuel=FALSE){

  options(timeout = 900)

  if( is.numeric(aoi_e) & is.null(buffer) ) {message("Coordinates provided without a buffer, defaulting to 100km.")
    buffer <- 100000
    aoi_e <- sf::st_buffer(sf::st_sfc(sf::st_point(x=aoi_e,dim="XYZ"),crs=4326),dist = buffer)
  }
  if( is.numeric(aoi_e) & is.numeric(buffer) ) {
    aoi_e <- sf::st_buffer(sf::st_sfc(sf::st_point(x=aoi_e,dim="XYZ"),crs=4326),dist = buffer)
  }
  if ( !is.null(reference_grid) ){
  if ( any(grepl("SpatRaster", class(reference_grid))) ) { grast <- reference_grid }
  if ( any(grepl("character", class(reference_grid))) ) { grast <- terra::rast(reference_grid) }
  if ( any(!grepl("SpatRaster|character", class(reference_grid))) ) { message("Reference Grid must be the directory of the spatraster or a spatraster object.") }}

  if( is.null(aoi_e) ) { aoi_e <- sf::st_as_sf(terra::as.polygons(grast, extent=T))}

  if ( any(grepl("sf", class(aoi_e))) ) { aoi_e <- aoi_e }
  if ( any(grepl("character", class(aoi_e))) ) { aoi_e <- (aoi_e) }
  if ( !any(grepl("sf|character", class(aoi_e))) ) { message("aoi_e must be a simple feature (sf) or a directory to a simple feature.") }


  data(utm_canada)
  sf::st_agr(utm_canada) <- "constant"
  target_crs <- sf::st_intersection(utm_canada,sf::st_transform(sf::st_centroid(aoi_e),sf::st_crs(utm_canada)),)$EPSG

  bb_target <- round(sf::st_bbox(sf::st_transform(aoi_e,crs=target_crs)),-2)

  bbox_rast <- rast(
    ext(c(xmin = bb_target["xmin"], xmax = bb_target["xmax"],
          ymin = bb_target["ymin"], ymax = bb_target["ymax"])),
    res = 100,
    crs=st_crs(target_crs))
  bb_4326 <- bbox_rast|>
    st_bbox()|>
    st_as_sfc()|>
    st_set_crs(target_crs)|>
    st_transform(4326)|>
    st_bbox()

  bb_target <- round(sf::st_bbox(sf::st_transform(bb_4326,crs=target_crs)),-2)

  Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE) ## This will ensure the SSL intercept in corporate environments does not crater the grid_grab
  elevation <- mrdem_subset_windowed(bbox = bb_target, target_res_m = 100,target_crs = target_crs)
  names(elevation) <- "Elevation"

  if(fuel){
    if(ref_is_fuel){
      fuels <- reference_grid
      if(crs(fuels)!=crs(elevation)){warning("The CRS provided was not ideally North Up for fire modelling it has been adjusted to EPSG:",target_crs)}
      fuels <- terra::resample(terra::project(fuels,elevation,method = "near"),y = elevation,method="near")
      if(all(is.na(unique(fuels[])))){warning("Fuel layer did not fall within desired location, run again with ref_is_fuel = FALSE.")}
      } else {
      fuel.url<-paste0("https://cwfis.cfs.nrcan.gc.ca/geoserver/public/wcs?",
                        "service=WCS&version=2.0.0&request=GetCoverage&coverageId=",
                        "public:cffdrs_fbp_fuel_types_100m&subset=Long(",
                        bb_4326[1],",",bb_4326[3],")&subset=Lat(",bb_4326[2],",",bb_4326[4],
                        ")&FORMAT=geotiff&subsettingCRS=EPSG:4326&outputCRS=http://www.opengis.net/def/crs/EPSG/0/",target_crs
                )

      fuels <- terra::resample(rast(fuel.url),y = elevation,method="near")}

    names(fuels) <- "Fuel"

    terra::writeRaster(terra::crop(fuels,bb_target),
                         paste0(output_directory,"FBP_Fuels_epsg_",target_crs,".tif"),
                         wopt = list(filetype = "GTiff",
                                     datatype = "INT2S",
                                     gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2","TILED=YES","BLOCKXSIZE=512", "BLOCKYSIZE=512")),
                         NAflag = -9999,
                         overwrite = T)
    }

  terra::writeRaster(terra::crop(elevation,bb_target),
                paste0(output_directory,"elevation_epsg_",target_crs,".tif"),
                wopt = list(filetype = "GTiff",
                            datatype = "INT2S",
                            gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2","TILED=YES","BLOCKXSIZE=512", "BLOCKYSIZE=512")),
                NAflag = -9999,
                overwrite = T)

print(paste0("Files have been written to: ",output_directory))
if(fuel == TRUE){return(c(fuels,elevation))} else {return(elevation)}
}

elev_grab <- function(...) {
  .Deprecated("grid_grab")
  return(grid_grab(...))
}

