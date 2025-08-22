#' Gridded Data Grabber Grabber - From NTS Grids
#'
#' This function uses your reference layer to define the NTS grids necessary to pull elevation from the CDEM layers and the national FBP Fuel grid provided by the Government of Canada.
#' @note Requires an internet connection
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object.
#' @param output_directory Output directory where your elevation and Wind Ninja elevation grids will be stored.
#' @param aoi_e Polygon for the area of interest to be intersected with the NTS grid layer. _(Default: "")_ If Default is used the full extent under the reference_grid will be returned.
#'
#' @details The purpose of this function is to generate a common and rapid elevation layer that is sampled and masked to the reference grid for use within Burn-P3. A second elevation grid is also generated for use in Wind Ninja as that software will fail with NA values in the elevation grid. Fuel data from:
#'
#' @importFrom terra rast crop merge writeRaster as.polygons project mask crs
#' @importFrom sf st_as_sf st_read st_crop st_transform st_crs
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
#' ref_grid <- rast(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#' temp_dir <- tempdir()
#' test <- elev_grab(reference_grid = ref_grid,
#'                   output_directory = paste0(temp_dir,"\\"))
#'
#'
#' unlink(temp_dir, recursive = T)

grid_grab <- function(aoi_e = NULL, reference_grid,output_directory){

  options(timeout = 900)

  if ( any(grepl("SpatRaster", class(reference_grid))) ) { grast <- reference_grid }
  if ( any(grepl("character", class(reference_grid))) ) { grast <- terra::rast(reference_grid) }
  if ( any(!grepl("SpatRaster|character", class(reference_grid))) ) { message("Reference Grid must be the directory of the spatraster or a spatraster object.") }

  if( is.null(aoi_e) ) { aoi_e <- sf::st_as_sf(as.polygons(grast, extent=T))}

  if ( any(grepl("sf", class(aoi_e))) ) { aoi_e <- aoi_e }
  if ( any(grepl("character", class(aoi_e))) ) { aoi_e <- sf:read_sf(aoi_e) }
  if ( !any(grepl("sf|character", class(aoi_e))) ) { message("aoi_e must be a simple feature (sf) or a directory to a simple feature.") }

  ## Download and extract the NTS grid specified by user (will be removed after use), uses the 250k grid as that is what CDEM is based on
  nts_temp <- tempfile(fileext = '.zip')
  download.file(destfile = nts_temp,url = "http://ftp.maps.canada.ca/pub/nrcan_rncan/vector/index/nts_snrc.zip")
  nts_files <- unzip(nts_temp,list = T)
  unzip(zipfile = nts_temp,
        files = nts_files$Name,
        exdir = gsub(".zip","",nts_temp))
  nts_grid <- sf::st_read(dsn = gsub(".zip","",nts_temp),
                      layer = "nts_snrc_250k")

  ## Determine the NTS grids the data exists across
  layers <- sf::st_crop(nts_grid,
                 sf::st_transform(aoi_e,crs = sf::st_crs(nts_grid)))$NTS_SNRC

  ## Extract the CDEM tiles needed to generate the grid.
  elevation <- lapply(layers,function(i){
    loc <- tempfile(fileext = ".zip")
    loc <- gsub("\\\\","/",loc)
    download.file(url = paste0("http://ftp.geogratis.gc.ca/pub/nrcan_rncan/elevation/cdem_mnec/",
                               substr(i,1,3),
                               "/cdem_dem_",
                               i,
                               "_tif.zip"),
                  destfile = loc)
    files <- unzip(loc,list = T)

    terra::rast(paste0("/vsizip/",loc,"/",files$Name[grep(".tif$",files$Name)]))

  }
  )

  if (length(elevation) > 1) {
    mosaic.r <- do.call(merge,elevation)
  } else{
    mosaic.r <- elevation[[1]]
  }

  mosaic.r <-  terra::crop(terra::project(x =  mosaic.r, y = grast),grast)

  bb_4326 <- st_bbox(st_transform(aoi_e,crs="EPSG:4326"))
  fuel.url<-paste0("https://cwfis.cfs.nrcan.gc.ca/geoserver/public/wcs?",
                    "service=WCS&version=2.0.0&request=GetCoverage&coverageId=",
                    "public:cffdrs_fbp_fuel_types_100m&subset=Long(",
                    bb_4326[1],",",bb_4326[3],")&subset=Lat(",bb_4326[2],",",bb_4326[4],
                    ")&FORMAT=geotiff&subsettingCRS=EPSG:4326&outputCRS=http://www.opengis.net/def/crs/EPSG/0/3402"
            )
  fuels <- resample(terra::project(rast(fuel.url),terra::crs(aoi_e),method = "near"),mosaic.r)

  terra::project(rast(fuel.url),terra::crs(aoi_e),method = "near", res=terra::res(mosaic.r))


  unlink(c(gsub(".zip","",nts_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)

  if (class(aoi_e)[1] == "sf") {
    terra::writeRaster(terra::mask(fuels,vect(aoi_e)),
                       paste0(output_directory,"FBP_National_Fuels.tif"),
                       wopt = list(filetype = "GTiff",
                                   datatype = "INT2S",
                                   gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")),
                       NAflag = -9999,
                       overwrite = T)

    terra::writeRaster(terra::mask(mosaic.r,vect(aoi_e)),
                paste0(output_directory,"elevation.tif"),
                wopt = list(filetype = "GTiff",
                            datatype = "INT2S",
                            gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")),
                NAflag = -9999,
                overwrite = T)
  }
    terra::writeRaster(mosaic.r,
                paste0(output_directory,"elevation_wn.tif"),
                wopt = list(filetype = "GTiff",
                            datatype = "INT2S",
                            gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")),
                NAflag = -9999,
                overwrite = T)
print(paste0("Files have been written to: ",dir(output_directory)))
}

elev_grab <- function(...) {
  .Deprecated("grid_grab")
  return(grid_grab(...))
}
