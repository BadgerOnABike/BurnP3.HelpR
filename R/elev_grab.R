#' Elevation Grabber - From NTS Grids
#'
#' This function uses your reference layer to define the NTS grids necessary to pull elevation from the CDEM layers provided by the Government of Canada.
#' @note Requires an internet connection
#'
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object.
#' @param output_directory Output directory where your elevation and Wind Ninja elevation grids will be stored.
#' @param aoi Polygon for the area of interest to be intersected with the NTS grid layer. _(Default: "")_ If Default is used the full extent under the reference_grid will be returned.
#'
#' @details The purpose of this function is to generate a common and rapid elevation layer that is sampled and masked to the reference grid for use within Burn-P3. A second elevation grid is also generated for use in Wind Ninja as that software will fail with NA values in the elevation grid.
#'
#' @importFrom terra rast crop merge writeRaster as.polygons project mask crs
#' @importFrom sf st_as_sf st_read st_crop st_transform
#'
#' @export
#'
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
#'                   output_directory = temp_dir)
#'
#'
#' unlink(temp_dir, recursive = T)

elev_grab <- function(aoi = NULL, reference_grid,output_directory){

  if ( grepl("SpatRaster", class(reference_grid)) ) { grast <- reference_grid }
  if ( grepl("character", class(reference_grid)) ) { grast <- terra::rast(reference_grid) }
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) { message("Reference Grid must be the directory of the spatraster or a spatraster object.") }

  if ( grepl("sf", class(aoi)) ) { aoi <- aoi }
  if ( grepl("character", class(aoi)) ) { aoi <- sf::read_sf(aoi) }
  if ( !grepl("sf|character", class(aoi)) ) { message("AOI must be a simple feature (sf) or a directory to a simple feature.") }

  if( !is.null(aoi)){
    e <- aoi
  }else{
    e <- sf::st_as_sf(as(terra::as.polygons(grast, extent=T),"Spatial"))
  }

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
                 sf::st_transform(e,crs = st_crs(nts_grid)))$NTS_SNRC

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

  unlink(c(gsub(".zip","",nts_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)

  if (class(aoi)[1] == "sf") {
    terra::writeRaster(terra::mask(mosaic.r,vect(aoi)),
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

}
