#' Elevation Grabber - From NTS Grids
#'
#' This function uses your reference layer to define the NTS grids necessary to pull elevation from the CDEM layers provided by the Government of Canada.
#' @note Requires an internet connection
#' @param reference_grid This is a reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project. Can be either the location of the raster or a raster object.
#' @param output_directory Output directory where your elevation and Wind Ninja elevation grids will be stored.
#'
#' @details The purpose of this function is to generate a common and rapid elevation layer that is sampled and masked to the reference grid for use within Burn-P3. A second elevation grid is also generated for use in Wind Ninja as that software will fail with NA values in the elevation grid.
#'
#' @return RasterLayer
#' @export
#'
#' @references [Open Data Canada](https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333)
#' @references [Open Data Canada FTP](http://ftp.maps.canada.ca/pub/nrcan_rncan/vector/index/)
#' @references [Wind Ninja](https://www.firelab.org/project/windninja)
#'
#'
#' @examples
#' ## Load example data
#' ref_grid <- raster(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#' temp_dir <- tempdir()
#' test <- elev_grab(reference_grid = ref_grid,
#'                   output_directory = temp_dir)
#'
#'
#' unlink(temp_dir, recursive = T)

elev_grab <- function(aoi = "", reference_grid,output_directory){


  if ( grepl("RasterLayer", class(reference_grid)) ) { grast <- reference_grid }
  if ( grepl("character", class(reference_grid)) ) { grast <- raster(reference_grid) }
  if ( !grepl("RasterLayer|character", class(reference_grid)) ) { message("Reference Grid must be the directory of the raster or a raster object.") }

  e <- as(extent(grast),"SpatialPolygons")
  proj4string(e) <- proj4string(grast)

  ## Download and extract the NTS grid specified by user (will be removed after use), uses the 250k grid as that is what CDEM is based on
  nts_temp <- tempfile(fileext = '.zip')
  download.file(destfile = nts_temp,url = "http://ftp.maps.canada.ca/pub/nrcan_rncan/vector/index/nts_snrc.zip")
  nts_files <- unzip(nts_temp,list = T)
  unzip(zipfile = nts_temp,
        files = nts_files$Name,
        exdir = gsub(".zip","",nts_temp))
  nts_grid <- rgdal::readOGR(dsn = gsub(".zip","",nts_temp),
                      layer = "nts_snrc_250k")

  ## Determine the NTS grids the data exists across
  layers <- raster::crop(nts_grid,
                 spTransform(e,CRSobj = CRS(proj4string(nts_grid))))$NTS_SNRC

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

    raster(paste0("/vsizip/",loc,"/",files$Name[grep(".tif$",files$Name)]))

  }
  )

  elev_layers <- lapply(elevation,function(x) raster::projectRaster(from = x, to = grast))

  if (length(elev_layers) > 1) {
    mosaic.r <- do.call(merge,elev_layers)
  } else{
    mosaic.r <- elev_layers[[1]]
  }

  unlink(c(gsub(".zip","",nts_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)

  mosaic.r <- raster::crop(mosaic.r,grast)

  if (aoi == "") { warning("You did not declare an area of interest, the elevation file will work for Wind Ninja also.")}

  writeRaster(mosaic.r,
              paste0(output_directory,"elevation.tif"),
              datatype = "INT2S",
              NAflag = -9999,
              format = "GTiff",
              overwrite = T)

  if (aoi != "") {
    writeRaster(mask(mosaic.r,aoi),
                paste0(output_directory,"elevation.tif"),
                datatype = "INT2S",
                NAflag = -9999,
                format = "GTiff",
                overwrite = T)
    writeRaster(mosaic.r,
                paste0(output_directory,"elevation_wn.tif"),
                datatype = "INT2S",
                NAflag = -9999,
                format = "GTiff",
                overwrite = T)
  }

}

