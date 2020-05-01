# Elevation Grabber - From NTS Grids --------------------------------------

elev_grab <- function(reference_grid,output_name,wind_ninja_name){
  
  grast <- raster(reference_grid)
  
  e <- as(extent(grast),"SpatialPolygons")
  proj4string(e) <- proj4string(grast)
  
  ## Download and extract the NTS grid specified by user (will be removed after use), uses the 250k grid as that is what CDEM is based on
  nts_temp <- tempfile(fileext = '.zip')
  download.file(destfile = nts_temp,url = "http://ftp.maps.canada.ca/pub/nrcan_rncan/vector/index/nts_snrc.zip")
  nts_files <- unzip(nts_temp)
  unzip(zipfile = nts_temp,files = gsub("./","",nts_files),exdir = gsub(".zip","",nts_temp))
  nts_grid <- readOGR(dsn = gsub(".zip","",nts_temp),layer = "nts_snrc_250k")
  
  ## Determine the NTS grids the data exists across
  layers <- crop(nts_grid,spTransform(e,CRSobj = CRS(proj4string(nts_grid))))$NTS_SNRC
  
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
  
  elev_layers <- lapply(elevation,function(x) projectRaster(from = x, to = grast))
  
  mosaic.r <- do.call(merge,elev_layers)
  
  unlink(c(gsub(".zip","",nts_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)
  wn_elev <- crop(mosaic.r,e)
  mosaic.r <- mask(mosaic.r,grast)
  
  writeRaster(wn_elev,wind_ninja_name,datatype = "INT2S", NAflag = -9999,format="GTiff",overwrite=T)
  writeRaster(mosaic.r,output_name,datatype = "INT2S", NAflag = -9999,format="GTiff",overwrite=T)
  
}

