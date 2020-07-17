## Fuel Grid Generator

fuel_grid_generator <- function(aoi, aoi_buffer = 15000, lut, reference_grid, fuel_layers,fuel_col,desired_resolution = 100, pc = F,pc_col, output_directory){


  if( grepl("RasterLayer", class(reference_grid)) ){ grast <- reference_grid }
  if( grepl("character", class(reference_grid)) ){ grast <- raster(reference_grid) }
  if( !grepl("RasterLayer|character", class(reference_grid)) ){ message("Reference Grid must be the directory of the raster or a raster object.") }

  if( grepl("sf", class(aoi)) ){ aoi <- aoi }
  if( grepl("character", class(aoi)) ){ aoi <- st_read(aoi) }
  if( !grepl("sf|character", class(reference_grid)) ){ message("Reference Grid must be the location of the shapefile or an sf object.") }

  aoi_poly <- st_buffer(
    st_transform(
      aoi_poly[grep("Jasper",aoi_poly$parkname_e,ignore.case = T),],
      crs(grast)
    ),
    dist = aoi_buffer
  )


  rsts <- fuel_layers[grep(".asc$|.tif$",fuel_layers)]

  rsts <- lapply(rsts,function(x) {

    rst.in <- rast(x)
    rst.in <- extend(rst.in,rast(vect(aoi)))

    if(crs(rst.in) != crs(grast)){

      rst.in <- project(rst.in,crs=crs(aoi),method="ngb")
      rst.in <- crop(rst.in,rast(vect(aoi)),snap="in")

    } else {

      rst.in <- crop(rst.in,rast(vect(aoi)),snap="in")

    }

    if(res(rst.in)[1] != res(grast)[1]){

      rst.in <- resample(rst.in,grast,method = "ngb")

    } else {

      rst.in

    }

  }
  )

  shps <- fuel_layers[grep(".shp$",fuel_layers)]

  shps <- lapply(shps,function(x) {
    y <- gregexpr("/",x)[[1]]
    c( substr(x,1,y[length(y)] - 1), substr(x,y[length(y)] + 1,nchar(x) - 4))
  }
  )

  print("Assessing Shapes")

  shps <- lapply(shps, function(x){
    print(paste0("Working on...", x))
    st_crop(
        st_make_valid(
          st_transform(
            read_sf(x[1],
                    x[2]),
            crs = crs(grast)
            )
          ),
      xmin = bbox(grast)[1,1],
      ymin = bbox(grast)[2,1],
      xmax = bbox(grast)[1,2],
      ymax = bbox(grast)[2,2])
  })

  for(i in seq_along(shps)){
    if( pc[i]){
      fuel_rep <- data.frame(shps[[i]])
      pc.dat <- fuel_rep[grep("M",fuel_rep[,fuel_col[i]]),pc_col[i]]
      pc.dat <- round(pc.dat, -1)
      pc.dat <- ifelse(pc.dat == 0, 15, ifelse(pc.dat == 100, 90, pc.dat))
      fuel_rep[grep("M",fuel_rep[,fuel_col[i]]),fuel_col[i]] <- paste0("M-1/M-2 (", pc.dat ," PC)")
      shps[[i]][,fuel_col[i]] <- fuel_rep[,fuel_col[i]]
      shps[[i]]$Fuel <- lut[match(gsub("-","",tolower(data.frame(shps[[i]])[,fuel_col[i]])),gsub("-","",tolower(lut$fuel_type))),"export_value"]
    } else {
      shps[[i]]$Fuel <- lut[match(gsub("-","",tolower(data.frame(shps[[i]])[,fuel_col[i]])),gsub("-","",tolower(lut$fuel_type))),"export_value"]
    }
  }

  fuel.r <- lapply(shps,function(x) {
    if(length(unique(st_geometry_type(x))) > 1){x <- st_cast(x,"MULTIPOLYGON")}
    fasterize(sf = x,raster = raster(grast),field = "Fuel")
  })

  fuel.r <- c(fuel.r,lapply(rsts,function(x) raster(x)))

  extents <- lapply(fuel.r,function(x)extent(x))
  extents <- ldply(extents,function(x){
    data.frame(xmin=x@xmin,xmax=x@xmax,ymin=x@ymin,ymax=x@ymax)
  })
  extents <- extent(c(min(extents$xmin),max(extents$xmax),min(extents$ymin),max(extents$ymax)))
  fuel.r <- lapply(fuel.r, function(x){
    x <- extend(x,extents)
    x <- crop(x,extents)
  }
  )

  mosaic.r <- do.call(merge,fuel.r)

  cropper <- fasterize(aoi,raster(grast))
  cropper <- crop(cropper,aoi)

  mosaic.r <- crop(mosaic.r,cropper)
  mosaic.r <- mask(mosaic.r,cropper)

  writeRaster(mosaic.r,
              output_directory,
              datatype="INT2S",
              NAflag=-9999,
              overwrite=T)
}
