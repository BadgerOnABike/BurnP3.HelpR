## Fuel Grid Generator

fuel_grid_generator <- function(aoi, lut, reference_grid, fuel_layers,fuel_col,desired_resolution = 100, pc = F,pc_col, output_directory){


  rsts <- fuel_layers[grep(".asc$|.tif$",fuel_layers)]

  rsts <- lapply(rsts,function(x) {

    rst.in <- raster(x)
    rst.in <- extend(rst.in,raster(aoi,res=desired_resolution,origin(reference_grid)))

    if(proj4string(rst.in) != proj4string(aoi)){

      rst.in <- projectRaster(rst.in,crs=CRS(proj4string(aoi)),method="ngb")
      rst.in <- crop(rst.in,raster(aoi,res=desired_resolution,origin(reference_grid)),snap="in")

    } else {

      rst.in <- crop(rst.in,raster(aoi,res=desired_resolution,origin(reference_grid)),snap="in")

    }

    if(res(rst.in)[1] != res(reference_grid)[1]){

      rst.in <- resample(rst.in,raster(aoi,res=desired_resolution,origin(reference_grid)),method = "ngb")

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

  shps <- lapply(shps, function(x){
    st_crop(st_transform(read_sf(x[1],x[2]),crs = CRS(proj4string(reference_grid))),reference_grid)
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

  fuel.r <- lapply(shps,function(x) fasterize(sf = x,raster = reference_grid,field = "Fuel"))

  fuel.r <- c(fuel.r, rsts)

  extents <- lapply(fuel.r,extent)
  extents <- ldply(extents,function(x){
    data.frame(xmin=x@xmin,xmax=x@xmax,ymin=x@ymin,ymax=x@ymax)
  })
  extents <- extent(c(min(extents$xmin),max(extents$xmax),min(extents$ymin),max(extents$ymax)))
  fuel.r <- lapply(fuel.r, function(x){
    extend(x,extents)
    crop(x,extents)
  }
  )

  mosaic.r <- do.call(merge,fuel.r)
  mosaic.r <- mask(mosaic.r,extend(rasterize(aoi,reference_grid),extents))

  writeRaster(mosaic.r,
              output_directory,
              datatype="INT2S",
              NAflag=-9999,
              overwrite=T)
}
