#' Fuel Grid Generator
#'
#' Generate a fuel grid from spatial and raster fuel layers
#'
#' @param aoi Spatial data layer containing the area of interest to create a fuel grid for Burn-P3
#' @param lut A look-up table containing the fuel information in order to convert the polygonal and raster information into the final fuel grid layer
#' @param reference_grid Reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.Can be either the location of the raster or a raster object.
#' @param fuel_layers Character vector defining spatial (shapefile) and gridded (raster) for ingestion during use of the function. These layers will be stacked from top to bottom, spatial layers first, then raster layers.
#' @param fuel_col Character vector defining the column that contains fuel calls in each shapefile in the same order they are called in the \code{fuel_layers} object
#' @param desired_resolution The desired resolution of the final raster. _(Default = 100)_
#' @param pc Character vector of logical values (T/F) defining whether or not percent conifer is to be calculated for mixedwood fuels based on a percent conifer column.
#' @param pc_col A character vector defining the column for percent conifer in each spatial layer. If the corresponding \code{pc} value is false enter "".
#' @param output_directory The directory to place the final fuel grid. If using the generated directories use \code{bp3_base} as the output directory.
#'
#' @return RasterLayer
#' @export
#'
#' @examples
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
