#' Fuel Grid Generator
#'
#' @param aoi Spatial data layer containing the area of interest to create a fuel grid for Burn-P3
#' @param aoi_buffer This is a buffer in meters to extend the AOI for use in clipping the gridded information.
#' @param lut A look-up table containing the fuel information in order to convert the polygonal and raster information into the final fuel grid layer. _*Mandatory columns are:*_ export value, descriptive_name, fuel_type
#' @param reference_grid Reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.Can be either the location of the raster or a raster object.
#' @param fuel_layers Character vector defining spatial (shapefile) and gridded (raster) for ingestion during use of the function. These layers will be stacked from top to bottom, spatial layers first, then raster layers.
#' @param fuel_col Character vector defining the column that contains fuel calls in each shapefile in the same order they are called in the \code{fuel_layers} object
#' @param desired_resolution The desired resolution of the final raster. _(Default = 100)_
#' @param pc A boolean vector of logical values (T/F) defining whether or not percent conifer is to be calculated for mixedwood fuels based on a percent conifer column.
#' @param pc_col A character vector defining the column for percent conifer in each spatial layer. If the corresponding \code{pc} value is false enter "".
#' @param output_directory The directory to place the final fuel grid. If using the generated directories use \code{bp3_base} as the output directory.
#'
#' @return
#' @export
#'
#' @examples

fuel_grid_generator <- function(aoi, aoi_buffer = 15000, lut, reference_grid, fuel_layers,fuel_col,desired_resolution = 100, pc = F,pc_col, output_directory){


  if( grepl("RasterLayer", class(reference_grid)) ){ grast <- reference_grid }
  if( grepl("character", class(reference_grid)) ){ grast <- raster(reference_grid) }
  if( !grepl("RasterLayer|character", class(reference_grid)) ){ message("Reference Grid must be the directory of the raster or a raster object.") }

  if(is.element("sf",class(aoi)) ){ aoi <- aoi }
  if( is.element("character",class(aoi)) ){ aoi <- st_read(aoi) }
  if( !is.element("sf", class(aoi)) & !is.element("character",class(aoi)) ){ message("Area of Interest must be the location of the shapefile or an sf object.") }

  aoi <- st_buffer(
    st_transform(
      aoi,
      crs(grast)
    ),
    dist = aoi_buffer
  )


  rsts <- fuel_layers[grep(".asc$|.tif$",fuel_layers)]

  rsts <- lapply(rsts,function(x) {

    print(x)

    rst.in <- rast(x)
    rst.in <- extend(rst.in,rast(vect(aoi)))

    rst.in <- project(rst.in,y=proj4string(crs(aoi)),method="ngb")
    rst.in <- crop(rst.in,rast(vect(aoi)),snap="in")

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
    print(paste0("Working on... ", x))
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
