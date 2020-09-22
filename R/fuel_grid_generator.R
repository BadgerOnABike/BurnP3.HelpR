#' Fuel Grid Generator
#'
#' @param aoi_poly Spatial data layer or character string to the location of the spatial data containing the area of interest to create a fuel grid for Burn-P3
#' @param aoi_buffer This is a buffer in meters to extend the aoi_poly for use in clipping the gridded information.
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

fuel_grid_generator <- function(aoi_poly, aoi_buffer = 15000, lut, reference_grid, fuel_layers,fuel_col,desired_resolution = 100, pc = F, pc_col, output_directory){


  if ( grepl("SpatRaster", class(reference_grid)) ) { grast <- reference_grid }
  if ( grepl("character", class(reference_grid)) ) { grast <- rast(reference_grid) }
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) { message("Reference Grid must be the directory of the raster or a SpatRaster object from the terra package.") }

  if (is.element("sf",class(aoi_poly)) ) { aoi_poly <- aoi_poly }
  if ( is.element("character",class(aoi_poly)) ) { aoi_poly <- st_read(aoi_poly) }
  if ( !is.element("sf", class(aoi_poly)) & !is.element("character",class(aoi_poly)) ) { message("Area of Interest must be the location of the shapefile or an sf object.") }

  aoi_poly <- st_buffer(
    st_transform(
      aoi_poly,
      crs(grast)
    ),
    dist = aoi_buffer
  )

  fuel.r <- lapply(fuel_layers, function(x) {

    i <- which(fuel_layers == x)

    if (grepl(".asc$|.tif$",x)) {


    print(paste0("Working on Raster... ",x))

      rst.in <- rast(x)
      rst.in <- expand(rst.in,ext(vect(aoi_poly)))

      rst.in <- project(rst.in,y = proj4string(crs(aoi_poly)),method = "ngb")
      rst.in <- crop(rst.in,rast(vect(aoi_poly)),snap = "in")

      if ( res(rst.in)[1] != res(grast)[1] ) {

        rst.in <- raster(resample(rst.in,grast,method = "ngb"))

      } else {

        rst.in <- raster(rst.in)

      }

      return(rst.in)

    }

    if (grepl(".shp",x)) {

      print(paste0("Working on Shapefile... ", x))
      shps <- st_crop(
                st_make_valid(
                  st_transform(
                    read_sf(x),
                    crs = crs(grast)
                    )
                  ),
                xmin = bbox(grast)[1,1],
                ymin = bbox(grast)[2,1],
                xmax = bbox(grast)[1,2],
                ymax = bbox(grast)[2,2]
                )
      shps <- st_cast(shps, "MULTIPOLYGON")

      if ( pc[i]) {
        fuel_rep <- data.frame(shps)
        pc.dat <- fuel_rep[grep("M",fuel_rep[,fuel_col[i]]),pc_col[i]]
        pc.dat <- round(pc.dat, -1)
        pc.dat <- ifelse(pc.dat == 0, 15, ifelse(pc.dat == 100, 90, pc.dat))
        fuel_rep[grep("M",fuel_rep[,fuel_col[i]]),fuel_col[i]] <- paste0("M-1/M-2 (", pc.dat ," PC)")
        shps[,fuel_col[i]] <- fuel_rep[,fuel_col[i]]
      }
       shps$Fuel <- lut[match(gsub("-","",tolower(data.frame(shps)[,fuel_col[i]])),gsub("-","",tolower(lut$fuel_type))),"export_value"]
       if (length(unique(st_geometry_type(shps))) > 1) {x <- st_cast(shps,"MULTIPOLYGON")}
       rst.in <- fasterize(sf = shps,raster = raster(grast),field = "Fuel")
       return(rst.in)
    }

      })

  extents <- lapply(fuel.r,function(x)extent(x))
  extents <- ldply(extents,function(x){
    data.frame(xmin = x@xmin,xmax = x@xmax,ymin = x@ymin,ymax = x@ymax)
  })
  extents <- extent(c(min(extents$xmin),max(extents$xmax),min(extents$ymin),max(extents$ymax)))
  fuel.r <- lapply(fuel.r, function(x){
    x <- extend(x,extents)
    x <- crop(x,extents)
  }
  )

  mosaic.r <- do.call(merge,fuel.r)

  cropper <- fasterize(aoi_poly,raster(grast))
  cropper <- crop(cropper,aoi_poly)

  mosaic.r <- crop(mosaic.r,cropper)
  mosaic.r <- mask(mosaic.r,cropper)

  writeRaster(mosaic.r,
              output_directory,
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = T)
}
