#' Fuel Grid Generator
#'
#' @param aoi_poly Spatial data layer or character string to the location of the spatial data containing the area of interest to create a fuel grid for Burn-P3
#' @param aoi_buffer This is a buffer in meters to extend the aoi_poly for use in clipping the gridded information.
#' @param lut A look-up table containing the fuel information in order to convert the polygonal and raster information into the final fuel grid layer. _*Mandatory columns are:*_ export_value, descriptive_name, fuel_type
#' @param reference_grid Reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.Can be either the location of the raster or a raster object.
#' @param fuel_layers Character vector defining spatial (shapefile) and gridded (raster) for ingestion during use of the function. These layers will be stacked from top to bottom, spatial layers first, then raster layers.
#' @param fuel_col Character vector defining the column that contains character fuel types in each shapefile in the same order they are called in the \code{fuel_layers} object
#' @param desired_resolution The desired resolution of the final raster. _(Default = 100)_
#' @param pc A boolean vector of logical values (T/F) defining whether or not percent conifer is to be calculated for mixedwood fuels based on a percent conifer column.
#' @param pc_col A character vector defining the column for percent conifer in each spatial layer. If the corresponding \code{pc} value is false enter "".
#' @param output_directory The directory to place the final fuel grid. If using the generated directories use \code{bp3_base} as the output directory.
#'
#' @importFrom terra rast ext vect project extend crs bbox
#' @importFrom sf st_read st_buffer st_transform read_sf st_crop st_make_valid st_cast st_geometry_type
#' @importFrom raster raster crop mask writeRaster resample extend
#' @importFrom fasterize fasterize
#' @importFrom sp proj4string
#' @importFrom plyr ldply
#'
#' @return
#' @export
#'
#'
#' @examples
#'
#' ## Load in requisite data
#' fuel_shape_1 <- st_read( dsn = system.file("extdata/extdata.gpkg", package = "BurnP3.HelpR"),
#'                          layer = "Shape_Fuels_1")
#' fuel_shape_2 <- st_read( dsn = system.file("extdata/extdata.gpkg", package = "BurnP3.HelpR"),
#'                          layer = "Shape_Fuels_2")
#' fuel_shape_PC <- st_read( dsn = system.file("extdata/extdata.gpkg", package = "BurnP3.HelpR"),
#'                          layer = "Shape_Fuels_PC")
#' fuel_raster <- raster(system.file("extdata/fuel.tif", package = "BurnP3.HelpR"))
#' reference_grid <- fuel_raster
#'
#' aoi_poly <- st_read(  dsn = system.file("extdata/extdata.gpkg", package = "BurnP3.HelpR"),
#'                          layer = "aoi_poly")
#' data("lut")
#'
#'fuel_grid_generator(aoi_poly = aoi_poly,
#'                    aoi_buffer = 0,
#'                    lut = lut,
#'                    reference_grid = reference_grid,
#'                    fuel_layers = list(fuel_shape_1,
#'                                    fuel_shape_2,
#'                                    fuel_raster,
#'                                    fuel_shape_PC),
#'                    fuel_col = c("Fuel_type",
#'                                 "Fuels",
#'                                 NA,
#'                                 "FBP"),
#'                    pc = c(F,F,F,T),
#'                    pc_col = c(NA,NA,NA,"PC"),
#'                    output_directory = paste0(tempdir(),"\\")
#'                    )
#'  print(paste0("Fuel Grid has been output to ",output_directory))


fuel_grid_generator <- function(aoi_poly, aoi_buffer = 15000, lut, reference_grid, fuel_layers,fuel_col,desired_resolution = 100, pc = F, pc_col, output_directory){


  if ( grepl("SpatRaster", class(reference_grid)) ) { grast <- reference_grid }
  if ( grepl("character", class(reference_grid)) ) { grast <- terra::rast(reference_grid) }
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) { message("Reference Grid must be the directory of the raster or a SpatRaster object from the terra package.") }

  if (is.element("sf",class(aoi_poly)) ) { aoi_poly <- aoi_poly }
  if ( is.element("character",class(aoi_poly)) ) { aoi_poly <- sf::st_read(aoi_poly) }
  if ( !is.element("sf", class(aoi_poly)) & !is.element("character",class(aoi_poly)) ) { message("Area of Interest must be the location of the shapefile or an sf object.") }

  aoi_poly <- sf::st_buffer(
    sf::st_transform(
      aoi_poly,
      terra::crs(grast)
    ),
    dist = aoi_buffer
  )

  aoi_poly$name <- "thepoly"

  names(fuel_layers) <- 1:length(fuel_layers)

  fuel.r <- lapply(seq_along(fuel_layers), function(x) {

    i <- x
    x <- fuel_layers[[x]]

    if (grepl(".asc$|.tif$",x) || grepl("SpatRaster", class(x))) {


    print(paste0("Working on Raster... ",i))

      if (class(x) == "character") {rst.in <- terra::rast(x)} else {rst.in <- x}

      rst.in <- terra::extend(rst.in,terra::ext(terra::vect(aoi_poly)))

      rst.in <- terra::project(rst.in,y = sp::proj4string(terra::crs(aoi_poly)),method = "ngb")
      rst.in <- terra::crop(rst.in,terra::rast(terra::vect(aoi_poly)),snap = "in")

      if ( res(rst.in)[1] != res(grast)[1] ) {

        rst.in <- raster::raster(raster::resample(rst.in,grast,method = "ngb"))

      } else {

        rst.in <- raster::raster(rst.in)

      }

      return(rst.in)

    }

    if (grepl(".shp",x) || is.element("sf",class(x))) {

      print(paste0("Working on Shapefile... ", i))

      if (class(x) == "character") {x <- sf::read_sf(x)}

      shps <- sf::st_crop(
                sf::st_make_valid(
                  sf::st_transform(
                    x,
                    crs = terra::crs(grast)
                    )
                  ),
                xmin = terra::bbox(grast)[1,1],
                ymin = terra::bbox(grast)[2,1],
                xmax = terra::bbox(grast)[1,2],
                ymax = terra::bbox(grast)[2,2]
                )
      shps <- sf::st_cast(shps, "MULTIPOLYGON")

      if ( pc[i]) {
        fuel_rep <- data.frame(shps)
        pc.dat <- fuel_rep[grep("M",fuel_rep[,fuel_col[i]]),pc_col[i]]
        pc.dat <- round(pc.dat, -1)
        pc.dat <- ifelse(pc.dat == 0, 15, ifelse(pc.dat == 100, 90, pc.dat))
        fuel_rep[grep("M",fuel_rep[,fuel_col[i]]),fuel_col[i]] <- paste0("M-1/M-2 (", pc.dat ," PC)")
        shps[,fuel_col[i]] <- fuel_rep[,fuel_col[i]]
      }
       shps$Fuel <- lut[match(gsub("-","",tolower(data.frame(shps)[,fuel_col[i]])),gsub("-","",tolower(lut$fuel_type))),"export_value"]
       if (length(unique(sf::st_geometry_type(shps))) > 1) {x <- sf::st_cast(shps,"MULTIPOLYGON")}
       rst.in <- fasterize::fasterize(sf = shps,raster = raster::raster(grast),field = "Fuel")
       return(rst.in)
    }

      })

  extents <- lapply(fuel.r,function(x) raster::extent(x))
  extents <- plyr::ldply(extents,function(x){
    data.frame(xmin = x@xmin,xmax = x@xmax,ymin = x@ymin,ymax = x@ymax)
  })
  extents <- raster::extent(c(min(extents$xmin),max(extents$xmax),min(extents$ymin),max(extents$ymax)))
  fuel.r <- lapply(fuel.r, function(x){
    x <- raster::extend(x,extents)
    x <- raster::crop(x,extents)
  }
  )

  mosaic.r <- do.call(merge,fuel.r)

  cropper <- fasterize::fasterize(aoi_poly,raster::raster(grast))
  cropper <- raster::crop(cropper,aoi_poly)

  mosaic.r <- raster::crop(mosaic.r,cropper)
  mosaic.r <- raster::mask(mosaic.r,cropper)

  raster::writeRaster(mosaic.r,
              output_directory,
              datatype = "INT2S",
              NAflag = -9999,
              overwrite = T)
}
