#' Area of Interest for Weather Stations - Designed for Parks Canada Data
#'
#'
#' @param area_of_interest_file Two element vector describing the location of the file (dsn) and the filename. The file is expected to follow a format readable by readOGR.
#' @param PC Parks Canada flag to define whether or not you want to download the Parks Canada data automatically. area_of_interest_file must be "" when using PC=T **and** define a / many park_of_interest. (*_Default_* = F)
#' @param reference_grid This is a reference raster to provide a projection.
#' @param buffer_width A width in meters to apply to the area of interest. If you are only interested in the area do not buffer. (*_Default_* = 0)
#' @param park_of_interest The english name of the park or parks of interest. Expects a vector of character strings. (*_Default_* = "")
#' @param stns Dataframe containing the stations within and around your area of interest.
#' @param stn_name_col A character string containing the column of the station names
#' @param stn_id_col A character string containing the column of the station ids
#'
#' @importFrom terra rast
#' @importFrom sf st_read st_transform st_intersection st_buffer
#' @importFrom geojsonsf geojson_sf
#' @import ggplot2
#'
#' @details Area of interest generator that allows a systematic AOI generation. Automated system for Parks Canada.
#'
#' @return SpatialPolygonsDataFrame
#'
#' @examples
#'
#'# Load test data
#'ref_grid <- rast(system.file("extdata/fuel.tif",package = "BurnP3.HelpR"))
#'weather_stations <- st_read(dsn=system.file("extdata/extdata.gpkg", package="BurnP3.HelpR"),layer="weather_stations")
#'
#'## Defined AOI
#'aoi(area_of_interest_file = st_read(system.file("extdata/extdata.gpkg", package="BurnP3.HelpR"),"aoi"),
#'    PC=F,
#'    reference_grid = ref_grid,
#'    buffer_width = 15000,
#'    park_of_interest="",
#'    stns = weather_stations,
#'    stn_name_col = "sttn_nm",
#'    stn_id_col = "statn_d")
#'
#'## Single Park
#'aoi(area_of_interest_file = "",
#'    PC=T,
#'    reference_grid = ref_grid,
#'    buffer_width = 15000,
#'    park_of_interest=c("Banff"),
#'    stns = weather_stations,
#'    stn_name_col = "sttn_nm",
#'    stn_id_col = "statn_d")
#'
#'##Multiple Parks
#'aoi(area_of_interest_file = "",
#'    PC=T,
#'    reference_grid = ref_grid,
#'    buffer_width = 15000,
#'    park_of_interest=c("Banff|Jasper"),
#'    stns = weather_stations,
#'    stn_name_col = "sttn_nm",
#'    stn_id_col = "statn_d")
#'
#' @export

aoi <- function(area_of_interest_file,
                PC = F,
                reference_grid,
                buffer_width = 0,
                park_of_interest = "",
                stns,
                stn_name_col,
                stn_id_col){

  if ( grepl("SpatRaster", class(reference_grid)) ) {grast <- reference_grid}
  if ( grepl("character", class(reference_grid)) ) {grast <- terra::rast(reference_grid)}
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) {message("Reference Grid must be the directory of the raster or a SpatRaster object.")}

  if ( is.null(area_of_interest_file) ) {stop("Please input an area of interest file or delcare PC (Parks Canada) as true to automatically load the Parks Canada layer")}

  if (PC == T & park_of_interest == "") {
    aoi_poly <- geojsonsf::geojson_sf("https://proxyinternet.nrcan.gc.ca/arcgis/rest/services/CLSS-SATC/CLSS_Administrative_Boundaries/MapServer/1/query?where=OBJECTID+%3E%3D0&geometryType=esriGeometryPolygon&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&outFields=*&returnGeometry=true&returnTrueCurves=true&returnIdsOnly=false&returnCountOnly=false&returnZ=false&returnM=false&returnDistinctValues=false&returnExtentOnly=false&featureEncoding=esriDefault&f=geojson")
    stop(paste0("You have declared that you are using Parks Canada data, however you have not defined a park or multiple parks. This will result in national data being used. Park/s of interest can be declared as a character vector.Park names are: ",aoi_poly$sf$adminAreaNameEng))}

  if ( is.null(area_of_interest_file) | park_of_interest != "") {

    aoi_poly <- geojsonsf::geojson_sf("https://proxyinternet.nrcan.gc.ca/arcgis/rest/services/CLSS-SATC/CLSS_Administrative_Boundaries/MapServer/1/query?where=OBJECTID+%3E%3D0&geometryType=esriGeometryPolygon&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&outFields=*&returnGeometry=true&returnTrueCurves=true&returnIdsOnly=false&returnCountOnly=false&returnZ=false&returnM=false&returnDistinctValues=false&returnExtentOnly=false&featureEncoding=esriDefault&f=geojson")

  }

  if (!is.null(area_of_interest_file) & area_of_interest_file != "") {
    if (length(area_of_interest_file) > 1) {
      aoi_poly <- sf::st_read(dsn = area_of_interest_file[1],
                              layer = area_of_interest_file[2])
    } else {
      aoi_poly <- sf::st_read(area_of_interest_file)
    }
  }


  if (park_of_interest != "") {
    aoi_poly <- aoi_poly[grep(park_of_interest,aoi_poly$adminAreaNameEng,ignore.case = T),]
  }

  aoi_poly <- sf::st_transform(x = aoi_poly ,crs = crs(grast))

  ## Some basic plotting to visualize where stations are in relation to the AOI

  stns_within_aoi <- sort(
    as.data.frame(
      sf::st_intersection(
        sf::st_transform(
          stns,
          crs = terra::crs(aoi_poly)
        ),
        sf::st_buffer(aoi_poly,
                      dist = buffer_width
        )
      )
    )[,stn_name_col])

  x11();

  p <- ggplot2::ggplot( sf::st_buffer(aoi_poly,
                                      dist = buffer_width)) +
    geom_sf(aes(fill = OBJECTID)) +
    geom_sf_label(data = sf::st_transform(stns[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,],
                                          crs = terra::crs(aoi_poly)),
                  aes(label = sttn_nm),
                  position = "identity") +
    ggtitle(label = paste0("There are ",length(stns_within_aoi)," staions inside your area of interest")) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))

  print(p)

  return(stns_within_aoi)
}

