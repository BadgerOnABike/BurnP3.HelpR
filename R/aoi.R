#' Area of Interest for Weather Stations - Designed for Parks Canada Data
#'
#' @author Brett Moore, \email{Brett.Moore@@canada.ca}
#'
#' @param area_of_interest_file Two element vector describing the location of the file (dsn) and the filename. The file is expected to follow a format readable by readOGR.
#' @param PC=F Parks Canada flag to define whether or not you want to download the Parks Canada data automatically. Leave area_of_interest_file blank if using PC=T and define a/many park_of_interest.
#' @param reference_grid This is a reference raster to provide a projection.
#' @param buffer_width=0 A width in meters to apply to the area of interest. If you are only interested in the area do not buffer.
#' @param park_of_interest="" The english name of the park or parks of interest. Expects a vector of character strings.
#' @param stns Dataframe containing the stations within and around your area of interest.
#' @param stn_name_col A character string containing the column of the station names
#' @param stn_id_col A character string containing the column of the station ids
#'
#' @details Area of interest generator that allows a systematic AOI generation. Automated system for Parks Canada.
#'
#' @return SpatialPolygonsDataFrame
#'
#' @examples
#'
#'# Load test data
#'ref_grid <- raster(system.file("extdata/fuel.tif",package = "bp3inputs"))
#'weather_stations <- readOGR(system.file("extdata/Weather_Station_List.shp", package="bp3inputs"))
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
#' @import rgdal
#' @import raster
#' @export



aoi <- function(area_of_interest_file,PC=F, reference_grid, buffer_width = 0, park_of_interest="",stns, stn_name_col,stn_id_col){

  if(class(reference_grid) != "character" | class(reference_grid) != "raster"){stop("reference_grid must be either a string to the location of the reference raster or a raster layer.")}
  if(class(reference_grid) =="character"){
    grast <- raster(reference_grid)
  }
  if(class(reference_grid) =="raster"){
    grast <- reference_grid
  }

  if(area_of_interest_file == "" & PC == F){stop("Please input an area of interest file or delcare PC (Parks Canada) as true to automatically load the Parks Canada layer")}

  if(PC == T & park_of_interest == ""){warning("You have declared that you are using Parks Canada data, however you have not defined a park or multiple parks. This will result in national data being used. Park/s of interest can be declared as a character vector.")}

  if(area_of_interest_file == "" & PC==T){
    pc_temp <- tempfile(fileext = '.zip')
    download.file(destfile = pc_temp,url = "http://ftp.maps.canada.ca/pub/pc_pc/National-parks_Parc-national/national_parks_boundaries/national_parks_boundaries.shp.zip")
    pc_files <- unzip(pc_temp)
    unzip(zipfile = pc_temp,files = gsub("./","",pc_files),exdir = gsub(".zip","",pc_temp))
    aoi_poly <- readOGR(dsn = gsub(".zip","",pc_temp),layer = "national_parks_boundaries")

    unlink(c(gsub(".zip","",pc_temp),list.files(tempdir(),pattern = ".zip",full.names = T)),recursive = T)
  }

  if(PC == T & park_of_interest == ""){stop(paste0("You have declared that you are using Parks Canada data, however you have not defined a park or multiple parks. This will result in national data being used. Park/s of interest can be declared as a character vector.Park names are: ",aoi_poly$parkname_e))}

  if(area_of_interest_file != ""){
    aoi_poly <- readOGR(dsn = area_of_interest_file[1],layer=area_of_interest_file[2])
  }


  if(park_of_interest != ""){
    aoi_poly <- aoi_poly[grep(park_of_interest,aoi_poly$parkname_e,ignore.case = T),]
  }

  aoi_poly <- spTransform(x = aoi_poly ,CRSobj = CRS(proj4string(grast)))

  ## Some basic plotting to visualize where stations are in relation to the AOI

  stns_within_aoi <- sort(as.data.frame(crop(spTransform(stns,CRSobj = CRS(proj4string(aoi_poly))),gBuffer(aoi_poly,width=buffer_width)))[,stn_name_col])

  x11()
  plot(gBuffer(aoi_poly,width=buffer_width),main=paste0("There are ",length(stns_within_aoi)," staions inside your area of interest"))
  plot(spTransform(stns[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,],CRSobj = CRS(proj4string(aoi_poly))),add=T,pch=1)
  text(spTransform(stns[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,],CRSobj = CRS(proj4string(grast))),data.frame(stns)[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,][,stn_id_col])
  plot(spTransform(stns[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,],CRSobj = CRS(proj4string(aoi_poly))),add=T,pch=1)

  return(stns_within_aoi)
}
