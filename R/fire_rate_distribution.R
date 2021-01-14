#' Fire Rate Distribution
#'
#' @param input Location of a binary file containing fire data, requires a date column
#' @param date_col Character string defining the name of the column containing date information.
#' @param date_format Format of the dates within the date column that will enable the function to transition the date to a julian day. _(Default = "%Y/%m/%d")_
#' @param aoi A spatial object do be used for its CRS and extent. Data will be cropped to this aoi.
#' @param output_location Folder location for fire rate distribution to be exported to. NOTE: Only requires the base directory, assumes the directory generator was used.
#' @param seasonal Declaration of the use of seasons in the weather data set. _(Default = F)_
#' @param seasons If seasonal is True, a two column data.frame that contains the seasons numerical identifier and the description. _(Default = "")_
#' @param zonal Declaration of the use of weather zones. _(Default = F)_
#' @param zones A raster containing the fire zones to be used.
#' @param zone_names If zonal is True, identify the descriptive names of the zones for use during mapping and output. _(Default = "")_
#' @param min_fire_size A minimum fire size to subset the fire information to for adjuste fire rate distribution depending on the question being asked. _(Default = 0.01)_
#' @param causes A character vector defining the causes within the fire dataset. _(Default = c("H","L"))_
#' 
#' @importFrom rgdal spTransform
#' @importFrom sp crs
#' @importFrom raster crop extract
#' 
#' @return
#' 
#' @export
#'
#' @seealso \link[BurnP3.HelpR]{bp3_dir_gen}
#'
#' @examples
#' ## Load relavent data
#' data("fire_data")
#' aoi <- readOGR(system.file("extdata","extdata.gpkg",package="BurnP3.HelpR"),"aoi")
#' output_location <- paste0(tempdir(),"\\")
#' zones <- raster(system.file("extdata","zones.tif",package="BurnP3.HelpR"))
#' data("seasons")
#' zone_names = c("Alpine-E","Montane-E","Alpine-W","Montane-W","IDF")
#'
#' fr <- fire_rate_distribution(input = fire_data,
#'                              date_format = "%Y/%m/%d",
#'                              aoi = aoi,
#'                              output_location = output_location,
#'                              date_col = "REP_DATE",
#'                              seasonal = F,
#'                              zonal = F,
#'                              seasons = "",
#'                              zones,
#'                              zone_names = "",
#'                              min_fire_size = 0.01,
#'                              causes = c("H","L")
#'                              )
#' print(fr)
#'
#' fr <- fire_rate_distribution(input = fire_data,
#'                              date_format = "%Y/%m/%d",
#'                              aoi = aoi,
#'                              output_location = output_location,
#'                              date_col = "REP_DATE",
#'                              seasonal = T,
#'                              zonal = F,
#'                              seasons = seasons,
#'                              zones,
#'                              zone_names = "",
#'                              min_fire_size = 0.01,
#'                              causes = c("H","L")
#'                              )
#' print(fr)
#'
#' fr <- fire_rate_distribution(input = fire_data,
#'                              date_format = "%Y/%m/%d",
#'                              aoi = aoi,
#'                              output_location = output_location,
#'                              date_col = "REP_DATE",
#'                              seasonal = F,
#'                              zonal = T,
#'                              seasons = "",
#'                              zones = zones,
#'                              zone_names = zone_names,
#'                              min_fire_size = 0.01,
#'                              causes = c("H","L")
#'                              )
#'
#' print(fr)
#'
#' unlink(output_location, recursive = T)
#'

fire_rate_distribution <- function(input, date_col, date_format = "%Y/%m/%d", aoi, output_location, seasonal=F, zonal=F, seasons = "", zones, zone_names = "", min_fire_size = 0.01, causes = c("H","L")){

  if (length(which(is.na(input@data[,date_col]))) > 0) {input <- input[-which(is.na(input@data[,date_col])),]}
  if (length(which(duplicated(paste(input$LATITUDE,input$LONGITUDE,input$YEAR)))) > 0) {input <- input[-which(duplicated(paste(input$LATITUDE,input$LONGITUDE,input$YEAR))),]}
  # subset of the 100km_input to 3 ha minimum fire size

  input <- subset(input, input@data$SIZE_HA >= min_fire_size)
  input$jday <- as.numeric(format(as.Date(input@data[,date_col],date_format),"%j"))

  ## May throw an error about bad geometry, that's find it still projects.
  input <- spTransform(input,CRSobj = st_crs(aoi))
  input <- crop(input, aoi)
  input <- input[input$CAUSE %in% causes,]

  if (seasonal & !is.element("season",names(input))) {

    input$season <- NA

    for (i in seasons$season[1:(length(seasons$season) - 1)]) {
      input$season <- ifelse(as.numeric(input$jday) >= seasons$jday[i] & as.numeric(input$jday) < seasons$jday[i + 1], seasons$season[i], input$season)
    }

    if (length(unique(is.na(input$season))) > 1)
    {input <- input[-which(is.na(input$season)),]}

    input <- input[which(input$season %in% seasons$season),]
  }

  if (zonal) {
    input$zone <- extract(zones,input)
    if (length(which(is.na(input$zone))) > 0) {input <- input[-which(is.na(input$zone)),]}
  }

  vars <- c("CAUSE", if (seasonal) {"season"},if (zonal) {"zone"})

  fire_rate <- ddply(.data = as.data.frame(input),
                     .variables = c(vars),
                     .fun = function(x){
                       counts <- nrow(x)
                       pct <- counts/nrow(input)
                       data.frame(esc_fires = round(pct*100,2))
                     }
  )

  colnames(fire_rate) <- tolower(colnames(fire_rate))
  fire_rate$cause <- as.numeric(as.factor(fire_rate$cause))
  write.csv(fire_rate, paste0(output_location,"Fire_Rate_Distribution.csv"),row.names = F)
  if (output_location == bp3_base) {
    write.csv(fire_rate,
              paste0(output_location,"Inputs/2. Modules/Distribution Tables/Fire_Rate_Distribution.csv"),
              row.names = F)}
  return(fire_rate)
}
