#' Fire Rate Distribution
#'
#' @param input Location of a binary file containing fire data, requires a date column
#' @param date_col
#' @param date_format
#' @param aoi A spatial object do be used for its CRS and extent. Data will be cropped to this aoi.
#' @param output_location Folder location for fire rate distribution to be exported to. NOTE: Only requries the base directory, assumes the directory generator was used.
#' @param seasonal Declaration of the use of seasons in the weather data set. _(Default = F)_
#' @param seasons If seasonal is True, a two column data.frame that contains the seasons numerical identifier and the description.
#' @param zonal Declaration of the use of weather zones. _(Default = F)_
#' @param zones A raster containing the fire zones to be used.
#' @param zone_names If zonal is True, identify the descriptive names of the zones for use during mapping and output.
#'
#' @return
#' @export
#'
#' @examples
fire_rate_distribution <- function(input, date_col, date_format = "%Y/%m/%d", aoi, output_location, seasonal=F, zonal=F, seasons = season_df, zones, zone_names = c("Alpine-E","Montane-E","Alpine-W","Montane-W","IDF")){

  x <- load(input,verbose=T) # Ignition points from the NFDB clipped to 100km from the NPs

  ## In the event the binary loaded is not called nfdb we coerce it
  nfdb <- get(x)
  ## Remove the non-nfdb named object
  if(x != "nfdb"){rm(list=c(x))}

  nfdb <- nfdb[-which(is.na(nfdb$date_col)),]
  nfdb <- nfdb[-which(duplicated(paste(nfdb$LATITUDE,nfdb$LONGITUDE,nfdb$YEAR))),]
  # subset of the 100km_nfdb to 3 ha minimum fire size

  nfdb <- subset(nfdb, nfdb@data$SIZE_HA >= 0.01)
  nfdb$jday <- as.numeric(format(as.Date(nfdb$date_col,date_format),"%j"))

  nfdb$season <- ifelse(as.numeric((nfdb$jday)) < season_df$jday[1], 1, NA)
  for(i in season_df$season[1:(length(season_df$season)-1)]){
    nfdb$season <- ifelse(as.numeric(nfdb$jday) >= season_df$jday[i] & as.numeric(nfdb$jday) < season_df$jday[i+1], season_df$season[i+1], nfdb$season)
  }
  nfdb$season <- ifelse(as.numeric((nfdb$jday)) >= season_df$jday[nrow(season_df)], max(season_df$season)+1, nfdb$season)

  nfdb <- nfdb[-which(nfdb$season >= max(season_df$season)+1),]
  ## May throw an error about bad geometry, that's find it still projects.
  nfdb <- spTransform(nfdb,CRSobj = CRS(proj4string(aoi)))
  nfdb <- crop(nfdb, aoi)
  nfdb <- nfdb[which(nfdb$CAUSE %in% c("H","L") & nfdb$season %in% season_df$season),]
  nfdb$zone <- extract(zones,nfdb)
  nfdb <- nfdb[-which(is.na(nfdb$zone)),]
  x <- hist(nfdb$zone,breaks=seq(min(nfdb$zone)-0.5,max(nfdb$zone)+0.5,1))
  barplot(height = x$counts,names=zone_names)

  vars <- c("CAUSE", if(seasonal){"season"},if(zonal){"zone"})

  fire_rate <- ddply(.data = as.data.frame(nfdb),
                     .variables = .(CAUSE, season, zone),
                     .fun=function(x){
                       counts <- nrow(x)
                       pct <- counts/nrow(nfdb)
                       data.frame(esc_fires=round(pct*100,2))
                     }
  )

  colnames(fire_rate) <- tolower(colnames(fire_rate))
  fire_rate$cause <- as.numeric(fire_rate$cause)
  write.csv(fire_rate, paste0(output_location,"Inputs/2. Modules/Distribution Tables/Fire_Rate_Distribution.csv"),row.names=F)}
