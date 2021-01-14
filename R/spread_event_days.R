#' Spread Event Days
#'
#' Spread Event Day calculator based on continuous weather
#'
#' @details This is a calculator for spread event days when continuous fire data does not exist and/or fire information from MODIS cannot be used due to inconsistencies. The calculator will use the FWI threshold defined to define the number of consecutive days above that threshold and generate a probability distribution for use in Burn-P3.
#'
#' @param input Weather information file with fire weather index values, the year, and station ids at a minimum.
#' @param yr_col Character declaration of year column _(Default = "yr")_
#' @param id_col Character declaration of id column _(Default = "id")_
#' @param seasonal Declaration of the use of seasons in the weather data set. _(Default = F)_
#' @param season_col If seasonal is True, a season column must be declared.
#' @param season_names If seasonal is True, identify the season descriptions for use during output.
#' @param zonal Declaration of the use of weather zones. _(Default = F)_
#' @param zone_col If zonal is True, a zone column must be declared.
#' @param zone_names If zonal is True, identify the descriptive names of the zones for use during mapping and output.
#' @param threshold For weather based spread event day assessments a threshold is necessary to minimize an excessive tail. This ensures a reasonable distribution for short to mid duration fires but excludes long durations. _(Default = 80)_
#' @param min_fwi A minimum fire weather index is used to describe days where fires are more likely to spread and should be consecutively counted. 19 is common in the Canadian Boreal per Podur and Wotton, 2011 _(Default = 19)_
#' @param directory Directory for files to be output when using the Burn-P3 directory generator. _(Default = "")_
#'
#' @importFrom plyr ddply
#'
#' @return data.frame
#' @export
#'
#' @references [Defining fire spread event days for fire-growth modelling. 2011. Podur,J.; Wotton, M. International Journal of Wildland Fire. 20:497-507](cfs.nrcan.gc.ca/publications?id=32563)
#'
#' @examples
#'
#' ## Load example data
#' load("E:/Quarantine/R/BP3_RProject/burn-p3-r-package/data/weather.rda")
#'
#' spread_event_days(input = wx_input,
#' yr_col = "yr",
#' id_col = "id",
#' seasonal = F ,
#' season_col = "season",
#' season_names = c("Early Spring","Late Spring","Early Summer","Late Summer","Early Fall","Late Fall"),
#' zonal = F,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine","Montane","West Alpine","West Montane","West Interior Douglas Fir"),
#' threshold = 80,
#' min_fwi = 19,
#' directory = "")
#'
#' spread_event_days(input = wx_input,
#' yr_col = "yr",
#' id_col = "id",
#' seasonal = T ,
#' season_col = "season",
#' season_names = c("Early Spring","Late Spring","Early Summer","Late Summer","Early Fall","Late Fall"),
#' zonal = F,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine","Montane","West Alpine","West Montane","West Interior Douglas Fir"),
#' threshold = 80,
#' min_fwi = 19,
#' directory = "")
#'
#' spread_event_days(input = wx_input,
#' yr_col = "yr",
#' id_col = "id",
#' seasonal = F ,
#' season_col = "season",
#' season_names = c("Early Spring","Late Spring","Early Summer","Late Summer","Early Fall","Late Fall"),
#' zonal = T,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine","Montane","West Alpine","West Montane","West Interior Douglas Fir"),
#' threshold = 80,
#' min_fwi = 19,
#' directory = "")
#'
spread_event_days <- function(input,
                              yr_col,
                              id_col,
                              seasonal = F ,
                              season_col = "season",
                              season_names = "",
                              zonal = F,
                              zone_col = "wx_zone",
                              zone_names = "",
                              threshold = 80,
                              min_fwi = 19,
                              directory = ""){

  if (seasonal == T) {
    if (length(season_names) != length(unique(input[,season_col]))) {warning("There are not enough season names for the number of unique season in your data. The system will proceed and the remaining zones will be unnamed, names are assigned in order of occurrence and may have no meaning.")}
  }
  if (zonal == T) {
    if (length(zone_names) != length(unique(input[,zone_col]))) {warning("There are not enough zone names for the number of unique zones in your data. The system will proceed and the remaining zones will be unnamed, names are assigned in order of occurrence and may have no meaning.")}
  }


  sum_thresh_sed <- function(sed,thresh){
    for (i in 1:nrow(sed)) {
      sed[i,"sed_sum"] <- sum(sed[1:i,"sp_ev_days"])
    }
    ## Set a threshold for the cumulative probability  to cut off the SED distribution\
    if ( thresh == 100 ) {
      sed <- sed } else {
        sed <- sed[-which(sed$sed_sum >= thresh)[-1],]
      }

    ## Add the difference to achieve 100 percent to the 1 day spread.
    sed$sp_ev_days <- sed$sp_ev_days + (100 - sum(sed$sp_ev_days))/nrow(sed)

    sed$sed_sum <- NULL
    return(sed)
  }

  if (seasonal == T) {
    sed_wx <- ddply(input,c(season_col, yr_col,id_col),.fun = function(x) {
      over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
      runs <- rle(over_thresh)
      counts <- runs$lengths[runs$values == 1]
      gaps <- runs$lengths[runs$values == 0]
      data.frame(counts = counts)
    })
    sed <- list()
    for (i in unique(input[,season_col])) {
      x <- hist(sed_wx[sed_wx[,season_col] == i,"counts"],
                breaks = 0:(max(sed_wx$counts,
                                na.rm = T)),
                freq = T)
      sed[[i]] <- data.frame(days = x$breaks[-1] ,
                             sp_ev_days = round(x$density*100,2))
    }

    names(sed) <- season_names

    sed <- lapply(sed, function(x){sum_thresh_sed(x,threshold)})
    if (directory == "") {
      print(sed)
    } else{
      lapply(seq_along(sed),function(x){
        write.csv(x = sed[[x]],
                  paste0(directory,
                         "/Inputs/2. Modules/Distribution Tables/Seasonal_",
                         names(sed)[x],
                         "_SED_Seasonal.csv"),
                  row.names = F)
      })
  }

  }

  if (zonal == T) {
    sed_wx <- ddply(input,c(zone_col, yr_col,id_col),.fun = function(x) {
      over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
      runs <- rle(over_thresh)
      counts <- runs$lengths[runs$values == 1]
      gaps <- runs$lengths[runs$values == 0]
      data.frame(counts = counts)
    })
    sed <- list()
    for (i in seq_along(unique(sed_wx[,zone_col]))) {
      x <- hist(sed_wx[sed_wx[,zone_col] == i,"counts"],
                breaks = 0:(max(sed_wx$counts,
                                na.rm = T)),
                freq = T)
      sed[[i]] <- data.frame(days = x$breaks[-1] ,sp_ev_days = round(x$density*100,
                                                                     2))
    }
    names(sed) <- zone_names


    sed <- lapply(sed, function(x){sum_thresh_sed(x,threshold)})

    if (directory == "") {
      print(sed)
    } else{
      lapply(seq_along(sed),function(x){
        write.csv(x = sed[[x]],
                  paste0(directory,
                         "/Inputs/2. Modules/Distribution Tables/Seasonal_",
                         names(sed)[x],
                         "_SED_Seasonal.csv")
                  ,row.names = F)
      })
    }
  }

  if ( seasonal == F & zonal == F) {
    sed_wx <- ddply(.data = input,
                    .variables = c( yr_col,id_col),
                    .fun = function(x) {
                                       over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
                                       runs <- rle(over_thresh)
                                       counts <- runs$lengths[runs$values == 1]
                                       gaps <- runs$lengths[runs$values == 0]
                                       data.frame(counts = counts)
                                       }
                    )
  x <- hist(sed_wx[,"counts"],breaks = 0:(max(sed_wx$counts ,na.rm = T)),freq = T)
  sed <- data.frame(days = x$breaks[-1] ,sp_ev_days = round(x$density*100,2))

  sed <- sum_thresh_sed(sed,threshold)

  ## Write out the Spread Event Days
  if (directory == "") {
    print(sed)
  } else{
  write.csv(x = sed,
            paste0(directory,
                   "/Inputs/2. Modules/Distribution Tables/SED.csv"),
            row.names = F)}
}
}

