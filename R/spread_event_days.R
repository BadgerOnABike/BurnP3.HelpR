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
#' @param threshold For weather based spread event day assessments a threshold is necessary to minimize an excessive tail. This ensures a reasonable distribution for short to mid duration fires but excludes long durations. The threshold is defined by the cumulative spread event day potential. As the cumulative sum is assessed the first time it crosses the threshold is the cut point. For example, if the cumulative sum for a spread event day distribution is: .65, .75, .79, .825, .815 (etc, to 1.0) with a threshold of .8, .825 would be the first time that value was cross. As such the first 4 spread event days would be maintained and the remaining .175 will be redistributed to achieve a value of 1.0 across days 1:4. This redistribution will be proportional to the spread event potential by day. That yields a small remainder that is then evenly distributed across the spread event days. This ensure we minimize impacts to the density across spread event days._(Default = 80)_
#' @param min_fwi A minimum fire weather index is used to describe days where fires are more likely to spread and should be consecutively counted. 19 is common in the Canadian Boreal per Podur and Wotton, 2011 _(Default = 19)_
#' @param min_dmc A minimum duff moisture code is used to describe the bottom of fire spread potential in most cases. This is used in conjunction with the minimum FWI to define spread event days. It will generally always be exceeded due to the minimum fwi, but can be set lower if there is a desire to maintain all weather, Wang et. al. 2022.  _(Default = 20)_
#' @param directory Directory for files to be output when using the Burn-P3 directory generator. _(Default = "")_
#'
#' @importFrom plyr ddply
#'
#' @return data.frame
#' @export
#'
#' @references [Defining fire spread event days for fire-growth modelling. 2011. Podur,J.; Wotton, M. International Journal of Wildland Fire. 20:497-507](cfs.nrcan.gc.ca/publications?id=32563)
#' @references [Future wildfire extent and frequency determined by the longest fire-conducive weather spell. 2022. Xianli Wang, Tom Swystun, Mike D. Flannigan Science of The Total Environment Vol 830](https://doi.org/10.1016/j.scitotenv.2022.154752)
#'
#' @examples
#'
#' ## Load example data
#' data(weather)
#'
#' spread_event_days(input = weather,
#' yr_col = "yr",
#' id_col = "id",
#' seasonal = FALSE ,
#' season_col = "season",
#' season_names = c("Early Spring",
#'                  "Late Spring",
#'                  "Early Summer",
#'                  "Late Summer",
#'                  "Early Fall",
#'                  "Late Fall"),
#' zonal = FALSE,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine",
#'                "Montane",
#'                "West Alpine",
#'                "West Montane",
#'                "West Interior Douglas Fir"),
#' threshold = 80,
#' min_fwi = 19,
#' directory = "")
#'
#' spread_event_days(input = weather,
#' yr_col = "yr",
#' id_col = "id",
#' seasonal = TRUE ,
#' season_col = "season",
#' season_names = c("Early Spring",
#' "Late Spring",
#' "Early Summer",
#' "Late Summer",
#' "Early Fall",
#' "Late Fall"),
#' zonal = FALSE,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine",
#' "Montane",
#' "West Alpine",
#' "West Montane",
#' "West Interior Douglas Fir"),
#' threshold = 80,
#' min_fwi = 19,
#' directory = "")
#'
#' test <- spread_event_days(input = weather,
#' yr_col = "yr",
#' id_col = "id",
#' seasonal = FALSE ,
#' season_col = "season",
#' season_names = c("Early Spring",
#' "Late Spring",
#' "Early Summer",
#' "Late Summer",
#' "Early Fall",
#' "Late Fall"),
#' zonal = TRUE,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine",
#' "Montane",
#' "West Alpine",
#' "West Montane",
#' "West Interior Douglas Fir"),
#' threshold = 95,
#' min_fwi = 19,
#' min_dmc = 20,
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
                              min_dmc = 20,
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
    ## Step 1: apply the remaining differrence from 100 to the spread event days proportionally to their initial contribution.
    sed$sp_ev_days <- sed$sp_ev_days + round((100 - sum(sed$sp_ev_days))*(sed$sp_ev_days/100),2)
    ## Step 2: To ensure we achieve 100 we evenly distribute the remainder after the bulk application at proportional rates.
    ## This will tend to be a very small number and is due to the proportions removed not being represented by the remaining values.
    if(sum(sed$sp_ev_days) != 100) {sed$sp_ev_days <- sed$sp_ev_days + (100 - sum(sed$sp_ev_days))/nrow(sed)}
    sed$sed_sum <- NULL
    return(sed)
  }

  if (seasonal == T) {
    sed_wx <- ddply(input,c(season_col, yr_col,id_col),.fun = function(x) {
      ## The DMC < 20 comes from Wang and Wotton as they discuss fires in a DMC <20 conditions as a fire ending event.
      ## Here we are collecting weather both above the minimum DMC and the minimum FWI.
      over_thresh <- x$dmc >= min_dmc & x$fwi >= min_fwi
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
    sed_wx <- plyr::ddply(input,c(zone_col, yr_col,id_col),.fun = function(x) {
      over_thresh <- x$dmc >= min_dmc & x$fwi >= min_fwi
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
    sed_wx <- plyr::ddply(.data = input,
                    .variables = c( yr_col,id_col),
                    .fun = function(x) {
                                       over_thresh <- x$dmc >= min_dmc & x$fwi >= min_fwi
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
  return(sed)
}

