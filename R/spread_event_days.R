#' Spread Event Days
#'
#' @param input Weather information file with fire weather index values, the year, and station ids at a minimum.
#' @param seasonal Declaration of the use of seasons in the weather data set. _(Default = F)_
#' @param season_col If seasonal is True, a season column must be declared.
#' @param season_names If seasonal is True, identify the season descriptions for use during output.
#' @param zonal Declaration of the use of weather zones. _(Default = F)_
#' @param zone_col If zonal is True, a zone column must be declared.
#' @param zone_names If zonal is True, identify the descriptive names of the zones for use during mapping and output.
#' @param threshold For weather based spread event day assessments a threshold is necessary to minimize an excessive tail. This ensures a reasonable distribution for short to mid duration fires but excludes long durations. _(Default = 80)_
#' @param min_fwi A minimum fire weather index is used to describe days where fires are more likely to spread and should be consecutively counted. 19 is common in the Canadian Boreal per Podur and Wotton, 2011 _(Default = 19)_
#' @param bp3_base Directory for files to be output when using the Burn-P3 directory generator.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' ## Load example data
#' load("E:/Quarantine/R/BP3_RProject/burn-p3-r-package/data/weather.rda")
#'
#' spread_event_days <- function(input = wx_input,
#' seasonal = F ,
#' season_col = "season",
#' season_names = c("Early Spring","Late Spring","Early Summer","Late Summer","Early Fall","Late Fall"),
#' zonal = F,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine","Montane","West Alpine","West Montane","West Interior Douglas Fir"),
#' threshold = 80,
#' min_fwi = 19)
#'
#' spread_event_days <- function(input = wx_input,
#' seasonal = T ,
#' season_col = "season",
#' season_names = c("Early Spring","Late Spring","Early Summer","Late Summer","Early Fall","Late Fall"),
#' zonal = F,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine","Montane","West Alpine","West Montane","West Interior Douglas Fir"),
#' threshold = 80,
#' min_fwi = 19)
#'
#' spread_event_days <- function(input = wx_input,
#' seasonal = F ,
#' season_col = "season",
#' season_names = c("Early Spring","Late Spring","Early Summer","Late Summer","Early Fall","Late Fall"),
#' zonal = T,
#' zone_col = "wx_zone",
#' zone_names = c("Alpine","Montane","West Alpine","West Montane","West Interior Douglas Fir"),
#' threshold = 80,
#' min_fwi = 19)
#'
spread_event_days <- function(input,
                              seasonal = F ,
                              season_col = "season",
                              season_names = "",
                              zonal = F,
                              zone_col = "wx_zone",
                              zone_names = "",
                              threshold = 80,
                              min_fwi = 19,
                              bp3_base){

  if(length(season_names) != length(unique(input[,season_col]))){warning("There are not enough season names for the number of unique season in your data. The system will proceed and the remaining zones will be unnamed, names are assigned in order of occurrence and may have no meaning.")}
  if(length(zone_names) != length(unique(input[,zone_col]))){warning("There are not enough zone names for the number of unique zones in your data. The system will proceed and the remaining zones will be unnamed, names are assigned in order of occurrence and may have no meaning.")}


  sum_thresh_sed <- function(sed,thresh){
    for(i in 1:nrow(sed)){
      sed[i,"sed_sum"] <- sum(sed[1:i,"sp_ev_days"])
    }
    ## Set a threshold for the cumulative probability  to cut off the SED distribution\
    sed <- sed[-which(sed$sed_sum >= thresh)[-1],]

    ## Add the difference to achieve 100 percent to the 1 day spread.
    sed$sp_ev_days <- sed$sp_ev_days + (100-sum(sed$sp_ev_days))/nrow(sed)

    sed$sed_sum <- NULL
    return(sed)
  }

  if (seasonal == T){
    test <- ddply(input,.(season_col, yr,id),.fun=function(x){
      over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
      runs <- rle(over_thresh)
      counts <- runs$lengths[runs$values == 1]
      gaps <- runs$lengths[runs$values == 0]
      data.frame(counts=counts)
    })
    sed_seasons <- list()
    for(i in unique(input[,season_col])){
      x <- hist(test[test[,season_col] == i,"counts"],breaks = 0:(max(test$counts)),freq=T)
      print(sum(x$density[1:3]))
      sed_seasons[[i]] <- data.frame(days=x$breaks[-1] ,sp_ev_days=round(x$density*100,2))
    }

    names(sed_seasons) <- season_names

    lapply(sed_seasons, function(x){sum_thresh_sed(x,threshold)})

    lapply(seq_along(sed_seasons),function(x){
      write.csv(x = sed_seasons[[x]],paste0(bp3_base,"/Inputs/2. Modules/Distribution Tables/Seasonal_",names(sed_seasons)[x],"_SED_Seasonal.csv"),row.names=F)
    })
  }

  if (zonal == T){
    test <- ddply(input,.(zone_col, yr,id),.fun=function(x){
      over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
      runs <- rle(over_thresh)
      counts <- runs$lengths[runs$values == 1]
      gaps <- runs$lengths[runs$values == 0]
      data.frame(counts=counts)
    })
    sed_zones <- list()
    for(i in seq_along(unique(test[,zone_col]))){
      x <- hist(test[test[,zone_col] == i,"counts"],breaks = 0:(max(test$counts)),freq=T)
      print(sum(x$density[1:3]))
      sed_zones[[i]] <- data.frame(days=x$breaks[-1] ,sp_ev_days=round(x$density*100,2))
    }
    names(sed_zones) <- zone_names


    sed_zones <- lapply(sed_zones, function(x){sum_thresh_sed(x,threshold)})

    lapply(seq_along(sed_zones),function(x){
      write.csv(x = sed_zones[[x]],paste0(bp3_base,"/Inputs/2. Modules/Distribution Tables/Zonal_",names(sed_zones)[x],"_SED_Zonal.csv"),row.names=F)
    })
  }

  if(seasonal ==F & zonal == F){test <- ddply(input,.(yr,id),.fun=function(x){
    over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
    runs <- rle(over_thresh)
    counts <- runs$lengths[runs$values == 1]
    gaps <- runs$lengths[runs$values == 0]
    data.frame(counts=counts)
  })
  x <- hist(test[,"counts"],breaks = 0:(max(test$counts)),freq=T)
  sed <- data.frame(days=x$breaks[-1] ,sp_ev_days=round(x$density*100,2))

  ## Write out the Spread Event Days
  write.csv(x = sed,paste0(bp3_base,"/Inputs/2. Modules/Distribution Tables/SED.csv"),row.names=F)
  }
}
