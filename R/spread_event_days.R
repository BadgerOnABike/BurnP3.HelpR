# Spread Event Days -------------------------------------------------------

spread_event_days <- function(weather_data, 
                              seasonal = F , 
                              season_col = "season", 
                              season_names = c("Early Spring","Late Spring","Early Summer","Late Summer","Early Fall","Late Fall"), 
                              zonal = F, 
                              zone_col = "wx_zone", 
                              zone_names = c("Alpine","Montane","West Alpine","West Montane","West Interior Douglas Fir"),
                              threshold = 80,
                              min_fwi = 19){
  
  if(length(season_names) != length(unique(weather_data[,season_col]))){warning("There are not enough season names for the number of unique season in your data. The system will proceed and the remaining zones will be unnamed, names are assigned in order of occurrence and may have no meaning.")}
  if(length(zone_names) != length(unique(weather_data[,zone_col]))){warning("There are not enough zone names for the number of unique zones in your data. The system will proceed and the remaining zones will be unnamed, names are assigned in order of occurrence and may have no meaning.")}
  
  
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
    test <- ddply(total_fwi,.(season, yr,id),.fun=function(x){
      over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
      runs <- rle(over_thresh)
      counts <- runs$lengths[runs$values == 1]
      gaps <- runs$lengths[runs$values == 0]
      data.frame(counts=counts)
    })
    sed_seasons <- list()
    for(i in unique(weather_data[,season_col])){
      x <- hist(test[test[,season_col] == i,"counts"],breaks = 0:(max(test$counts)),freq=T)
      print(sum(x$density[1:3]))
      sed_seasons[[i]] <- data.frame(days=x$breaks[-1] ,sp_ev_days=round(x$density*100,2))
    }
    
    names(sed_seasons) <- season_names
    
    lapply(sed_seasons, function(x){sum_thresh_sed(x,threshold)})
    
    lapply(seq_along(sed_seasons),function(x){
      write.csv(x = sed_seasons[[x]],paste0("//S-edm-berg/workgroups/edm/fire/ParksCanadaFireRisk/BP3_Banff_January_28_2020/2. Modules/Seasonal_",names(sed_seasons)[x],"_BYK_SED.csv"),row.names=F)
    })
  }
  
  if (zonal == T){
    test <- ddply(total_fwi,.(wx_zone, yr,id),.fun=function(x){
      over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
      runs <- rle(over_thresh)
      counts <- runs$lengths[runs$values == 1]
      gaps <- runs$lengths[runs$values == 0]
      data.frame(counts=counts)
    })
    sed_zones <- list()
    for(i in seq_along(unique(test$wx_zone))){
      x <- hist(test[test$wx_zone == i,"counts"],breaks = 0:(max(test$counts)),freq=T)
      print(sum(x$density[1:3]))
      sed_zones[[i]] <- data.frame(days=x$breaks[-1] ,sp_ev_days=round(x$density*100,2))
    }
    names(sed_zones) <- zone_names 
    
    
    sed_zones <- lapply(sed_zones, function(x){sum_thresh_sed(x,threshold)})
    
    lapply(seq_along(sed_zones),function(x){
      write.csv(x = sed_zones[[x]],paste0(home_dir,"BYK_February_2020/Inputs/2. Modules/Distribution Tables/Zonal_",names(sed_zones)[x],"_BYK_SED_Zonal.csv"),row.names=F)
    })
  }
  
  if(seasonal ==F & zonal == F){test <- ddply(total_fwi,.(yr,id),.fun=function(x){
    over_thresh <- x$dmc >= 20 & x$fwi >= min_fwi
    runs <- rle(over_thresh)
    counts <- runs$lengths[runs$values == 1]
    gaps <- runs$lengths[runs$values == 0]
    data.frame(counts=counts)
  })
  x <- hist(test[,"counts"],breaks = 0:(max(test$counts)),freq=T)
  sed <- data.frame(days=x$breaks[-1] ,sp_ev_days=round(x$density*100,2))
  
  ## Write out the Spread Event Days
  write.csv(x = sed,paste0(home_dir,"BYK_February_2020/Inputs/2. Modules//Distribution Tables/BYK_SED.csv"),row.names=F)
  }
}
