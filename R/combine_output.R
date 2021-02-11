#' Combine Output
#'
#' @param directory Directory where the outputs are stored and new outputs will be generated to.
#' @param file_prefix A file prefix that will preceed "stats", "polygons" and/or "burn probability". _(Default: "Combined")_
#' @param polygon A logical flag for combination of fire polygons. _(Default: False)_
#' @param raster A logical flag for combination of burn probability rasters. _(Default: True)_
#' @param daily A logical flag for the combination of daily or final perimeters. It will default to final when daily = F. _(Default: False)_
#'
#' @importFrom sf st_read st_write
#' @importFrom terra rast app writeRaster
#'
#' @return
#' @export
#'
#'
#' @examples
#'
combine_output <- function(directory, file_prefix = 'Combined', polygon = F, raster = T, daily = F){

  bp_stats_list <- lapply(X = list.files(path = directory,
                                     pattern = "Statistics",
                                     recursive = T,
                                     full.names = T),
                          FUN = read.csv)

  bp_stats <- bp_stats_list[[1]]

  for (i in 2:length(bp_stats_list)) {

    bp_stats_list[[i]]$fire <- bp_stats_list[[i]]$fire + max(bp_stats$fire,na.rm = T)
    bp_stats <- rbind(bp_stats, bp_stats_list[[i]])

  }

  write.csv(x = bp_stats,
            file = paste0(directory,
                   "/",
                   file_prefix,
                   "_Stats.csv"),
            row.names = F)


  bp_rep_list <- lapply(X = list.files(path = directory,
                                         pattern = "Replay",
                                         recursive = T,
                                         full.names = T),
                          FUN = read.csv)

  bp_rep <- bp_rep_list[[1]]

  for (i in 2:length(bp_rep_list)) {

    bp_rep <- rbind(bp_rep, bp_rep_list[[i]])

  }

  write.csv(x = bp_rep,
            file = paste0(directory,
                          "/",
                          file_prefix,
                          "_Replay.csv"),
            row.names = F)

  if (polygon == T) {
  bp_shapes_list <- lapply(X = list.files(directory,
                                      pattern = if (daily == F) {"_FF.shp$"} else {"_DFF.shp$"},
                                      recursive = T,
                                      full.names = T),
                          FUN = st_read)

  bp_shapes <- bp_shapes_list[[1]]

  for (i in 2:length(bp_shapes_list)) {

    bp_shapes_list[[i]]$fire <- bp_shapes_list[[i]]$fire + max(bp_shapes$fire, na.rm = T)
    bp_shapes <- rbind(bp_shapes, bp_shapes_list[[i]])

  }

  st_write(obj = bp_shapes,
           dsn = paste0(directory,"/Polygons.GPKG"),
           layer = paste0(file_prefix,"_Polygon_Fires"),
           driver = "GPKG",
           append = F)
  }

  if (raster == T) {

  bp_list <- terra::rast(x = list.files(path = directory,
                                 pattern = "Burn_Probability.tif$",
                                 recursive = T,
                                 full.names = T))

  bp_list <- terra::app(x = bp_list,
                 fun = sum)

  writeRaster(x = bp_list,
              filename = paste0(directory,"/",file_prefix,"_Burn_Probability.tif"),
              datatype = "INT2U",
              format = "GTiff",
              overwrite = T)
  }


}



