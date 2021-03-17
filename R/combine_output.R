#' Combine Output
#'
#' @param directory Directory where the outputs are stored and new outputs will be generated to.
#' @param file_prefix A file prefix that will precede "stats", "polygons" and/or "burn probability". _(Default: "Combined")_
#' @param stats_file A text string identifying the name of the statistics file. Can be left blank if your statistics file ends with 'Statistics'.
#' @param bp_file A text string identifying the name of the burn probability file. Can be left blank if your burn probability file ends with 'Probability'.
#' @param polygon A logical flag for combination of fire polygons. _(Default: False)_
#' @param raster A logical flag for combination of burn probability rasters. _(Default: True)_
#' @param daily A logical flag for the combination of daily or final perimeters. It will default to final when daily = F. _(Default: False)_
#'
#' @importFrom sf st_read st_write
#' @importFrom terra rast app writeRaster
#' @importFrom plyr rbind.fill
#'
#' @return
#' @export
#'
#'
#' @examples
#'
combine_output <- function(directory, file_prefix = 'Combined', stats_file, bp_file, polygon = F, raster = T, daily = F){

  bp_stats_list <- lapply(X = list.files(path = directory,
                                     pattern = paste0(stats_file,"|Statistics|Stats"),
                                     recursive = T,
                                     full.names = T),
                          FUN = read.csv)

  bp_stats <- bp_stats_list[[1]]

  for (i in 2:length(bp_stats_list)) {

    bp_stats_list[[i]]$fire <- bp_stats_list[[i]]$fire + max(bp_stats$fire,na.rm = T)
    bp_stats <- plyr::rbind.fill(bp_stats, bp_stats_list[[i]])

  }

  write.csv(x = bp_stats,
            file = paste0(directory,
                   "/",
                   file_prefix,
                   "_Stats.csv"),
            row.names = F)


  bp_rep_list <- lapply(X = list.files(path = directory,
                                         pattern = "Replay.csv$",
                                         recursive = T,
                                         full.names = T),
                          FUN = read.csv)

  bp_rep <- bp_rep_list[[1]]

  for (i in 2:length(bp_rep_list)) {

    bp_rep <- plyr::rbind.fill(bp_rep, bp_rep_list[[i]])

  }

  write.csv(x = bp_rep,
            file = paste0(directory,
                          "/",
                          file_prefix,
                          "_Replay.csv"),
            row.names = F)

  if (polygon == T) {
  bp_shapes_list <- lapply(X = grep("DFF",
                                    list.files(directory,
                                               pattern = "FF.shp$",
                                               recursive = T,
                                               full.names = T),
                                    value = T,
                                    invert = if (daily == T) {F} else {T} ),
                          FUN = st_read)

  bp_shapes <- bp_shapes_list[[1]]

  for (i in 2:length(bp_shapes_list)) {

    bp_shapes_list[[i]]$fire <- bp_shapes_list[[i]]$fire + max(bp_shapes$fire, na.rm = T)
    st_crs(bp_shapes_list[[i]]) <- st_crs(bp_shapes)
    bp_shapes <- plyr::rbind.fill(bp_shapes, bp_shapes_list[[i]])

  }

  st_write(obj = bp_shapes,
           dsn = paste0(directory,"/Polygons.GPKG"),
           layer = paste0(file_prefix,"_Polygon_Fires"),
           driver = "GPKG",
           append = F)
  }

  if (raster == T) {

  bp_list <- terra::rast(x = list.files(path = directory,
                                 pattern = paste0(bp_file,"|Burn_Probability.tif$|Burn_Count.tif$|Probability.tif$|Count.tif$"),
                                 recursive = T,
                                 full.names = T))

  bp_list <- terra::app(x = bp_list,
                 fun = sum)

  writeRaster(x = bp_list,
              filename = paste0(directory,"/",file_prefix,"_Burn_Probability.tif"),
              datatype = "INT2U",
              overwrite = T)
  }


}



