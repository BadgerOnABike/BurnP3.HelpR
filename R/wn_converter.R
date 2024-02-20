#' Wind Ninja ASCII to TIF Converter
#'
#' A quick converter for ASCII wind ninja grids to GeoTiff for smaller storage and faster loading.
#'
#' @importFrom terra writeRaster rast
#'
#' @param directory directory of wind ninja grids. The directory that contains the direction and velocity folders.
#' @export
#'
#'
wn_converter <- function(directory){
  if (length(grep("dir|speed|spd|ang|vel",list.dirs(directory,recursive = F))) > 0) {

    direction <- list.dirs(directory,
                           recursive = F)[grep("dir|ang",
                                               list.dirs(directory,
                                                         recursive = F)
                                               )
                                          ]
    speed <- list.dirs(directory,
                       recursive = F)[grep("speed|spd|vel",
                                           list.dirs(directory,
                                                     recursive = F)
                                           )
                                      ]

    lapply(c(direction,speed), function(i){
      lapply(list.files(i,
                        full.names = T,
                        pattern = ".asc"),
             function(x){
        writeRaster(rast(x),
                    gsub(".asc",".tif",x),
                    wopt = list(filetype = "GTiff",
                                datatype = "INT2s",
                                gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")),
                    NAflag=-9999,
                    overwrite = T)
      }
      )
    }
    )
  } else {
    lapply(list.files(directory,
                      full.names = T,
                      pattern = ".asc"),
           function(x){
      writeRaster(rast(x),
                  gsub(".asc",".tif",x),
                  wopt = list(filetype = "GTiff",
                              datatype = "INT2s",
                              gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")),
                  NAflag=-9999,
                  overwrite = T)
    }
    )
  }
}
