#' ASC To Tif Converter
#'
#' Basic converter to turn ascii files into tifs for compressed use within Burn-P3
#'
#' @param file_full_names A vector of file names that are currently ascii to be converted into geotif files.
#'
#' @importFrom terra rast writeRaster
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' ascii_to_tif(list.files(dir, pattern = ".asc$", full.names = T))
#'
#' }
#'
ascii_to_tif <- function(file_full_names){

  files.tif <- gsub(".asc",".tif",file_full_names)

  files.r <- lapply(file_full_names,rast)

  lapply(seq_along(files.tif), function(x){
    terra::writeRaster(files.r[[x]],files.tif[x],wopt = list(filetype = "GTiff",
                                                      datatype = "INT2S",
                                                      gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")),NAflag = -9999)
  })

  unlink(file_full_names)
}
