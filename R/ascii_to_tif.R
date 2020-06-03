#' ASC To Tif Converter
#'
#' Basic converter to turn ascii files into tifs for compressed use within Burn-P3
#'
#' @param file_full_names A vector of file names that are currently ascii to be converted into geotif files.
#'
#' @export
#' @import raster
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

  files.r <- lapply(file_full_names,raster)

  lapply(seq_along(files.tif), function(x){
    writeRaster(files.r[[x]],files.tif[x],format="GTiff",datatype = "INT2S",NAflag = -9999)
  })

  unlink(file_full_names)
}
