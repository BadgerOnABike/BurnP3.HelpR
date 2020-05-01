# ASC To Tif Converter ----------------------------------------------------

ascii_to_tif <- function(file_full_names){
  
  files.tif <- gsub(".asc",".tif",file_full_names)
  
  files.r <- lapply(file_full_names,raster)
  
  lapply(seq_along(files.tif), function(x){
    writeRaster(files.r[[x]],files.tif[x],format="GTiff",datatype = "INT2S",NAflag = -9999)
  })
  
  unlink(file_full_names)
}