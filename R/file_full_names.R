# ASC To Tif Converter ----------------------------------------------------

file_full_names <- list.files("E:/Thesis/prometheus/",pattern = ".asc",recursive = T,full.names = T)
a_to_t <- function(file_full_names){
  
  files.tif <- gsub(".asc",".tif",file_full_names)
  
  files.r <- lapply(file_full_names,raster)
  
  lapply(seq_along(files.tif), function(x){
    writeRaster(files.r[[x]],files.tif[x],format="GTiff",datatype = "INT2S",NAflag = -9999)
  })
  
  unlink(file_full_names)
}