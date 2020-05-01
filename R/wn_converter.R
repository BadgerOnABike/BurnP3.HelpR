# Wind Ninja ASCII to TIF Converter ---------------------------------------

library(raster)

wn_converter <- function(directory){
  if(length(grep("dir|speed|spd",list.dirs(directory,recursive = F))) > 0){
    
    direction <- list.dirs(directory,recursive = F)[grep("dir",list.dirs(directory,recursive = F))]
    speed <- list.dirs(directory,recursive = F)[grep("speed|spd",list.dirs(directory,recursive = F))]
    
    lapply(c(direction,speed), function(i){
      lapply(list.files(i,full.names=T,pattern=".asc"),function(x){
        writeRaster(raster(x),gsub(".asc",".tif",x),format = "GTiff",NAflag= -9999, datatype = "INT2S", overwrite=T)
      }
      )
    }
    )
  } else {
    lapply(list.files(directory,full.names=T,pattern=".asc"),function(x){
      writeRaster(raster(x),gsub(".asc",".tif",x),format = "GTiff",NAflag= -9999, datatype = "INT2S", overwrite=T)
    }
    )
  }
}