# Area of Interest for Weather Stations -----------------------------------

aoi <- function(area_of_interest_file, reference_grid, buffer_width, park_of_interest="",stns, stn_name_col,stn_id_col){
  
  grast <- raster(reference_grid)
  aoi_poly <- readOGR(dsn = area_of_interest_file[1],layer=area_of_interest_file[2])
  if(park_of_interest != ""){
    aoi_poly <- aoi_poly[grep(park_of_interest,aoi_poly$parkname_e,ignore.case = T),]
  }
  
  aoi_poly <- spTransform(x = aoi_poly ,CRSobj = CRS(proj4string(grast)))
  grast <- crop(extend(grast,extent(gBuffer(aoi_poly,width=buffer_width))),gBuffer(aoi_poly,width=buffer_width))
  
  ## Some basic plotting to visualize where stations are in relation to the AOI
  
  stns_within_aoi <- sort(as.data.frame(crop(spTransform(stns,CRSobj = CRS(proj4string(aoi_poly))),gBuffer(aoi_poly,width=buffer_width)))[,stn_name_col])
  
  x11()
  plot(gBuffer(aoi_poly,width=buffer_width),main=paste0("There are ",length(stns_within_aoi)," staions inside your area of interest"))
  plot(grast,add=T,legend=F)
  plot(spTransform(stns[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,],CRSobj = CRS(proj4string(aoi_poly))),add=T,pch=1)
  text(spTransform(stns[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,],CRSobj = CRS(proj4string(grast))),data.frame(stns)[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,][,stn_id_col])
  plot(spTransform(stns[data.frame(stns)[,stn_name_col] %in% stns_within_aoi,],CRSobj = CRS(proj4string(aoi_poly))),add=T,pch=1)
  
  return(stns_within_aoi)
}