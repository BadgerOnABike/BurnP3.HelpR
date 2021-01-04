#' @title  Hours of Burning Function
#' @description The hours of burning are an important part of a Burn-P3 simulation. One third of the total daylight hours are considered to be burning hours. Values are rounded to the nearest whole number and the proportion of each of those hours will be output to a file in your declared location. If you are declaring different hours for the whole simulation, use a single season with the start and end julian days spanning the weather relevant to your study.
#'
#' @param reference_grid A reference raster from the area of interest. This will provide the midpoint of the landscape for use when calculating the day length values
#' @param season_df A data.frame with 3 columns: season, jstart, jend
#' @param out_dir Output directory
#' @param season_col The column containing the season names.
#'
#' @importFrom insol daylength
#' @import terra
#' @import rgdal
#'
#' @return
#' @export
#'
#' @examples
#'
#' elev <- rast(system.file("extdata","elev.tif",package="BurnP3.HelpR"))
#'
#' season_df <- data.frame(season = c("Spring","Summer"),
#'                         jstart = c(125,160),
#'                         jend = c(159, 185))
#'
#' out_dir <- tempdir()
#'
#' burn_hrs( reference_grid = elev,
#'           season_df = season_df,
#'           season_col = "season",
#'           out_dir = out_dir)
#'
#' print(paste0("Files can be found at: ",gsub("\\\\","/",out_dir)))
#'
burn_hrs <- function(reference_grid, season_df, season_col, out_dir){

midpt <- spTransform(
            SpatialPoints(coords =  matrix(ncol = 2,c(mean(ext(reference_grid)@ptr$vector[1:2]),
                                          mean(ext(reference_grid)@ptr$vector[3:4]))
                                          ),
                          proj4string = CRS(crs(reference_grid))),
            CRSobj = CRS("+init=EPSG:4326")
            )

for (j in unique(season_df[,season_col])) {

x <- table(round(daylength(lat = midpt@coords[2],long = midpt@coords[1],tmz = -7,jd = season_df[which(season_df[,season_col] == j),"jstart"]:season_df[which(season_df[,season_col] == j),"jend"])[,"daylen"]/3,0))

burn_hrs <- data.frame(Hours = names(x),Percent = NA)

for (i in seq_along(x)) {
  burn_hrs[i,"Percent"] <- round( x[i] / sum(x), 2)
}

write.csv(x = burn_hrs,
          file = paste0(out_dir,"/",j,"_Burning_Hours.csv"),
          row.names = F)
}
}
