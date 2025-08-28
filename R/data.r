#' fire_data
#'
#' Examples datasets for the Burn P3 Helpr Package
#'
#' @format a \code{sf::SpatialPointsDataframe} object with typical fire
#' information used when generating ignition grids with 120 rows and 30 columns
#'
#' \describe{
#'  \item{"SRC_AGENCY"}{Source Agency}
#'	\item{"FIRE_ID"}{Agency Fire Number}
#'	\item{"FIRENAME"}{Agency Fire Name (if available)}
#'	\item{"LATITUDE"}{Latitude}
#'	\item{"LONGITUDE"}{Longitude}
#'	\item{"YEAR"}{Year}
#'	\item{"MONTH"}{Month}
#'	\item{"DAY"}{Day}
#'	\item{"REP_DATE"}{Date fire was reported}
#'	\item{"ATTK_DATE"}{Date fire was initially actioned}
#'	\item{"OUT_DATE"}{Date fire was declared out}
#'	\item{"DECADE"}{Decade the fire occurred in}
#'	\item{"SIZE_HA"}{Area burned as reported by the agency in hectares}
#'	\item{"CAUSE"}{Agency declared fire cause}
#'	\item{"PROTZONE"}{Type of protection zone (full response / modified / etc.)}
#'	\item{"FIRE_TYPE"}{Flaming behaviour category, surface, intermittent crown, crown}
#'	\item{"MORE_INFO"}{Additional Information}
#'	\item{"CFS_REF_ID"}{Federal Fire Number}
#'	\item{"CFS_NOTE1"}{Note}
#'	\item{"CFS_NOTE2"}{Note}
#'	\item{"ACQ_DATE"}{Acquisition date}
#'	\item{"ECODISTRIC"}{Ecological District}
#'	\item{"ECOREGION"}{Ecological Region}
#'	\item{"ECOZONE"}{Ecological Zone}
#'	\item{"CFS_ECOZ"}{Ecological Zone - Canadian Forest Service}
#'	\item{"jday"}{Julian Day}
#'	\item{"season"}{Season (1-3 for spring, summer, and fall.)}
#'	}
#'
"fire_data"

#' Weather
#'
#' Example weather data table.
#'
#' @format A data frame with 3270 rows and 23 columns:
#' \describe{
#'   \item{date}{\code{POSIXct} Date of the weather record}
#'   \item{temp}{\code{numeric} Temperature(°C)}
#'   \item{rh}{\code{numeric} Relative Humidity(%)}
#'   \item{ws}{\code{numeric} Wind Speed (km/hr)}
#'   \item{wd}{\code{numeric} Wind Direction (°)}
#'   \item{prec}{\code{numeric} Precipitation (mm)}
#'   \item{ffmc}{\code{numeric} Fine Fuel Moisture Code}
#'   \item{dmc}{\code{numeric} Duff Moisture Code}
#'   \item{dc}{\code{numeric} Drought Code}
#'   \item{bui}{\code{numeric} Build Up Index}
#'   \item{isi}{\code{numeric} Initial Spread Index}
#'   \item{fwi}{\code{numeric} Fire Weather Index}
#'   \item{dsr}{\code{numeric} Daily Severity Rating}
#'   \item{yr}{\code{numeric} Year}
#'   \item{mon}{\code{numeric} Month}
#'   \item{day}{\code{numeric} Day}
#'   \item{id}{\code{factor} Weather Station ID}
#'   \item{station_name}{\code{character} Weather Station Name}
#'   \item{lat}{\code{numeric} Latitude}
#'   \item{long}{\code{numeric} Longitude}
#'   \item{wx_zone}{\code{numeric} Weather Zone}
#'   \item{Class}{\code{factor}}
#'   \item{season}{Levels present: \code{1, 2, 3, 4, 5, 6} Representing Early / Late Spring, Summer, and Fall}
#' }
#'
#' @examples
#' data(weather)
#' head(weather)
#'
"weather"



#' Indicator stack
#'
#' Example raster data (terra) of a set of indicators for building ignition grids.
#'
#' @format A \code{terra::SpatRaster} with 21 layers.
#' Resolution: 100 x 100; Extent: xmin=440200.0000, xmax=465700.0000,
#' ymin=5700500.0000, ymax=5734000.0000;
#' CRS: +proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
#' +ellps=GRS80 +units=m +no_defs
#'
#' \describe{
#'
#'	\item{"elevation"}{Elevation (m)}
#'	\item{"fuels"}{FBP Fuel Type}
#'	\item{"ecodistrict"}{Ecological District}
#'	\item{"topography_position_index"}{Topographic Position Index (TPI)}
#'	\item{"terrain_ruggedness_index"}{Terrain Ruggedness Index (TRI)}
#'	\item{"road_density"}{Road Density}
#'	\item{"trail_density"}{Trail Density}
#'	\item{"asset_distance"}{Euclidian Distance to Assets}
#'	\item{"trail_distance"}{Euclidian Distance to Trails}
#'	\item{"road_distance"}{Euclidian Distance to Roads}
#'	\item{"solar_rad"}{Incident Solar Radiation}
#'	\item{"lightning_density"}{Density of Lightning Strikes}
#'	\item{"asset_density"}{Asset Density}
#'	\item{"rail_distance"}{Distance to Railway}
#'	\item{"rail_density"}{Density of Railway}
#'	\item{"in_out_park"}{In or Out of a National Park}
#'	\item{"river_distance"}{Distance to River}
#'	\item{"river_density"}{Density of River}
#'	\item{"lat"}{Latitude}
#'	\item{"lon"}{Longitude}
#'	\item{"town_boundary"}{In or Out of the boundary of a town of interest}
#'
#' }
#'
#' @examples
#' data(indicator_stack)
#' indicator_stack <- terra::unwrap(indicator_stack)
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   terra::nlyr(indicator_stack); terra::names(indicator_stack)
#' }
#'
"indicator_stack"

#' Lut
#'
#' Example look up table for FBP Fuel Types in Prometheus data.
#'
#' @format A data frame with 154 rows and 10 columns:
#' \describe{
#'   \item{grid_value}{Value in the grid}
#'   \item{export_value}{Value the grid should be interpretted as by Promtheus}
#'   \item{descriptive_name}{Descriptive FBP Fuel Type}
#'   \item{fuel_type}{FBP Fuel Type}
#'   \item{r}{red}
#'   \item{g}{green}
#'   \item{b}{blue}
#'   \item{h}{hue}
#'   \item{s}{saturation}
#'   \item{l}{luminance}
#' }
#'
#' @examples
#' data(lut)
#' head(lut)
#'
"lut"


#' Season df
#'
#' Example seasonal tabular data.
#'
#' @format A data frame with 6 rows and 3 columns:
#' \describe{
#'   \item{jday}{\code{numeric}}
#'   \item{season_desc}{\code{character}}
#'   \item{season}{Levels present: \code{1, 2, 3, 4, 5, 6} Corresponding with
#'   Early / Late Spring, Summer, and Fall}
#' }
#'
#' @examples
#' data(season_df)
#' head(season_df)
#'
"season_df"

#' Interest raster
#'
#' Example raster data (terra) of an area of interest.
#'
#' @format A \code{terra::SpatRaster} with 1 layer.
#' Layer names: fuel
#' Resolution: 99.9999936090549 x 99.9999936090754; Extent: xmin=440201.0086,
#' xmax=465701.0070, ymin=5700499.2515, ymax=5733999.2493;
#' CRS: +proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0
#' +ellps=GRS80 +units=m +no_defs
#'
#' @examples
#' data(interest_raster)
#' interest_raster <- terra::unwrap(interest_raster)
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   terra::nlyr(interest_raster); terra::names(interest_raster)
#' }
#'
"interest_raster"
