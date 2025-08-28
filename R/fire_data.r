#' Data descriptions
#'
#' Examples datasets for the Burn P3 Helpr Package
#'
#' @format ## `fire_data`
#' A data frame with 120 rows and 30 columns
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
#'
#' @format ## `indicator_stack`
#'
#' A spatraster stack with 21 layers
#'
#' \describe{
#'
#'	\item{"elevation"}{Elevation (m)
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
"indicator_stack"
