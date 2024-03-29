% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ign_grid.R
\name{ign_grid}
\alias{ign_grid}
\title{Ignition Grids}
\usage{
ign_grid(
  fire_data,
  indicator_stack,
  reference_grid,
  indicators_1,
  indicators_2,
  causes,
  season_description,
  output_location,
  min_fire_size = "",
  model = "",
  factor_vars = NULL,
  non_fuel_vals = NULL,
  testing = F
)
}
\arguments{
\item{fire_data}{Spatial Points Data Frame containing wildfire location data, this should be cropped to the area of interest or it will take a very long time to write NA values into the data.}

\item{indicator_stack}{Raster Stack of explanatory variables within the area of interest.}

\item{reference_grid}{Reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.Can be either the location of the raster or a raster object.}

\item{indicators_1}{Character vector describing the layer name from the indicator stack that should be used when calculating ignition probability for the first cause.}

\item{indicators_2}{Character vector describing the layer name from the indicator stack that should be used when calculating ignition probability for the second cause.}

\item{causes}{Character vector describing the causes within the data, typically H (human) and L (lightning), used for naming files and filtering wildfire data. Must correspond with information in the 'cause' column of fire_data.}

\item{season_description}{Character vector with the descriptive names of the factor seasons within the wildfire data.}

\item{output_location}{Directory for the rasters that will be calculated.}

\item{min_fire_size}{A minimum fire size that may be defined to filter the wildfire data provided. \emph{(Default = "")}}

\item{model}{A character string defining the model to be used during the ignition gridding process. Can be one of: rf_stock, rf, gbm and brt. The models are: stock random forest - run without any tuning. Random forest, an automatically tuned random forest run. Gradient Boosted Model - a gbm that is run in its default mode. Boosted Regression Tree - run in its default mode and provides details on the equation yielded.}

\item{factor_vars}{If there are layers that are factors they need to be added to a character vector for use in the function.}

\item{non_fuel_vals}{If there are non-fuels that you want excluded from ignition grids they need to be in a numeric vector.}

\item{testing}{This flag turns off a minimum data check within the modelling process. 100 records are typically required for the model to proceed however the testing dataset is smaller, to improve performance, and as such the minimum data check must be ignored. \emph{(Default = F)}}
}
\value{
spatraster
}
\description{
Ignition grid calculation for use within the Burn-P3 model.
}
\details{
Ignition grids are a probability surface describing the potential for any one cell within a landscape to contain an ignition during a wildfire simulation within Burn-P3. This function provides 3 methods to calculate ignition grids, however many more exist. Consider this function a method to check your data and ensure an ignition surface can be generated. The methods provided are by no means the only way to perform this task, if you develop other methods you feel should be included, please send them my way!
}
\examples{

## Load in example data
data("indicator_stack")
indicator_stack <- terra::unwrap(indicator_stack)
fire_data <- sf::read_sf(system.file("extdata/extdata.gpkg",package = "BurnP3.HelpR"),layer="fires")
indicators_1 <- c("elevation",
                  "road_distance",
                  "rail_distance",
                  "river_distance",
                  "rail_density",
                  "river_density",
                  "topography_position_index",
                  "terrain_ruggedness_index")
indicators_2 <- c("elevation","topography_position_index","solar_rad","Lightning_Density")
causes <- c("H","L")
season_description <- c("Spring","Summer","Fall")
output_location <- paste0(tempdir(),"\\\\")
model = "gbm"

ign_grid(fire_data = fire_data,
         indicator_stack = indicator_stack,
         reference_grid = terra::rast(system.file("extdata","elev.tif",package="BurnP3.HelpR")),
         indicators_1 = indicators_1,
         indicators_2 = indicators_2,
         causes = causes,
         season_description = season_description,
         output_location = output_location,
         min_fire_size = "",
         model = model,
         factor_vars =  c("ecodistrict","ign","in_out_park","fuels","town_boundary"),
         non_fuel_vals = 101:110,
         testing = T)

print(paste0("Test files have been written to: ", output_location))

unlink(tempdir(),recursive = T)
}
