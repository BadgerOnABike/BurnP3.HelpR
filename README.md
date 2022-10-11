# BurnP3.HelpR-package
A package for R that assists with Burn-P3 input generation.

To install:

```
install.packages("devtools")
devtools::install_github("BadgerOnABike/Burnp3.HelpR")
```

 # 0. Establish Area of Interest and Question

 While the package will be unable to do most of this work for you, we will describe the inputs you need ready to use this tool effectively.

 The key inputs needed prior to use of this package:
 * Area of Interest - polygon
 * Base raster for your area - raster
 * Data frame of Weather Information - data.frame
 * Data frame of wildfire ignitions in area of interest - data.frame
 * Explanatory variables for Ignition Grid generation - point, line, polygon, and raster

 *__Note__: There is an [aoi](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/aoi.R) function that can assist in generating a polygon for use within the model, however if the Canadian National Parks are not the area of interest a polygon of the area is required.

 # 1. Establish Directory

 The [bp3_dir_gen](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/BP3_Directory_Generator.R) function is set up to generate a work space for Burn-P3 under the current generation windows build (2021.08.00). This will flesh out the basic requirements for any Burn-P3 project. This should be run every time as it will not overwrite or regenerate the directory but does create directory objects for later use. Specifically bp3_base and base_dir. The bp3_base is the directory where the project has been instantiated, base_dir is the working directory.

  # 2. Fuel Grid Generation

  Fuel and elevation data are key to the operation of any fire growth model. In this case the elevation is automatically generated within Canada from the 30m - CDEM. Fuels will be developed from a layering of polygonal and raster layers delivered to the [fuel_grid_generator](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/fuel_grid_generator.R) function. This function will layer the files in the order that they are given to the function and return a fuel raster. Upon receipt of the fuel layer an elevation layer can be developed.

  # 3. Elevation Grid Generation

  The Fuel raster is used as a base layer until the elevation grid is derived at which point the elevation is the common reference grid. Elevation is derived from the [CDEM data](https://open.canada.ca/data/en/dataset/7f245e4d-76c2-4caa-951a-45d1d2051333) which is a 30-m resolution. Within [elev_grab](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/elev_grab.R) the elevation will be resampled and reprojected to the reference grid. Two layers will be output, the elevation masked to the area of interest and an extent wide elevation layer. The latter layer is for use within the Wind Ninja model if a user is interested in using that tool.

  # 4. Define Fire and Weather Zones

  Fire and Weather zones describe areas with unique fire regimes and weather patterns. The fire and weather zone layers give rise to tabular occurrence which will be developed in a later stage of this process. The occurrence tables regarding the proportion of fires occurring by season, fire zone and weather zone are key to the modelling system. It is not a requirement to have both fire and weather zones however it is very typical to have weather zones due to the landscape level processes that Burn-P3 is attempting to leverage. Fire zones require a large number of fires to accurately describe fire zones. There are no specific functions to create weather zones and fire zones, those are a work in progress. As it stands the layers required are rasters describing the unique areas within the area of interest.

  # 5. Ignition Grids

  Ignition grids are a primary driving force of the Burn-P3 model. Ignition grids describe the probability of an ignition starting at any one location across the area of interest. Within the [ign_grid](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/ign_grid.R) function there are current 4 models available for developing ignition grids and more to be added over time. The 4 current models are 2 forms of random forest, a gradient boosted model and a boosted regression tree. It is recommendable to test various models in order to define best fit for your area of interest. These models are also not the only way to perform this process and other methods are acecptable and encouraged. *Note: If you have another way of performing these tasks, please notifying the authors and tehy will entertain incorporating it into the package.

  Ignition grids require the largest amount of forethought and input data. There are a number of helper functions to prepare spatial data for use in the ignition gridding process. Specifically [density_ign](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/density_ign.R) and [distance_ign](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/distance_ign.R) which generate kernal density and euclidean distance grids respectively. Those functions use further helper functions to ingest and process the raw data to prepare it for the primary tool. Topographic position index and topographic roughness index are typically used to describe ignition potential in lightning and human caused fire respectively. The [tpi.bp3](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/tpi.bp3.R) uses [tpi_w](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/tpi.bp3.R) and [TRI](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/tpi.bp3.R) functions develop these layers explicitly. A window may be set by the user within [tpi.bp3](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/tpi.bp3.R) and a raster stack will be returned with named variables for use.

  # 6. Weather List

  Currently broader weather processing is not supported by this package, however there are some final tweaks that should be made. Specifically conforming to the weather list requirements of the Burn-P3 model and limiting the weather to the seasons and zones that would be called by the model.

  # 7. Distributions for Random Selection

  ## 7.1  Spread Event Days

  The spread event day distribution table describes the number of "simulation days" within the model. A simulation day is declared within the model as a number of hours that the fire would simulate for. The spread event day distribution then defines the number of simulation days, these distributions can be developed either with real fire information from satellite data or potential spread event days with weather data. The functions in this package are focused on weather based analysis, fire based analysis is a work in progress. The function for spread event days is [spread_event_days](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/spread_event_days.R).

  ## 7.2 Fire Rates

  The fire rates table describes the number of fires that occur by cause and season. This table can be further descretized to the fire zone level, however it is not required. The function for fire rates is [fire_rate_distribution](https://github.com/BadgerOnABike/BurnP3.HelpR/blob/master/R/fire_rate_distribution.R).

  # 8. Outputs

  Upon completion of Burn-P3 modeling the outputs will require some processing and presentation. Some tools for quick viewing are in development but are currently works in progress.
