#' Ignition Grids
#'
#' Ignition grid calculation for use within the Burn-P3 model.
#'
#' @details Ignition grids are a probability surface describing the potential for any one cell within a landscape to contain an ignition during a wildfire simulation within Burn-P3. This function provides 3 methods to calculate ignition grids, however many more exist. Consider this function a method to check your data and ensure an ignition surface can be generated. The methods provided are by no means the only way to perform this task, if you develop other methods you feel should be included, please send them my way!
#'
#' @param fire_data Spatial Points Data Frame containing wildfire location data, this should be cropped to the area of interest or it will take a very long time to write NA values into the data.
#' @param indicator_stack Raster Stack of explanatory variables within the area of interest.
#' @param reference_grid Reference raster to provide a projection and a surface to assign values onto, this should be a grid that registers with the other grids you are using for your project.Can be either the location of the raster or a raster object.
#' @param indicators_1 Character vector describing the layer name from the indicator stack that should be used when calculating ignition probability for the first cause.
#' @param indicators_2 Character vector describing the layer name from the indicator stack that should be used when calculating ignition probability for the second cause.
#' @param causes Character vector describing the causes within the data, typically H (human) and L (lightning), used for naming files and filtering wildfire data. Must correspond with information in the 'cause' column of fire_data.
#' @param season_description Character vector with the descriptive names of the factor seasons within the wildfire data.
#' @param output_location Directory for the rasters that will be calculated.
#' @param min_fire_size A minimum fire size that may be defined to filter the wildfire data provided. _(Default = "")_
#' @param model A character string defining the model to be used during the ignition gridding process. Can be one of: rf_stock, rf, gbm and brt. The models are: stock random forest - run without any tuning. Random forest, an automatically tuned random forest run. Gradient Boosted Model - a gbm that is run in its default mode. Boosted Regression Tree - run in its default mode and provides details on the equation yielded.
#' @param testing This flag turns off a minimum data check within the modelling process. 100 records are typically required for the model to proceed however the testing dataset is smaller, to improve performance, and as such the minimum data check must be ignored. _(Default = F)_
#' @param factor_vars If there are layers that are factors they need to be added to a character vector for use in the function.
#' @param non_fuel_vals If there are non-fuels that you want excluded from ignition grids they need to be in a numeric vector.
#'
#' @importFrom terra rast cellFromXY mask setValues global writeRaster
#' @importFrom caret rfeControl rfe downSample createDataPartition rfFuncs train resamples confusionMatrix varImp
#' @importFrom randomForest tuneRF randomForest importance
#' @importFrom dismo gbm.step
#' @importFrom sf read_sf st_coordinates
#' @importFrom data.table as.data.table
#' @import gbm
#'
#' @return spatraster
#'
#' @export
#'
#'
#' @examples
#'
#' ## Load in example data
#' data("indicator_stack")
#' indicator_stack <- terra::unwrap(indicator_stack)
#' fire_data <- sf::read_sf(system.file("extdata/extdata.gpkg",package = "BurnP3.HelpR"),layer="fires")
#' indicators_1 <- c("elevation",
#'                   "road_distance",
#'                   "rail_distance",
#'                   "river_distance",
#'                   "rail_density",
#'                   "river_density",
#'                   "topography_position_index",
#'                   "terrain_ruggedness_index")
#' indicators_2 <- c("elevation","topography_position_index","solar_rad","Lightning_Density")
#' causes <- c("H","L")
#' season_description <- c("Spring","Summer","Fall")
#' output_location <- paste0(tempdir(),"\\")
#' model = "gbm"
#'
#' ign_grid(fire_data = fire_data,
#'          indicator_stack = indicator_stack,
#'          reference_grid = terra::rast(system.file("extdata","elev.tif",package="BurnP3.HelpR")),
#'          indicators_1 = indicators_1,
#'          indicators_2 = indicators_2,
#'          causes = causes,
#'          season_description = season_description,
#'          output_location = output_location,
#'          min_fire_size = "",
#'          model = model,
#'          factor_vars =  c("ecodistrict","ign","in_out_park","fuels","town_boundary"),
#'          non_fuel_vals = 101:110,
#'          testing = T)
#'
#' print(paste0("Test files have been written to: ", output_location))
#'
#' unlink(tempdir(),recursive = T)


ign_grid <- function(fire_data,
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
                     testing = F){

  if ( grepl("SpatRaster", class(reference_grid)) ) { grast <- reference_grid }
  if ( grepl("character", class(reference_grid)) ) { grast <- terra::rast(reference_grid) }
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) { message("Reference Grid must be the directory of the raster or a raster object.") }

  if (model == "") {
    stop("Please define a model. Either rf for an automatically refined random forest, rf_stock for a baseline randomForest::randomForest, gbm for gradient boosting machine or brt for a boosted regression tree.")}
  if (min_fire_size != "") {fire_data <- fire_data[which(fire_data$SIZE_HA > min_fire_size),]}
  season_description <- c(season_description,"All")
  indicators_1 <- tolower(indicators_1)
  indicators_2 <- tolower(indicators_2)

  # Random Forest -----------------------------------------------------------

  if (model == "rf") {
    for (cause in causes) {
      for (season in min(unique(fire_data$season)):(max(unique(fire_data$season)) + 1)) {
        if (season == max(unique(fire_data$season)) + 1) {
          tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$CAUSE == cause,]))
        } else {
          tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$CAUSE == cause & fire_data$season == season,]))
        }

        print(paste0("Starting ",cause," - ",season_description[season]))
        pres_abs = terra::setValues(grast,0)
        pres_abs[tab] <- 1
        pres_abs <- terra::mask(pres_abs,grast)
        names(pres_abs) <- "ign"
        modelling_stack <- c(indicator_stack,pres_abs)
        names(modelling_stack) <- tolower(names(modelling_stack))

        # build ign dataframe
        data <- data.table::as.data.table(modelling_stack)
        data <- data[complete.cases(data)] # remove NA instances
        if (!is.null( non_fuel_vals ) ) data <- data[!fuels %in% non_fuel_vals] ## Remove Rock and Water
        if (!is.null( factor_vars ) ) data[,
                                           (factor_vars) := lapply(.SD, as.factor),
                                           .SDcols = factor_vars]

        data_mod <- caret::downSample(x = data[,-"ign"],
                                      y = data$ign,
                                      yname = "ign")

        dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
        data_train <- data_mod[dat_part,]
        data_test <- data_mod[-dat_part,]

        repeat {

          if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)]}
          if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)]}

          control <- caret::rfeControl(functions = caret::rfFuncs, method = "cv",number = 10,repeats = 2)
          results <- caret::rfe(x = predictors[-1],y = predictors[,"ign"],sizes = c(1:length(predictors)),rfeControl = control, p = 1, metric = "Accuracy")
          print(results)

          if ( cause == "L" && max(results$results$Accuracy) > 0.65 ) { break }
          if ( cause == "H" && max(results$results$Accuracy) > 0.70 && results$results[which.max(results$results$Accuracy),"Variables"] > 3 ) { break }

        }

        predictors <- predictors[,c("ign",results$optVariables)]

        if (testing == F) {
          if (nrow(data_train) <= 100) {
            warning("Sample was less than 100 elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }

        trControl <- caret::trainControl(method = "cv",
                                  number = 10,
                                  search = "grid")
        # Run the model
        rf_default <- caret::train(ign~.,
                            data = predictors,
                            method = "rf",
                            metric = "Accuracy",
                            trControl = trControl)
        # Print the results
        print(rf_default)

        tuneGrid <- expand.grid(.mtry = c(1:10))
        rf_mtry <- caret::train(ign~.,
                         data = predictors,
                         method = "rf",
                         metric = "Accuracy",
                         tuneGrid = tuneGrid,
                         trControl = trControl,
                         importance = TRUE,
                         nodesize = 14,
                         ntree = 300)
        print(rf_mtry)
        best_mtry <- rf_mtry$bestTune$mtry
        mtry <- best_mtry

        store_maxnode <- list()
        tuneGrid <- expand.grid(.mtry = best_mtry)
        for (maxnodes in c(5:15)) {
          rf_maxnode <- caret::train(ign~.,
                              data = predictors,
                              method = "rf",
                              metric = "Accuracy",
                              tuneGrid = tuneGrid,
                              trControl = trControl,
                              importance = TRUE,
                              nodesize = 14,
                              maxnodes = maxnodes,
                              ntree = 300)
          current_iteration <- toString(maxnodes)
          store_maxnode[[current_iteration]] <- rf_maxnode
        }
        results_node <- caret::resamples(store_maxnode)
        x <- summary(results_node)
        print(rf_maxnode)
        if (x$statistics$Accuracy[,"Mean"][length(x$statistics$Accuracy[,"Mean"])]/x$statistics$Accuracy[,"Mean"][1] > 1) {

          store_maxnode <- list()
          tuneGrid <- expand.grid(.mtry = best_mtry)
          for (maxnodes in c(20:30)) {
            rf_maxnode <- caret::train(ign~.,
                                data = predictors,
                                method = "rf",
                                metric = "Accuracy",
                                tuneGrid = tuneGrid,
                                trControl = trControl,
                                importance = TRUE,
                                nodesize = 14,
                                maxnodes = maxnodes,
                                ntree = 300)
            key <- toString(maxnodes)
            store_maxnode[[key]] <- rf_maxnode
          }
          results_node <- caret::resamples(store_maxnode)
          summary(results_node)
        }
        x <- summary(results_node)
        maxnodes <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))

        store_maxtrees <- list()
        for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
          rf_maxtrees <- caret::train(ign~.,
                               data = predictors,
                               method = "rf",
                               metric = "Accuracy",
                               tuneGrid = tuneGrid,
                               trControl = trControl,
                               importance = TRUE,
                               nodesize = 14,
                               maxnodes = maxnodes,
                               ntree = ntree)
          key <- toString(ntree)
          store_maxtrees[[key]] <- rf_maxtrees
        }
        results_tree <- caret::resamples(store_maxtrees)
        x <- summary(results_tree)
        ntrees <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))

        fit_rf <- caret::train(ign~.,
                        data = predictors,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        nodesize = 14,
                        maxnodes = maxnodes,
                        ntree = ntrees)

        # model diagnostics
        print(fit_rf)

        # model variable randomForest::importance

        # predict probability of pres (1) or abs (0)
        prediction <- predict(fit_rf,
                              data_test)
        print(varImp(fit_rf,useModel = T,scale = F))

        write(x = paste0(cause," ",
                    season_description[season]," ",
                    results$optVariables," ",
                    varImp(fit_rf,useModel = T,scale = F)),
              file = paste0(output_location,
                            "RF_Var_Imp.txt"),
              append = T,
              sep = "\n",
              ncolumns = 1
              )

        caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"]

        ign <- terra::predict(object = indicator_stack,
                              model = rf_default,
                              type = "prob",
                              fun = predict,
                              na.rm=T)[[2]]

        ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0

        terra::writeRaster(rast(ign),
                    paste0(output_location,
                           paste(cause,
                                 season_description[season],
                                 ifelse(min_fire_size != "",
                                        paste0("ign_randomforest_auto_over_",
                                               min_fire_size,
                                               ".tif"),
                                        "ign_randomforest_auto.tif"),
                                 sep = "_")
                    ),
                    overwrite = T,
                    wopt = list(filetype = "GTiff",
                                datatype = "FLT4S",
                                gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))

        write(x = c(cause,
                    season_description[season],
                    results$optVariables,
                    paste0("Balanced Accuracy: ",caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
              file = paste0(output_location,
                            "RF_Model_Inputs.txt"),
              append = T,
              sep = "\t",
              ncolumns = length(c(cause,
                                  season,
                                  results$optVariables,
                                  caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
              )
        )

      }
    }
  }


# Gradient Boosting Machine -----------------------------------------------

  if (model == "gbm") {
    for (cause in causes) {
      for (season in min(unique(fire_data$season)):(max(unique(fire_data$season)) + 1)) {
        if (season == max(unique(fire_data$season)) + 1) {
          tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$CAUSE == cause,]))
        } else {
          tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$CAUSE == cause & fire_data$season == season,]))
        }

        print(paste0("Starting ",cause," - ",season_description[season]))
        pres_abs = terra::setValues(grast,0)
        pres_abs[tab] <- 1
        pres_abs <- terra::mask(pres_abs,grast)
        names(pres_abs) <- "ign"
        modelling_stack <- c(indicator_stack,pres_abs)
        names(modelling_stack) <- tolower(names(modelling_stack))


        # build ign dataframe
        data <- data.table::as.data.table(modelling_stack)
        data <- data[complete.cases(data)] # remove NA instances
        if (!is.null( non_fuel_vals ) ) data <- data[!fuels %in% non_fuel_vals] ## Remove Rock and Water
        if (!is.null( factor_vars ) ) data[,
                                           (factor_vars) := lapply(.SD, as.factor),
                                           .SDcols = factor_vars]

        data_mod <- caret::downSample(x = data[,-"ign"],
                                      y = data$ign,
                                      yname = "ign")

        dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
        data_train <- data_mod[dat_part,]
        data_test <- data_mod[-dat_part,]

        if (cause == causes[1]) {predictors <- indicators_1}
        if (cause == causes[2]) {predictors <- indicators_2}
        data_train <- data_train[,c("ign",predictors)]

        if (testing == F) {
          if (nrow(data_train) <= 100) {
            warning("Sample was less than 100 elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }

        gbm_step <- dismo::gbm.step(data_train,
                             gbm.y = 1,
                             #using the same subset of response variables
                             gbm.x = c(2:ncol(data_train)),
                             #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                             tree.complexity = 3,
                             n.folds = 10,
                             #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                             learning.rate = 0.005,
                             #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                             family = "bernoulli",
                             max.trees = 2000,
                             bag.fraction = 0.5)

        data_train <- data_train[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
        data_test <- data_test[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]

        gbm_step <- dismo::gbm.step(data_train,
                             gbm.y = 1,
                             #using the same subset of response variables
                             gbm.x = c(2:ncol(data_train)),
                             #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                             tree.complexity = 3,
                             n.folds = 10,
                             #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                             learning.rate = 0.005,
                             #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                             family = "bernoulli",
                             max.trees = 2000,
                             bag.fraction = 0.5)

        summary(gbm_step)

        predictions <- predict(gbm_step,
                               data_test,
                               n.trees = gbm_step$n.trees,
                               type = "response")

        gbm_testing <- prediction(predictions, data_test$ign)
        roc_test <- performance(gbm_testing, "tpr","fpr")
        plot(roc_test)
        gbm_auc <- as.numeric(performance(gbm_testing, "auc")@y.values)
        print(gbm_auc)


        ign <- terra::predict(indicator_stack,
                       gbm_step,
                       n.trees = gbm_step$n.trees,
                       type = "response")

        scaled_ign <- (ign - global(ign, fun = 'min')) / (global(ign, fun = 'max') - global(ign, fun = 'min'))

        scaled_ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0

        write(x = c(cause,
                    season_description[season],
                    results$optVariables,
                    paste0("Area Under the Curve: ",gbm_auc),
                    paste0("Balanced Accuracy: ",caret::confusionMatrix(as.factor(ifelse(predictions > 0.8, 1, 0)),as.factor(data_test$ign))$byClass['Balanced Accuracy'])),
              file = paste0(output_location,
                            "GBM_Model_Inputs.txt"),
              append = T,
              sep = "\t",
              ncolumns = length(c(cause,
                                  season,
                                  results$optVariables,
                                  gbm_auc)
              )
        )

        terra::writeRaster(scaled_ign,
                    paste0(output_location,
                           paste(cause,
                                 season_description[season],
                                 ifelse(min_fire_size != "",
                                        paste0("ign_gbmstep_over_",
                                               min_fire_size,
                                               ".tif"),
                                        "ign_gbmstep.tif"),
                                 sep = "_")
                    ),
                    overwrite = T,
                    wopt = list(filetype = "GTiff",
                                datatype = "FLT4S",
                                gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2"))) # write raster
      }
    }
  }


# Random Forest Stock -----------------------------------------------------

  if (model == "rf_stock") {
    for (cause in causes) {
      for (season in min(unique(fire_data$season)):(max(unique(fire_data$season)) + 1)) {
        if (season == max(unique(fire_data$season)) + 1) {
          tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$CAUSE == cause,]))
        } else {
          tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$CAUSE == cause & fire_data$season == season,]))
        }

        print(paste0("Starting ",cause," - ",season_description[season]))
        pres_abs = terra::setValues(grast,0)
        pres_abs[tab] <- 1
        pres_abs <- terra::mask(pres_abs,grast)
        names(pres_abs) <- "ign"
        modelling_stack <- c(indicator_stack,pres_abs)
        names(modelling_stack) <- tolower(names(modelling_stack))

        # build ign dataframe
        data <- data.table::as.data.table(modelling_stack)
        data <- data[complete.cases(data),] # remove NA instances
        if (!is.null( non_fuel_vals ) ) data <- data[-which(data$fuels %in% non_fuel_vals),] ## Remove Rock and Water
        if (!is.null( factor_vars ) ) data[,
                                           (factor_vars) := lapply(.SD, as.factor),
                                           .SDcols = factor_vars]

        data_mod <- caret::downSample(x = data[,-"ign"],
                                      y = data$ign,
                                      yname = "ign")

        dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
        data_train <- data_mod[dat_part,]
        data_test <- data_mod[-dat_part,]

        if (testing == F) {
          if (nrow(data_train) <= 100) {
            warning("Sample was less than 100 elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }

        repeat {

        if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)]}
        if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)]}

        control <- caret::rfeControl(functions = caret::rfFuncs,
                              method = "cv",
                              number = 10,
                              repeats = 2)
        results <- caret::rfe(x = predictors[-1],
                       y = predictors[,"ign"],
                       sizes = c(1:length(predictors)),
                       rfeControl = control,
                       p = 1,
                       metric = "Accuracy")
        print(c("Results for ", paste0("Starting ",cause," - ",season_description[season]), "\n",results))

        if ( cause == "L" && max(results$results$Accuracy) > 0.65 ) { break }
        if ( cause == "H" && max(results$results$Accuracy) > 0.70 && results$results[which.max(results$results$Accuracy),"Variables"] > 3 ) { break }

        }

        predictors <- predictors[,c("ign",results$optVariables)]

        best_mtry <- randomForest::tuneRF(predictors[-1],
                            predictors$ign,
                            ntree = 1500,
                            stepFactor = 1.5,
                            improve = 0.01,
                            trace = TRUE,
                            plot = TRUE)
        best_mtry <- best_mtry[which.min(best_mtry[,2]),1]

        rf <- randomForest::randomForest(ign ~ .,
                           data = predictors,
                           ntree = 1500,
                           mtry = best_mtry,
                           importance = TRUE,
                           response.type = "binary")

        # model variable randomForest::importance
        imp <- randomForest::importance(rf)

        # predict probability of pres (1) or abs (0)
        prediction <- predict(rf, data_test)

        print(caret::confusionMatrix(prediction,
                              data_test$ign)$byClass["Balanced Accuracy"])

        # build/name ign raster
        ign <- terra::predict(model = rf,
                               object = indicator_stack,
                               type = "prob")[[2]]

        x11()
        plot(ign)

        ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0

        write(x = c(cause,
                    season_description[season],
                    paste(names(sort(imp[,3],decreasing = T)),round(sort(imp[,3],decreasing = T),2)),
                    paste0("Balanced Accuracy: ",caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
              file = paste0(output_location,
                            "RF_Model_Inputs.txt"),
              append = T,
              sep = "\t",
              ncolumns = length(c(cause,
                                  season,
                                  paste(names(sort(imp[,3],decreasing = T)),round(sort(imp[,3],decreasing = T),2)),
                                  caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
              )
        )

        terra::writeRaster(ign,
                    paste0(output_location,
                           paste(cause,
                                 season_description[season],
                                 "ign_randomforest.tif",
                                 sep = "_")
                    ),
                    overwrite = T,
                    wopt = list(filetype = "GTiff",
                                datatype = "FLT4S",
                                gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))

      }
    }
  }


# Boosted Regression Tree -------------------------------------------------

  if (model == "brt") {
    for (cause in causes) {
      for (season in min(unique(fire_data$season)):(max(unique(fire_data$season)) + 1)) {
        if (season == max(unique(fire_data$season)) + 1) {
          tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$CAUSE == cause,]))
        } else {
          tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$CAUSE == cause & fire_data$season == season,]))
        }

        print(paste0("Starting ",cause," - ",season_description[season]))
        pres_abs = terra::setValues(grast,0)
        pres_abs[tab] <- 1
        pres_abs <- terra::mask(pres_abs,grast)
        names(pres_abs) <- "ign"
        modelling_stack <- c(indicator_stack,pres_abs)
        names(modelling_stack) <- tolower(names(modelling_stack))

        # build ign dataframe
        data <- data.table::as.data.table(modelling_stack)
        data <- data[complete.cases(data),] # remove NA instances
        if (!is.null( non_fuel_vals ) ) data <- data[-which(data$fuels %in% non_fuel_vals),] ## Remove Rock and Water
        if (!is.null( factor_vars ) ) data[,
                                           (factor_vars) := lapply(.SD, as.factor),
                                           .SDcols = factor_vars]

        data_mod <- caret::downSample(x = data[,-"ign"],
                                      y = data$ign,
                                      yname = "ign")

        dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
        data_train <- data_mod[dat_part,]
        data_test <- data_mod[-dat_part,]

        data_train$ign <- as.integer(as.character(data_train$ign))

        if (testing == F) {
          if (nrow(data_train) <= 100) {
            warning("Sample was less than 100 elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }

        if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)] }
        if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)] }

        brt <- dismo::gbm.step(data = predictors,
                        gbm.x = 2:length(predictors),
                        gbm.y = 1,
                        tree.complexity = 3,
                        family = "bernoulli",
                        n.folds = 20,
                        n.trees = 500,
                        step.size = 50,
                        max.trees = 7500,
                        learning.rate = 0.0005)

        brt <- dismo::gbm.step(data = predictors,
                        gbm.x = which(names(predictors) %in% brt$contributions[brt$contributions$rel.inf >= 1.0, "var"]),
                        gbm.y = 1,
                        tree.complexity = 3,
                        family = "bernoulli",
                        n.folds = 20,
                        n.trees = 500,
                        step.size = 50,
                        max.trees = 7500,
                        learning.rate = 0.0005,
                        plot.main = T)

        # model variable randomForest::importance
        imp <- summary(brt)

        # build/name ign raster
        ign <- terra::predict(model = brt,
                               object = indicator_stack,
                               type = "response",
                               n.trees = brt$n.trees,
                               na.rm = T)

        ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0

        ign <- (ign - min(ign[],na.rm = T))/(max(ign[],na.rm = T) - min(ign[],na.rm = T))

        write(x = c(cause,
                    season_description[season],
                    paste("CV AUC: ",round(mean(brt$cv.roc.matrix),2)),
                    paste(imp[,"var"],round(imp[,"rel.inf"],2),sep = ": ")
                    ),
              file = paste0(output_location,
                            "BRT_Model_Inputs.txt"),
              append = T,
              sep = "\t",
              ncolumns = length(c(cause,
                                  season,
                                  paste(imp[,"var"],round(imp[,"rel.inf"],2),sep = ": "),
                                  mean(brt$cv.roc.matrix))
              )
        )

        terra::writeRaster(ign,
                    paste0(output_location,
                           paste(cause,
                                 season_description[season],
                                 "ign_boostedregressiontree.tif",
                                 sep = "_")
                    ),
                    overwrite = T,
                    wopt = list(filetype = "GTiff",
                                datatype = "FLT4S",
                                gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))

      }
    }
  }
}

