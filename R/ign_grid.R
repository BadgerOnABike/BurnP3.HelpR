# Ignition Grids ----------------------------------------------------------

ign_grid <- function(fire_data,indicator_stack,indicators_1,indicators_2,causes,season_description,output_location,min_fire_size = "",model=""){
  ## Perform a randomForest on each season and each cause as well as one with all season within.
  
  if(model == ""){stop("Please define a model. Either rf for random forest or gbm for gradient boosting machine.")}
  if(min_fire_size != ""){fire_data <- fire_data[which(fire_data$SIZE_HA > min_fire_size),]}
  season_description <- c(season_description,"All")
  indicators_1 <- tolower(indicators_1)
  indicators_2 <- tolower(indicators_2)
  
  ## Random Forest Model
  if(model == "rf"){
    for(cause in causes){
      for(season in min(unique(fire_data$season)):(max(unique(fire_data$season))+1)){
        if(season == max(unique(fire_data$season))+1){
          tab <- cellFromXY(setValues(grast,0), fire_data[fire_data$CAUSE == cause,])
        }else{
          tab <- cellFromXY(setValues(grast,0), fire_data[fire_data$CAUSE == cause & fire_data$season == season,])
        }
        print(paste0("Starting ",cause," - ",season_description[season]))
        pres_abs = setValues(grast,0)
        pres_abs[tab] <- 1
        pres_abs <- mask(pres_abs,grast)
        names(pres_abs) <- "ign"
        modelling_stack <- stack(indicator_stack,pres_abs)
        names(modelling_stack) <- tolower(names(modelling_stack))
        
        # build ign dataframe
        data <- as.data.frame(modelling_stack)
        data <- data[-which(!complete.cases(data)),] # remove NA instances
        data <- data[-which(data$fuels %in% c(101:110)),] ## Remove Rock and Water
        data$ecodistrict <- as.factor(data$ecodistrict) # factor ecozones
        data$ign <- as.factor(data$ign) # factor ecozones
        
        dat_part <- createDataPartition(y = data$ign,p = .8)[[1]]
        data_train <- data[dat_part,]
        data_test <- data[-dat_part,]
        
        data_train <-downSample(x = data_train[,-ncol(data_train)],
                                y= as.factor(data_train$ign),yname = "ign")
        
        data_test <-downSample(x = data_test[,-ncol(data_test)],
                               y= as.factor(data_test$ign),yname = "ign")
        # 
        # data_train$ign <- as.numeric(as.character(data_train$ign))
        # data_test$ign <- as.numeric(as.character(data_test$ign))
        # random forest; sampsize = 75% of pres samples
        # if training dataset not required, use data = data_na in rf model
        if(cause == cause[1]){predictors <- data_train[,indicators_1]}
        if(cause == cause[2]){predictors <- data_train[,indicators_2]}
        
        control <- rfeControl(functions = rfFuncs, method="cv", number = 10)
        results <- rfe(predictors,y = data_train[,"ign"],sizes = c(1,length(predictors)),rfeControl = control)
        predictors <- data_train[,c("ign",results$optVariables)]
        
        if(nrow(predictors) <= 100){warning("Sample was less than 100 elements, skipping.");next}
        
        trControl <- trainControl(method="cv", number = 10, search = "grid")
        # Run the model
        rf_default <- train(ign~.,
                            data = predictors,
                            method = "rf",
                            metric = "Accuracy",
                            trControl = trControl)
        # Print the results
        print(rf_default)
        
        tuneGrid <- expand.grid(.mtry = c(1: 10))
        rf_mtry <- train(ign~.,
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
        for (maxnodes in c(5: 15)) {
          rf_maxnode <- train(ign~.,
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
        results_node <- resamples(store_maxnode)
        x <- summary(results_node)
        if(x$statistics$Accuracy[,"Mean"][length(x$statistics$Accuracy[,"Mean"])]/x$statistics$Accuracy[,"Mean"][1] > 1){
          
          store_maxnode <- list()
          tuneGrid <- expand.grid(.mtry = best_mtry)
          for (maxnodes in c(20: 30)) {
            rf_maxnode <- train(ign~.,
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
          results_node <- resamples(store_maxnode)
          summary(results_node)
        }
        x <- summary(results_node)
        maxnodes <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))
        
        store_maxtrees <- list()
        for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
          rf_maxtrees <- train(ign~.,
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
        results_tree <- resamples(store_maxtrees)
        x <- summary(results_tree)
        ntrees <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))
        
        fit_rf <- train(ign~.,
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
        
        # model variable importance
        
        # predict probability of pres (1) or abs (0)
        prediction <- predict(fit_rf, 
                              data_test)
        #print(importance(prediction))
        confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"]
        
        ign <- raster::predict(model=fit_rf,
                               object=indicator_stack,
                               type="prob",
                               index=2)
        
        ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
        
        writeRaster(ign, 
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
                    overwrite = T)
        
        write(x = c(cause,
                    season_description[season],
                    results$optVariables,
                    paste0("Balanced Accuracy: ",confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
              file = paste0(output_location,
                            "RF_Model_Inputs.txt"),
              append = T,
              sep = "\t",
              ncolumns = length(c(cause,
                                  season,
                                  results$optVariables,
                                  confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
              )
        )
        
      }
    }
  }
  
  ## Gradient Boosting Machine
  if(model == "gbm"){
    for(cause in causes){
      for(season in min(unique(fire_data$season)):(max(unique(fire_data$season))+1)){
        if(season == max(unique(fire_data$season))+1){
          tab <- as.numeric(names(table(cellFromXY(setValues(grast,0), fire_data[fire_data$CAUSE == cause,]))))
        }else{
          tab <- as.numeric(names(table(cellFromXY(setValues(grast,0), fire_data[fire_data$CAUSE == cause & fire_data$season == season,]))))
        }
        
        print(paste0("Starting ",cause," - ",season_description[season]))
        pres_abs = setValues(grast,0)
        pres_abs[tab] <- 1
        pres_abs <- mask(pres_abs,grast)
        names(pres_abs) <- "ign"
        modelling_stack <- stack(indicator_stack,pres_abs)
        names(modelling_stack) <- tolower(names(modelling_stack))
        
        # build ign dataframe
        data <- as.data.frame(modelling_stack)
        data <- data[-which(!complete.cases(data)),] # remove NA instances
        data <- data[-which(data$fuels %in% c(101:110)),] ## Remove Rock and Water
        data$ecodistrict <- as.factor(data$ecodistrict) # factor ecozones
        
        dat_part <- createDataPartition(y = data$ign,p = .8)[[1]]
        data_train <- data[dat_part,]
        data_test <- data[-dat_part,]
        
        data_train <-downSample(x = data_train[,-ncol(data_train)],
                                y= as.factor(data_train$ign),yname = "ign")
        
        data_test <-downSample(x = data_test[,-ncol(data_test)],
                               y= as.factor(data_test$ign),yname = "ign")
        
        ## Attempt a boosted regression tree. 
        # building training dataset may not be required depending on size of area of interest
        # data_train <- data[data$ign == 0,] # subset abs occurrences
        # data_train <- data_train[sample(x = nrow(data_train), 
        #                                 size = (nrow(data[data$ign == 1,])), 
        #                                 replace = F),] # subset abs (19 * number of pres)
        # data_train <- rbind(data[which(data$ign == 1),], 
        #                     data_train) # rbind all pres and sampled abs
        # data_test <- data[-as.numeric(row.names(data_train)),]
        # random forest; sampsize = 75% of pres samples
        # if training dataset not required, use data = data_na in rf model
        
        if(cause == cause[1]){predictors <- indicators_1}
        if(cause == cause[2]){predictors <- indicators_2}
        data_train <- data_train[,c("ign",predictors)]
        
        if(nrow(data_train) <= 100){warning("Sample was less than 100 elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next}
        
        gbm_step <- gbm.step(data_train, 
                             gbm.y=1, 
                             #using the same subset of response variables
                             gbm.x=c(2:ncol(data_train)),
                             #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                             tree.complexity = 3,
                             n.folds = 10,
                             #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                             learning.rate = 0.005,
                             #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                             family = "bernoulli", 
                             max.trees = 2000,
                             bag.fraction= 0.5)
        
        data_train <- data_train[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
        data_test <- data_test[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
        
        gbm_step <- gbm.step(data_train, 
                             gbm.y=1, 
                             #using the same subset of response variables
                             gbm.x=c(2:ncol(data_train)), 
                             #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                             tree.complexity = 3,
                             n.folds = 10,
                             #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                             learning.rate = 0.005,
                             #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                             family = "bernoulli", 
                             max.trees = 2000,
                             bag.fraction= 0.5)
        
        summary(gbm_step)
        
        predictions <- predict(gbm_step,
                               data_test,
                               n.trees = gbm_step$n.trees,
                               type= "response")
        
        gbm_testing <- prediction(predictions, data_test$ign)
        roc_test <- performance(gbm_testing, "tpr","fpr")
        plot(roc_test)
        gbm_auc <- as.numeric(performance(gbm_testing, "auc")@y.values)
        print(gbm_auc)
        
        
        ign <- predict(indicator_stack, gbm_step, n.trees=gbm_step$n.trees, type="response")
        
        scaled_ign <- (ign - cellStats(ign, stat='min')) / (cellStats(ign, stat='max') - cellStats(ign, stat='min'))
        
        scaled_ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
        
        write(x = c(cause,
                    season_description[season],
                    results$optVariables,
                    paste0("Area Under the Curve: ",gbm_auc),
                    paste0("Balanced Accuracy: ",confusionMatrix(as.factor(ifelse(predictions > 0.8, 1, 0)),as.factor(data_test$ign))$byClass['Balanced Accuracy'])),
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
        
        writeRaster(scaled_ign, 
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
                    overwrite = T) # write raster
      }
    }
  }
  
  if(model == "rf_old"){
    for(cause in causes){
      for(season in min(unique(fire_data$season)):(max(unique(fire_data$season))+1)){
        if(season == max(unique(fire_data$season))+1){
          tab <- cellFromXY(setValues(grast,0), fire_data[fire_data$CAUSE == cause,])
        }else{
          tab <- cellFromXY(setValues(grast,0), fire_data[fire_data$CAUSE == cause & fire_data$season == season,])
        }
        print(paste0("Starting ",cause," - ",season_description[season]))
        pres_abs = setValues(grast,0)
        pres_abs[tab] <- 1
        pres_abs <- mask(pres_abs,grast)
        names(pres_abs) <- "ign"
        modelling_stack <- stack(indicator_stack,pres_abs)
        names(modelling_stack) <- tolower(names(modelling_stack))
        
        # build ign dataframe
        data <- as.data.frame(modelling_stack)
        data <- data[-which(!complete.cases(data)),] # remove NA instances
        data <- data[-which(data$fuels %in% c(101:110)),] ## Remove Rock and Water
        data$ecodistrict <- as.factor(data$ecodistrict) # factor ecozones
        data$ign <- as.factor(data$ign) # factor ecozones
        
        dat_part <- createDataPartition(y = data$ign,p = .8)[[1]]
        data_train <- data[dat_part,]
        data_test <- data[-dat_part,]
        
        data_train <-downSample(x = data_train[,-ncol(data_train)],
                                y= as.factor(data_train$ign),yname = "ign")
        
        data_test <-downSample(x = data_test[,-ncol(data_test)],
                               y= as.factor(data_test$ign),yname = "ign")
        
        if(cause == cause[1]){predictors <- data_train[,c("ign",indicators_1)]}
        if(cause == cause[2]){predictors <- data_train[,c("ign",indicators_2)]}
        
        if(nrow(predictors) <= 100){warning("Sample was less than 100 elements, skipping.");next}
        
        # random forest; sampsize = 75% of pres samples
        # if training dataset not required, use data = data in rf model
        rf <- randomForest(ign ~ ., data = predictors, ntree = 500, importance = TRUE, response.type = "binary", sampsize = rep(round(nrow(predictors[predictors$ign == 1,])), 2))
        
        # model variable importance
        importance(rf)
        
        # predict probability of pres (1) or abs (0)
        prediction <- predict(rf, data_test)
        
        print(confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
        
        # build/name ign raster
        ign <- raster::predict(model=rf,
                               object=indicator_stack,
                               type="prob",
                               index=2)
        
        ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
        
        write(x = c(cause,
                    season_description[season],
                    paste0("Balanced Accuracy: ",confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
              file = paste0(output_location,
                            "Initial_RF_Model_Inputs.txt"),
              append = T,
              sep = "\t",
              ncolumns = length(c(cause,
                                  season,
                                  confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
              )
        )
        
        writeRaster(ign, 
                    paste0(output_location,
                           paste(cause,
                                 season_description[season],
                                 "ign_initial_randomforest.tif",
                                 sep = "_")
                    ), 
                    overwrite = T) # write raster
        
      }
    }
  }
}

