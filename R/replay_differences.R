spring_base_poly <- st_read("E:/Quarantine/JNP_May_2020/Outputs/No_Beetle_Outputs/Spring/Polygons.GPKG")
spring_base_bc <- rast("E:/Quarantine/JNP_May_2020/Outputs/No_Beetle_Outputs/Spring/Spring_Combined_Burn_Probability.tif")

spring_replay_bp <- spring_base_bc/nrow(spring_base_poly)

summer_base_poly <- st_read("E:/Quarantine/JNP_May_2020/Outputs/No_Beetle_Outputs/Summer/Polygons.GPKG")
summer_base_bc <- rast("E:/Quarantine/JNP_May_2020/Outputs/No_Beetle_Outputs/Summer/Summer_Combined_Burn_Probability.tif")

summer_replay_bp <- summer_base_bc/nrow(summer_base_poly)

mean_bp_classification(input = spring_replay_bp,
                       output_filename = "E:/Quarantine/JNP_May_2020/Outputs/No_Beetle_Outputs/Spring/Baseline_Classified_BP.tif")

mean_bp_classification(input = summer_replay_bp,
                       output_filename = "E:/Quarantine/JNP_May_2020/Outputs/No_Beetle_Outputs/Summer/Replay_Classified_BP.tif")

replay_diff <- spring_base_bp/spring_replay_bp

replay_diff <- classify(replay_diff,rcl = matrix(ncol = 2, c(Inf,NA)))

writeRaster(replay_diff,filename = "E:/Quarantine/JNP_May_2020/Outputs/No_Beetle_Outputs/Spring/Replay_Difference_inf_spring.tif",overwrite = T, datatype="FLT4S")

replay_diff <- summer_base_bp/summer_replay_bp

replay_diff <- classify(replay_diff,rcl = matrix(ncol = 2, c(Inf,NA)))

writeRaster(replay_diff,filename = "E:/Quarantine/JNP_May_2020/Outputs/No_Beetle_Outputs/summer/Replay_Difference_inf_summer.tif",overwrite = T, datatype = "FLT4S")

replay_difference <- function(directory, baseline_location, replay_location, probability){

  if(probability == T){
    base_bp <- rast(list.files(path = baseline_location, pattern = "Burn_Probability.tif$",recursive = T,full.names = T))
    replay_bp <- rast(list.files(path = replay_location, pattern = "Burn_Probability.tif$",recursive = T,full.names = T))
  } else{

    if(file.size(paste0(baseline_location,"/Polygons.GPKG")) / 1000000000 > 15) {warning("Polygons are larger than 15 Gb, this will take time and RAM, if you do not have at least 32 Gb of RAM this will fail. 64 Gb RAM recommended.")}

    gc()

    baseline_poly <- st_read(paste0(baseline_location,"/Polygons.GPKG"))
    baseline_bc <- rast(list.files(path = baseline_location, pattern = "Burn_Probability.tif$|Burn_Count.tif$",recursive = T,full.names = T))
    baseline_bp <- baseline_bc / nrow(baseline_poly)
    rm(baseline_poly)

    replay_poly <- st_read(paste0(replay_location,"/Polygons.GPKG"))
    replay_bc <- raste(list.files(path = replay_location, pattern = "Burn_Probability.tif$|Burn_Count.tif$",recursive = T,full.names = T))
    replay_bp <- replay_bc / nrow(replay_poly)
    rm(replay_poly)

  }

}
