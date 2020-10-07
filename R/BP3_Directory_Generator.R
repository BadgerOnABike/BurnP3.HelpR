#' Burn-P3 Directory Generator
#'
#' A common directory for Burn-P3 Projects
#'
#' @param root The root directory where the full Burn-P3 directory will be generated.
#' @param project_name The name of the project to generate a Burn-P3 project for.
#'
#' @return
#'
#' @export
#'
#' @examples
#'
#' bp3_dir_gen(project_name = "Test_Project")
#'

bp3_dir_gen <- function(root="",project_name = ""){
  if(root == ""){
    print(paste0("No directory provided, defaulting to: ",getwd()))
    base_dir <<- getwd()
  } else{
    print(paste0("Base directory for all project was defined as: ",root))
    base_dir <<- root
  }
  if(project_name == ""){
    project_name <- paste("BP3_Project",Sys.getenv("USERNAME"),Sys.Date(),sep="_")
    message(paste0("Project name was blank, defaulting to: ",project_name," Located in: ",root))
  }

  dirs <- c("/Inputs/1. Landscape/Fuel",
            "/Inputs/1. Landscape/Elevation",
            "/Inputs/1. Landscape/Weather Zones",
            "/Inputs/1. Landscape/Fire Zones",
            "/Inputs/2. Modules/Ignition Grids",
            "/Inputs/2. Modules/Distribution Tables",
            "/Inputs/2. Modules/Weather",
            "/Inputs/2. Modules/Specialized Weather",
            "/Outputs")
  for(i in dirs) dir.create(paste0(root,project_name,i),recursive = T)

  bp3_base <<- paste0(base_dir,project_name,"/")
  print(paste0("Your Burn-P3 project directory is ",bp3_base))

}
