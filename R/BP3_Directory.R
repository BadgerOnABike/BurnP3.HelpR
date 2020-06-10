### Burn-P3 Directory 

bp3_dirs <- function(root="",project_name = ""){
  if(root == ""){
    stop(paste0("Define the root where the project exists."))
  } else{
    print(paste0("Base directory for all project was defined as: ",root))
    base_dir <<- root
  }
  if(project_name == ""){
    stop(paste0("Define the project name."))
  } else {
    bp3_base <<- paste0(base_dir,project_name,"/")
    print(paste0("Your Burn-P3 project directory is ",bp3_base))
  }
  
}