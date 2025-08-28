#' Burn-P3 Directory
#'
#' A function to generate objects with base and Burn-P3 directories for use in subsequent functions.
#'
#' @param root The root directory where the full Burn-P3 directory currently exists.
#' @param project_name The name of the current Burn-P3 project.
#'
#' @export
#'
#' @examples
#'
#' bp3_dirs(root=tempdir(),project_name = "Test_Project")
#'
bp3_dirs <- function(root="", project_name = "") {
  if (root == "") {
    stop(paste0("Define the root where the project exists."))
  } else{
    print(paste0("Base directory for all project was defined as: ",root))
    base_dir <<- root
  }
  if (project_name == "") {
    stop(paste0("Define the project name."))
  } else {
    bp3_base <<- paste0(base_dir,project_name,"/")
    print(paste0("Your Burn-P3 project directory is ",bp3_base))
  }

}
