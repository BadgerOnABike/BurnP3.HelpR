#' Hazard Mapping Utility
#'
#' This utility will use a fire intensity grid and a burn probability grid and output a hazard grid.
#'
#' @param fi Fire intensity spatrast
#' @param bp Burn Probability spatrast (must be the burn probability and not the burn count)
#' @param bp_break_divisor A divisor for the burn probability break, this will dictate the number of HFI breaks required. _Default: 4_
#' @param hfi_breaks A character vector of hfi breaks for use within the mapping system.
#' @param bp_breaks In the event you desire to compare hazards you may define a 4 element burn probability break vector.
#'
#' @importFrom terra values setValues as.data.frame
#'
#' @return SpatRast
#' @export
#'

hazard_mapping <- function(fi,
                           bp,
                           bp_break_divisor = 4,
                           hfi_breaks = c(0,2000,4000,10000),
                           bp_breaks=""){

  hfi_breaks <- c(hfi_breaks[1] + 0.001,
                  hfi_breaks[2:length(hfi_breaks)])

  if (bp_breaks != "") {

    bp_vals <- bp_breaks

  } else {

    bp_vals <- c()

    for (k in 1:bp_break_divisor) {

      bp_vals[k] <- max(terra::values(bp), na.rm = T) * (k/bp_break_divisor)

    }

    }

  haz <- as.data.frame(terra::setValues(bp,
                                        NA),
                       xy = T,
                       na.rm = F)

  bp.df <- as.data.frame(bp, na.rm = F)

  for (i in 1:length(bp_vals)) {

    break_mat <- matrix(ncol = 3, nrow = length(hfi_breaks) + 1)

    for (j in seq_along(hfi_breaks)) {

      if (j == 1) {
        break_mat[j, ] <- c(-Inf,0.000001,NA)
      } else {
        break_mat[j, ] <- c(hfi_breaks[j - 1],hfi_breaks[j], j - 1)
        if (j == max(seq_along(hfi_breaks))) break_mat[j + 1, ] <- c(hfi_breaks[j], Inf, j)
      }

    }
    break_mat[,3] <- break_mat[,3] + (i - 1) * 4

    if (i == 1 & i < length(bp_vals)) {
      haz[ which(bp.df <= bp_vals[ i ]) ,3] <- terra::classify(fi,rcl = break_mat)[bp <= bp_vals[ i ]]
      } else{
        haz[ which(bp.df > bp_vals[ i - 1 ] & bp.df <= bp_vals[ i ]) ,3] <- terra::classify(fi,rcl = break_mat)[bp > bp_vals[ i - 1 ] & bp <= bp_vals[ i ]]
        }
    }

  haz.r <- terra::setValues(bp, haz[,3])
  out_list <- list(bp_vals,haz.r,hfi_breaks)
  names(out_list) <- c("bp_vals","haz.r","hfi_breaks")
  return(out_list)

}
