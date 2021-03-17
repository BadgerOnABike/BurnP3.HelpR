#' Hazard Mapping Utility
#'
#' This utility will use a fire intensity grid and a burn probability grid and output a hazard grid.
#'
#' @param fi Fire intensity spatrast
#' @param bp Burn Probability spatrast (must be the burn probability and not the burn count)
#' @param bp_break_divisor A divisor for the burn probability break, this will dictate the number of HFI breaks required. _Default: 4_
#' @param hfi_breaks A character vector of hfi breaks for use within the mapping system.
#'
#' @importFrom terra values setValues
#'
#' @return SpatRast
#' @export
#'
#' @examples
haz_mapping <- function(fi,
                        bp,
                        bp_break_divisor = 4,
                        hfi_breaks = c(0,2000,4000,10000)){

  hfi_breaks <- c(hfi_breaks[1] + 0.001,
                  hfi_breaks[2:length(hfi_breaks)])

  bp_vals <- c()

  for (i in 1:bp_break_divisor) {
    bp_vals[i] <- max(terra::values(bp), na.rm = T) * (i/bp_break_divisor)
  }


  haz <- as.data.frame(terra::setValues(bp,
                                        NA),
                       xy = T)

  bp.df <- as.data.frame(bp)

  for (i in 1:bp_break_divisor) {

    break_mat <- matrix(ncol = 3, nrow = length(hfi_breaks) + 1)

    for (j in seq_along(hfi_breaks)) {

      if (j == 1) {
        break_mat[j, ] <- c(-Inf,hfi_breaks[j],NA)
      } else {
        break_mat[j, ] <- c(hfi_breaks[j - 1],hfi_breaks[j], j - 1)
        if (j == max(seq_along(hfi_breaks))) break_mat[j + 1, ] <- c(hfi_breaks[j], Inf, j)
      }

    }
    break_mat[,3] <- break_mat[,3] + (i - 1) * 4

    if (i == 1 & i < bp_break_divisor) {
      haz[ which(bp.df <= bp_vals[ i ]) ,3] <- terra::classify(fi,
                                                               rcl = break_mat
      )[bp <= bp_vals[ i ]]
    } else {

      haz[ which(bp.df > bp_vals[ i - 1 ] & bp.df <= bp_vals[ i ]) ,3] <- terra::classify(fi,
                                                                                          rcl = break_mat
      )[bp > bp_vals[ i - 1 ] & bp <= bp_vals[ i ]]

    }

    if (i == bp_break_divisor) {
      haz[ which(bp.df > bp_vals[ i ]), 3] <- terra::classify(fi,
                                                              rcl = break_mat
      )[bp > bp_vals[ i ]]
    }
  }

  haz.r <- terra::setValues(bp, haz[,3])
  out_list <- list(bp_vals,haz.r,hfi_breaks)
  names(out_list) <- c("bp_vals","haz.r","hfi_breaks")
  return(out_list)

}