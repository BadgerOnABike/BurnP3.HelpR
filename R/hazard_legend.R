#' Hazard Legend
#'
#' This function generates a 4x4 legend square for the quartiles of probability and a set of 4 HFI breaks.
#'
#' @param output_location Output location
#' @param output_filename Output filename with png extension
#' @param hfi_breaks A string of 4 numeric HFI breaks
#' @param prob_vals A sequence of percentages you would like to use for your hazard legend.
#'
#' @importFrom graphics abline box grid hist mtext par polygon text
#'
#' @export
#'

hazard_legend <- function(output_location,output_filename, hfi_breaks = c(0,2000,4000,10000), prob_vals = seq(25,100,25) ){

  cols = c(
    "#bee8ff",
    "#73dfff",
    "#d1ff73",
    "#55ff00",
    "#73b2ff",
    "#0070ff",
    "#70a800",
    "#267300",
    "#f5f57a",
    "#ffff00",
    "#e8beff",
    "#df73ff",
    "#f5ca7a",
    "#ffaa00",
    "#e600a9",
    "#a80084")


  if (!grepl("png$",output_filename)) {output_filename <- paste0(output_filename,".png")}

png(paste0(output_location,"/",output_filename),
    height = 1300,
    width = 1600,
    type = "windows")
par(mar = c(20,20,15,5),
    bg = NA)
plot(x = 0:4,
     y = 0:4,
     pch = "",
     xaxs = "i",
     yaxs = "i",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     panel.first = grid(4,4, col = "black", lty = 1),
     main = "HAZARD",
     cex.main = 10)
mtext(side = 1,
      line = 17,
      text = "\nWeighted Mean Fire Intensity (kW/m)",
      cex = 6.5,
      xpd = T)
mtext(side = 2,
      line = 9,
      text = "Percent of Landscape Maximum\nBurn Likelihood (%)",
      cex = 6.5)
for (i in 1:4 ) {
  for (j in 1:4 ) {
    polygon(y = c(i - 1,
                  i - 1,
                  i,
                  i),
            x = c(j - 1,
                  j,
                  j,
                  j - 1),
            col = cols[j + (4 * (i - 1))])
  }
}

abline(v = 2,
       lwd = 10)
abline(v = 1,
       lwd = 7.5)
abline(v = 3,
       lwd = 7.5)
abline(h = 1,
       lwd = 7.5)
abline(h = 3,
       lwd = 7.5)
abline(h = 2,
       lwd = 10)

box(lwd = 10, lend = 2)
text(x = -.2,
     y = 1:4,
     labels = prob_vals,
     xpd = T,
     cex = 4)
text(x = (1:(length(hfi_breaks) )) - .05,
     y = -.25,
     labels = c(prettyNum(hfi_breaks[2:length(hfi_breaks)],big.mark = ","),paste0(">",prettyNum(hfi_breaks[length(hfi_breaks)],big.mark = ","))),
     xpd = T,
     srt = 45,
     adj = c(.75,.75),
     cex = 5.5)
text(x = -.1,
     y = -.1,
     labels = 0,
     srt = 22.5,
     xpd = T,
     cex = 5.5)
dev.off()

print(paste0("Output to ", output_location,"/",output_filename))

}
