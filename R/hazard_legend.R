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

png("E:/Quarantine/Outputs/Result_Figures/BYK/BaselineUpdate2020/Hazard_Legend.png",
    height = 1000,
    width = 1200,
    type = "windows")
par(mar = c(15,20,10,5),
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
     cex.main = 7)
mtext(side = 1,
      line = 13,
      text = "\nWeighted Mean Fire Intensity (kW/m)",
      cex = 4.5,
      xpd = T)
mtext(side = 2,
      line = 10,
      text = "Relative Burn Likelihood Percent\nof Landscape Maximum (%)",
      cex = 4.5)
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
text(x = -.4,
     y = 1:4,
     labels = seq(25,100,25),
     xpd = T,
     cex = 4)
text(x = 1:(length(hfi_breaks) ),
     y = -.25,
     labels = c(prettyNum(hfi_breaks[2:length(hfi_breaks)],big.mark = ","),paste0(">",prettyNum(hfi_breaks[length(hfi_breaks)],big.mark = ","))),
     xpd = T,
     srt = 45,
     adj = c(.75,.75),
     cex = 4)
text(x = -.1,
     y = -.1,
     labels = 0,
     srt = 45,
     xpd = T,
     cex = 4)
dev.off()


## Trying to get this working with ggplot

# plot_polys <- data.frame(x = NA,
#                          y = NA,
#                          id = NA,
#                          col = NA)
# for (i in 1:4 ) {
#   for (j in 1:4 ) {
#     if (i == 1 & j == 1) {
#     plot_polys <- data.frame(y = c(i - 1,
#                                    i - 1,
#                                    i,
#                                    i),
#                              x = c(j - 1,
#                                    j,
#                                    j,
#                                    j - 1),
#                              id = j + (4 * (i - 1)),
#                              col = cols[j + (4 * (i - 1))])
#     print(j + (4 * (i - 1)))
#     print(cols[j + (4 * (i - 1))])
#     } else {
#       plot_polys <- rbind( plot_polys,
#                            data.frame(y = c(i - 1,
#                                      i - 1,
#                                      i,
#                                      i),
#                                      x = c(j - 1,
#                                            j,
#                                            j,
#                                            j - 1),
#                                      id = j + (4 * (i - 1)),
#                                      col = cols[j + (4 * (i - 1))]))
#       print(j + (4 * (i - 1)))
#       print(cols[j + (4 * (i - 1))])
#     }
#   }
# }
#
# ggplot(data = plot_polys) +
#   geom_polygon(aes(x = x,
#                    y = y,
#                    fill = col,
#                    group = id),
#                colour = "black")
