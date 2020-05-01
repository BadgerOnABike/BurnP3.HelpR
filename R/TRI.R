# Windowed TRI ------------------------------------------------------------

TRI <- function(x,window_size){
  
  f <- matrix(1, nc=window_size, nr=window_size)
  f[ceiling(0.5 * length(f))] <- 0
  
  focal(x, f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)
}
