plot.VarSelPerformance <-
function(x, ...){

  x_x = c(0,x$x)
  x_y = c(x$y_0,x$y)
  r = x$nVars-x_x
  dh=(max(x_y)-min(x_y))/30
  plot.default(r, x_y, cex=.5, xlab="No. of selected variables", ylab="Performance", main=x$main)
  text(r, x_y+dh, labels=x_x)
}

