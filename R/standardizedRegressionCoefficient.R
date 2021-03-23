standardizedRegressionCoefficient <-
function(x, y) {

  cols = ncol(x)
  
  r = matrix(0, cols, 1)

  # calculation about standardized regression coefficient to measure the
  # influence of variables in data.frame x over response variable y
  for(i in 1:cols){
    r[i] = abs((t(x[,i]) %*% as.matrix(y))  / sqrt(t(x[,i]) %*% x[,i]))
  }

  return(r)
}

