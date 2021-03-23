pearsonCoefficient <-
function(x, y) {
  
  r = abs(cor(x, y, method='pearson'))

  return(r)
}

