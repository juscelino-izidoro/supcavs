kendallCoefficient <-
function(x, y) {

  r = abs(cor(x, y, method='kendall'))

  return(r)
}

