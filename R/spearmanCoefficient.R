spearmanCoefficient <-
function(x, y) {

  r = abs(cor(x, y, method='spearman'))

  return(r)
}

