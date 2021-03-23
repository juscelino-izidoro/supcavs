linearRegressionModel <-
function(x, y, pcaMethod="prcomp", pcaParams=NULL){
  m = lm(y ~ ., data=as.data.frame(x))

  resp = abs(summary(m)$r.squared)
  
  return(resp)
}

