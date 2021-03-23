naiveBayesModel <-
function(x, y, pcaMethod=NULL, pcaParams=NULL){
  library('e1071')
  
  t = nrow(x)
  
  train = sample(t, t*.66)
  test  = setdiff(1:t, train)
  
  disc = factor(as.numeric(y>mean(y)))
      
  nb = naiveBayes(x[train,], disc[train])
  t = table(predict(nb, x[test,]), disc[test])
  
  resp = sum(diag(t))/sum(t)
  
  return(resp)
}

