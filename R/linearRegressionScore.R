linearRegressionScore <-
function(x, y=NULL, p1=NULL, pcaMethod='prcomp', pcaParams=NULL){
  return(linearRegressionModel(x=x, y=y, pcaMethod=pcaMethod, pcaParams=pcaParams))
}

