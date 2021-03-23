naiveBayesScore <-
function(x, y=NULL, p1=NULL, pcaMethod='prcomp', pcaParams=NULL){
  return(naiveBayesModel(x=x, y=y, pcaMethod=pcaMethod, pcaParams=pcaParams))
}

