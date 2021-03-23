artificialNeuralNetworkScore <-
function(x, y=NULL, p1=NULL, pcaMethod='prcomp', pcaParams=NULL){
  return(-artificialNeuralNetworkModel(x=x, y=y, pcaMethod=pcaMethod, pcaParams=pcaParams))
}

