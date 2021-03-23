pcavs <-
function(x, y, minVar=NULL, maxVar=NULL, lambda=NULL, selectionMethod=B2, lockVars=NULL, discardVars=NULL, pcaMethod='prcomp', pcaParams=NULL) {

  if(is.null(lambda) && is.null(minVar) && is.null(maxVar)){
    stop('There is no minVar, maxVar neither lambda. Please, specify parameters.\n');
  }
  
  if(!is.null(minVar) && (minVar < 1)){
    stop('When using minVar with maxVar, please, specify a number greater or equal to 1,\n in a way to satisfay (maxVar >= minVar).\nWhen using lambda, minVar can also be leaved NULL.\n');
  }
  
  if(!is.null(maxVar) && (maxVar < minVar)){
    stop('Please, verify that maxVar < minVar. Change the values.\n');
  }

  n = dimnames(x)[[2]]

  if(is.numeric(lockVars)){
    lockVars = n[lockVars]
  }

  if(!is.null(discardVars)){
    if(is.character(discardVars)){
      n = setdiff(n, discardVars)
    }
    else{
      n = setdiff(n, n[discardVars])
    }
    
    x = x[,n]
  }
  
  pcaObject = getPCAObject(x, pcaMethod, pcaParams)
  if(class(pcaObject) == 'pcaRes'){
    x = pcaObject@completeObs
  }

  listVars = list()

  l = 1

  if(is.null(lambda)){
    for(k in minVar:maxVar){
      listVars[[l]] = selectionMethod(pcaObject, max=k, lockVars=lockVars);
      l = l + 1
    }
  } else{
    if(is.null(minVar)){
      minVar = 1;
    }
    
    listVars[[l]] = selectionMethod(pcaObject, max=NULL, lambda=lambda, lockVars=lockVars);
  }
  
  ret = list(x=x, var=listVars, y=y, main=paste('Unsupervised',substitute(selectionMethod)))
  class(ret) = 'VarSel'
  return(ret)

}

