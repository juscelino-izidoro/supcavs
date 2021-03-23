spcavs <-
function(x, y,  minVar=NULL, maxVar=NULL, lambda=NULL,
                  selectionMethod=B2, supervisionMethod=standardizedRegressionCoefficient, 
                  scoreMethod=linearRegressionScore, lockVars=NULL, discardVars=NULL, 
                  pcaMethod='prcomp', pcaParams=NULL) {
   
  if(is.null(lambda) && is.null(minVar) && is.null(maxVar)){
    stop('There is no minVar, maxVar neither lambda. Please, specify parameters.\n');
  }
  
  if(!is.null(minVar) && (minVar < 2)){
    stop('When using minVar with maxVar, please, specify a number greater or equal to 2,\n in a way to satisfay (maxVar >= minVar).\nWhen using lambda, minVar can also be leaved NULL.\n');
  }
  
  if(!is.null(minVar) && (maxVar < minVar)){
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
  
  # This is used by score methods and when using lambda.
  # When using lambda, p1 is used to determine the maxVar value.
  # To optmize the calculations of score we do it just one time and pass the object to the score function.
  p1 = getPCAObject(x, pcaMethod, pcaParams)
  if(class(p1) == 'pcaRes'){
    x = p1@completeObs
  }
  
  if(!is.null(lambda)){
    if(is.null(minVar)){
      minVar = 2;
    }
    
    if(is.numeric(lambda)){
      pcaEigenvalues = getEigenvaluesFromPCA(p1);
      maxVar = sum(pcaEigenvalues < lambda);
    } else {
      stop('You have provided a non-numeric lambda parameter.\nPlease, review that value.\n');
    }
  }
  
  if(!is.null(lambda) && (maxVar < 1)){
    stop('It was estimated "maxVar" through "lambda" parameter. As "maxVar" was less than 1, the process will be stopped. Specify another lambda. Sorry..\n');
  }
  
  if(maxVar < minVar){
    stop('It is not possible to process the data set with maxVar > minVar.\nIf you are using lambda, please, specify another value.');
  }
  
# ------ Supervision Calculations Block ------

  numVars = ncol(x)
  
  influenceOverY = supervisionMethod(x, y);

  superv = list()
  
  l = 1
  for(k in 1:(maxVar-1)){
    sel = c()
    
    for(i in 1:k){
      j = 1
      m = order(influenceOverY)
      m = n[m]
      
      # If there are locked variables (that cannot be eliminated from dataset)
      if(!is.null(lockVars)){
        # We remove locked variables from the sorted list of selection
        m = setdiff(m, lockVars)
        # Update the number of variables to remove discounting the number of locked variables
        numVars = length(m) - length(lockVars)
      }
      
      while((j < numVars) && (sum(sel == m[j]) > 0)){
        j = j + 1
      }
      
      sel[i] = m[j]
    }
    
    superv[[l]] = sel
    
    l = l + 1
  }

# ------ End of Supervision Block ------

# ------ PCA Selection Block ------

  listVars = list()
  super    = list()
  sel      = NULL;
  response = c();

  for(i in minVar:maxVar){

    melhor_est = -Inf
    melhor_sel = NULL
    melhor_superv = NULL
    
    for(j in 1:(i-1)){
      nVars = i - j
      
      with = setdiff(n, superv[[j]] );
      
      pcaObject = getPCAObject(x[, with ], pcaMethod, pcaParams)
      if(class(p1) == 'pcaRes'){
        x = p1@completeObs
      }
  
      if(is.null(lambda)){
        sel = selectionMethod(pcaObject, max=nVars, lambda=NULL  , lockVars=lockVars)
      } else {
        sel = selectionMethod(pcaObject, max=NULL , lambda=lambda, lockVars=lockVars)
      }

      if(!is.null(sel)){
        with = setdiff(with, sel);
        
        # Here we force to convert x into a data.frame because when remain just one column,
        # R loses the name of the variable. Forcing x as data.frame we can keep the name of variable,
        est = scoreMethod(as.data.frame(x[, with, drop=FALSE]) , y, p1, pcaMethod, pcaParams);

        if(est > melhor_est){
          melhor_est = est
          melhor_sel = sel
          melhor_superv = j
        }
      } else {
        warning('"SelectionMethod" brings us a NULL value in a case of Supervision removes', j ,'variables and SelectionMethod rejects', i, 'by PCA.\nHowever, the process will continue.\n' );
      }
    }
    
    listVars[[i-minVar+1]]   = c(superv[[melhor_superv]], melhor_sel)
    super[[i-minVar+1]]      = length(superv[[melhor_superv]])
    response                 = c(response, melhor_est)
  }

# ------ End of PCA Selection Block ------

  ret = list(x=x, var=listVars, y=y, super=as.numeric(super), score=response, main=paste('(Supervised)', substitute(supervisionMethod), substitute(selectionMethod), substitute(scoreMethod), sep=' + '))
  
  class(ret) = "VarSel"
  
  return(ret)
}

