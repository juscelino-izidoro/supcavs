B4 <-
function(pcaObject, max=NULL, lambda=NULL, lockVars=NULL) {

  if(is.null(max) && is.null(lambda)){ stop('You need to provide the maximum number of variables to remove (max)\nor a threshold to filter by eigenvalues (lambda).\n'); }

  if(is.numeric(lambda)){
    pcaEigenvalues = getEigenvaluesFromPCA(pcaObject);
    max = sum(pcaEigenvalues < lambda);
  } else if(!is.numeric(max)){
    stop('You have provided a non-numeric lambda or a non-numeric max parameters.\nPlease, review that values.\n');
  }

  if(!is.null(lambda) && (max < 1)){
    warning('It was estimated "max" through "lambda" parameter. As "max" was less than 1, the process is lost. But, the program will not be broken.\n');
    return(NULL);
  }
  
  pcaLoadMatrix = getLoadingsFromPCA(pcaObject);

  n = dimnames(pcaLoadMatrix)[[1]];

  if(is.numeric(lockVars)){
    lockVars = n[lockVars];
  }
  
  load_row = nrow(pcaLoadMatrix);
  load_col = ncol(pcaLoadMatrix);

  sel = c()
  for(i in max:1){
    j = 1
    m = order(pcaLoadMatrix[,load_col-i+1], decreasing=T)
    m = n[m]
    # If there are locked variables (that cannot be eliminated)
    if(!is.null(lockVars)){
      # Remove them from the sorted selection list of selection
      m = setdiff(m, lockVars)
      # Update dimension 1 (d1) by discounting the number of locked variable
      load_row = length(m) - length(lockVars)
    }
    
    # This loop avoid the selection of the variable already selected
    while((j < load_row) && (sum(sel == m[j]) > 0)){
      j = j + 1
    }
    sel[max-i+1] = m[j]
  }
  
  return(sel);
}

