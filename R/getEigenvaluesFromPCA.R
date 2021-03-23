getEigenvaluesFromPCA <-
function(x){
  if(class(x) == 'prcomp'){
    return(x$sdev^2)
  }
  else if(class(x) == 'princomp'){
    return(x$sdev^2)
  }
  else if(class(x) == 'pcaRes'){
    return(x@sDev^2)
  }
  else if(class(x) == 'acp'){
    return(x$explica)
  }
stop("Erro: No eigenvalues");
}

