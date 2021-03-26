getLoadingsFromPCA <-
function(x){
  if(class(x) == 'prcomp'){
    return(x$rotation)
  }
  else if(class(x) == 'princomp'){
    return(x$loadings)
  }
  else if(class(x) == 'pcaRes'){
    return(x@loadings)
  }
  else if(class(x) == 'acp'){
    return(x$eigen$vectors)
  }
stop("Erro: No loads");
}

