getScoresFromPCA <-
function(x){
  if(class(x) == 'prcomp'){
    return(x$x)
  }
  else if(class(x) == 'princomp'){
    return(x$scores)
  }
  else if(class(x) == 'pcaRes'){
    return(x@scores)
  }
  else if(class(x) == 'acp'){
    return(x$escores)
  }
stop("Erro: Sem Scores");
}

