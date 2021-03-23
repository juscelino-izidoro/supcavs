getPCAObject <-
function(x, pcaMethod='prcomp', pcaParams=NULL){
  run = paste(pcaMethod, '(x', sep='') ;
  
  if(!is.null(pcaParams)){
    for(i in names(pcaParams)){
      run = paste(run, ',', i, '=', pcaParams[i], sep='')
    }
  }

  run = paste(run, ')', sep='')

  return(eval(parse(text=run)));
}

