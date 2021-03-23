modelPerformance <-
function(obj, Model=linearRegressionModel, isModelScore=T, pcaMethod='prcomp', pcaParams=NULL){
  test       = list();
  test$x     = matrix(0, length(obj$var), 1);
  test$y     = matrix(0, length(obj$var), 1);
  test$nVars = ncol(obj$x);
  test$nRegs = nrow(obj$x);
  
  if(exists('super', where=obj)){
    test$super = as.matrix(obj$super);
  }
  
  m = Model(as.data.frame(obj$x), obj$y, pcaMethod, pcaParams);
  test$y_0 = m;
  
  if(isModelScore && exists('score', where=obj) && exists('super', where=obj)){
    for(i in 1:length(obj$var)){
      test$x[i]  = length(obj$var[[i]]);
    }
    test$y = obj$score;
  }
  else{
    for(i in 1:length(obj$var)){
      test$x[i]  = length(obj$var[[i]]);
      with = setdiff(dimnames(obj$x)[[2]], obj$var[[i]]);
      test$y[i] = Model(as.data.frame(obj$x[,with]), obj$y, pcaMethod, pcaParams);
    }
  }
  test$main = paste(obj$main, substitute(Model), sep=" + ");
  class(test) = 'VarSelPerformance';
  
  return(test)
}

