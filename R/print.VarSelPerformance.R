print.VarSelPerformance <-
function(x, ...){
  cat(paste(" Methods:", x$main, "(No. of registers = ", x$nRegs, ")\n\n"))
  RemovedVariables  = c(0,x$x)
  SelectedVariables = c(x$nVars-RemovedVariables)
  Performance       = c(x$y_0,x$y)
  if(exists('super', where=x)){
    RemovedSupervision = rbind(0,x$super)
    print(data.frame(Removed=RemovedVariables, Selected=SelectedVariables, Supervision=RemovedSupervision, Performance=Performance), row.names=F)
  }
  else{
    print(data.frame(Removed=RemovedVariables, Selected=SelectedVariables,  Performance=Performance), row.names=F)
  }
  cat("\n")
}

