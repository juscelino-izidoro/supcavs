print.VarSel <-
function(x, ...){
  cat(paste(" Methods:", x$main, "(No. of registers = ", nrow(x$x), ")\n\n"))
  xnames = dimnames(x$x)[[2]]
  n = length(x$var)
  RemovedVariables = vector('integer', n)
  SelectedVariables   = vector('integer', n)
  RemovedVariablesNames = c()
  SelectedVariablesNames   = c()
  for(i in 1:n){ RemovedVariables[i] = length(x$var[[i]]) }
  SelectedVariables = ncol(x$x)-RemovedVariables
  
  for(i in 1:n){ 
    RemovedVariablesNames = c(RemovedVariablesNames,paste(x$var[[i]], collapse=',')) ;
    SelectedVariablesNames   = c(SelectedVariablesNames, paste(setdiff(xnames, x$var[[i]]), collapse=',')) ;
  }
  
  if(exists('super', where=x)){
    Super = vector('integer', n)
    
    for(i in 1:n){ Super[i] = x$super[i] } 
    
    print(data.frame(NumVarRem=RemovedVariables, 
                     VarsRems=RemovedVariablesNames,
                     Superv=Super, 
                     NumVarSel=SelectedVariables,
                     VarsSel=SelectedVariablesNames
                    ),
          row.names=F
    )
  }
  else{
    print(data.frame(NumVarRem=RemovedVariables, 
                     VarsRems=RemovedVariablesNames, 
                     NumVarSek=SelectedVariables,
                     VarsSel=SelectedVariablesNames
                    ), 
          row.names=F
    )
  }
  cat("\n")
}

