.supcavsEnv <- new.env()

supcavsenv <- function(){ as.environment('.supcavsEnv'); }

print.supcavsenv <- function(){
  print("<environment: supcavsEnv>")
}

