\name{spcavs}

\alias{spcavs}
\alias{pcavs}

\title{Supervised and unsupervised PCA variable selection}

\description{\code{pcavs} is a function to select variable in an unsupervised manner, using PCA technique. \code{spcavs} is a function to select variable in a supervised manner, that combines a kind of supervision with PCA technique.}

\usage{
pcavs(x, y, minVar=NULL, maxVar=NULL, lambda=NULL, selectionMethod = B2, 
      lockVars = NULL, discardVars = NULL, 
      pcaMethod = "prcomp", pcaParams = NULL)

spcavs(x, y, minVar=NULL, maxVar=NULL, lambda=NULL, selectionMethod = B2, 
       supervisionMethod = standardizedRegressionCoefficient, 
       scoreMethod = linearRegressionScore, lockVars = NULL, 
       discardVars = NULL, pcaMethod = "prcomp", 
       pcaParams = NULL)
}
\arguments{
  \item{x}{A data frame with rows being samples and columns being variables.}
  \item{y}{A vector with response variable values. Each element is a sample (paired with the rows of x). This is used in performance test model and is also used, particularly in spcavs, to help in variable selection as a pre filter.}
  \item{minVar}{The minimum number of variables to be removed from the data set.}
  \item{maxVar}{The maximum number of variables to be removed from the data set.}
  \item{lambda}{The threshold to filter some principal componentes to proceed variable selection.}
  \item{selectionMethod}{The function which must be used to select variables from x. This parameter specify a function that use PCA calculations to return a vector with the name of removed variables (B2 or B4 implemented in this package)}
  \item{supervisionMethod}{(Just for spcavs) The function that would be used to supervise the selection.}
  \item{scoreMethod}{(Just for spcavs) The function that return a vector of each variable with its respective coefficient.}
  \item{lockVars}{The name of variables that need to be considered in the final model and cannot be discarded by the PCA variable selection, neither by supervision.}
  \item{discardVars}{The name of variables that need to be discarded in all the process of variable selection. This parameter helps the user to cut off those variables that must be ignored in dataset and cannot apear in the final model.}
  \item{pcaMethod}{The name of PCA function that do the PCA calculations (SVD, Probabilistic PCA, Bayesian PCA, and so forth.}
  \item{pcaParams}{The vector in the form c(name1=value1, name2=value2, ...) that contains the parameters used by pcaMethod.}
}

\details{
  An attention is given for "minVar", "maxVar" and "lambda". The user can use "minVar" with "maxVar". The parameter "maxVar" needs to be greater or equal to "minVar". 
  Both minVar and maxVar need to be at least 2 in case of using "spcavs". In case of using "pcavs", minVar and maxVar can be 1.
  The user can decide to use "lambda", difined as eigenvalues threshold as a parameter for selection. In this case, "minVar" and "maxVar" are leaved NULL and "lambda" receives a number.
  Also, when using "lambda", user can specify "minVar". This way, the procedures will use the specified values.
  
  Note, when using "lambda", "maxVar" is NULL. To "spcavs" works, it needs a "maxVar" to do the pre filter (supervision).
  In this case, is calculated a PCA over original data set (with all variables of the data set) and used their eigenvalues to estimate "maxVar".
  Note, also, that "spcavs" will pass forward "lambda" value to the SelectionMethod (B2 or B4). This, will modify the search space and can contribute positively or not for selection results.
}

\value{The return of the function pcavs and spcavs are an object of type VarSel, which has the following properties:
\item{x}{The data set, without variables in the discardVars vector}
\item{var}{The list with group of eliminated variables in each interaction of variable selection criterion.}
\item{y}{The response variable. This is used in the performance test model.}
\item{super}{(Just for spcavs) The vector that has in each element, the number of discarded variables in supervision process. Looking at a vector with the names of the removed variables, the first N variables (with N=super) was removed by supervision. The other was removed by PCA.}
\item{score}{(Just for spcavs) If the performance measure (statistic) is used as score in spcavs, set this option to TRUE. Otherwise, if the score is not the same as model performance statistic, set this option FALSE. But, if "score" is TRUE, the performance test model is executed faster.}
\item{main}{The title what will apear in print or plot reports.}
}

\seealso{ B2, B4, supervision, score }

\examples{
  x = as.data.frame(matrix(rnorm(1000), 
                    100, 10, 
                    dimnames=list(NULL, paste('X', 1:10, sep=''))))
                    
  y = rowSums(x) + rnorm(100);
  
  pcavs(x, y, 1, 5)
  
  pcavs(x, y, 1, 5, selectionMethod=B4)
 
  pcavs(x, y, lambda=.6)
  
  pcavs(x, y, lambda=.6, selectionMethod=B4)
  
  library(pcaMethods)
  pcavs(x, y, 1, 3, pcaMethod='pca', 
                    pcaParams=(c(method="'ppca'", nPcs=5)))
  
  spcavs(x, y, 2, 5)
  
  spcavs(x, y, 2, 5, selectionMethod=B4)
 
  spcavs(x, y, lambda=.6)
  
  spcavs(x, y, lambda=.6, selectionMethod=B4)
  
  spcavs(x, y, 2, 5, selectionMethod=B4, supervisionMethod=spearmanCoefficient)
  
  spcavs(x, y, 2, 5, pcaMethod='princomp', pcaParams=c(scale=TRUE))

}
