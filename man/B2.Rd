\name{B2}

\alias{B2}
\alias{B4}

\title{PCA variable selection criteria.}

\description{These functions execute a PCA variable selection over a data set and return the group of eliminated variable.}

\usage{
B2(pcaObject, max=NULL, lambda=NULL, lockVars = NULL)
B4(pcaObject, max=NULL, lambda=NULL, lockVars = NULL)
}

\arguments{
  \item{pcaObject}{A object obtained from some PCA technique.}
  \item{max}{The maximum number of variables to eliminate from data set.}
  \item{lambda}{The threshold to filter some principal componentes to proceed variable selection.}
  \item{lockVars}{The name of the variables to be locked in the selection (those variables must not be eliminated in the process, neither in the model).}
}

\details{According to Jolliffe (1972), this method analyses the load matrix from PCA calculations, starting from the last PC (principal component) to k-th PC. For each PC, a variable with the largest load is choosen. If this variable is already been choosen or locked (it means, the name of the variable is in "lockVars" list), then, algorithm chooses the next one with the second largest load. And so forth, until the algorithm achieves the maximum number of variables to remove or "pcaLoadMatrix" has no more variables to be removed.

If "lambda" is specified, then, it takes preference and "max" parameter is discarded. So, "max" is assigned from "lambda" according to Jollife (1972) criterion.
}

\value{This function returns one group with "max" elements being the variable names that can be discarded from the data set.}

\references{Jolliffe, I. T. (1972) Discarding Variables in a Principal Component Analysis. I: Artificial Data. Journal of the Royal Statistical Society. Series C (Applied Statistics). 21, 160 - 173.}

\examples{
  x = matrix(rnorm(100), 20,5, 
             dimnames=list(NULL, paste('X', 1:5, sep='')))

  pcaObject = prcomp(x)

  B2(pcaObject, max=3)
  B4(pcaObject, max=3)

  B2(pcaObject, lambda=.5)
  B4(pcaObject, lambda=.5)
}
