\name{getPCAObject}

\alias{getPCAObject}

\title{An interface to get PCA object from different PCA methods}

\description{This function evaluate a string that represents a call to a given PCA function. The function (string) is mounted in a way that data set can be processed by PCA function.}

\usage{
getPCAObject(x, pcaMethod = "prcomp", pcaParams = NULL)
}

\arguments{
  \item{x}{Data set as a data frame. Each column is a variable and each row is a sample.}
  \item{pcaMethod}{The name of PCA function.}
  \item{pcaParams}{The list of parameters to be passed to PCA function.}
}

\details{The parameter pcaParams has a particularity. For example, if name1 need to be an integer (for example, 500), just do c(name1=500). If name1 need to be a name of function, do c(name1='func'). And, if name1 need to be a string, do c(name1='"str"')). This is necessary because of the manner that evaluation is done by R.}

\value{This function returns a PCA object obteined from the evaluation of the string command.}

\examples{
  # Example of simple use of getPCAObject
  x = matrix(rnorm(100), 20,5, 
             dimnames=list(NULL, paste('X', 1:5, sep='')))

  pc = getPCAObject(x, pcaMethod="prcomp", 
                    pcaParams=c(scale=TRUE))
  
  
  # Example of using pca function from pcaMethods, 
  #doing calculations with "ppca" and 4 PCs.
  #library(pcaMethods)
  
  x = matrix(rnorm(100), 20,5, 
             dimnames=list(NULL, paste('X', 1:5, sep='')))
  
  library(pcaMethods)
  pc = getPCAObject(x, pcaMethod='pca', 
                    pcaParams=c(method='"ppca"', nPcs=4))

  getLoadingsFromPCA(pc)
  
  getScoresFromPCA(pc)
}
