\name{artificialNeuralNetworkScore}

\alias{score}
\alias{artificialNeuralNetworkScore}
\alias{linearRegressionScore}
\alias{naiveBayesScore}

\title{Scores methods}

\description{These functions allow to calculate scores for a given model. A score is a statistic (one value) used in SPCAVS to help deciding, in a given interaction, what is the best subset of variables that best fits response variable.}

\usage{
linearRegressionScore(x, y = NULL, p1 = NULL, 
                      pcaMethod = "prcomp", 
                      pcaParams = NULL)
                      
artificialNeuralNetworkScore(x, y = NULL, p1 = NULL, 
                             pcaMethod = "prcomp", 
                             pcaParams = NULL)
                             
naiveBayesScore(x, y = NULL, p1 = NULL, 
                pcaMethod = "prcomp", 
                pcaParams = NULL)
}

\arguments{
  \item{x}{Data set with the columns being variables and rows being samples.}
  \item{y}{Response variable represented by a vector. Each element of "y" corresponds to one sample of "x".}
  \item{p1}{A PCA object over all variables in "x". This helps variable selection process to go faster, because we could avoid to calculate PCA for all variables every time. This will be the same result in every iteration.}
  \item{pcaMethod}{The name of the function that perform PCA calculations.}
  \item{pcaParams}{The vector with the parameters of pcaMethod.}
}

\value{It returns a value that indicate how good is the model with selected subset of variables.}

\note{The examples below just show functions working. It doesn't make sense to use the score functions alone. Actually, they are returning the performance of the model as scores. But you can write your own score functions.}

\examples{
  x = as.data.frame(matrix(rnorm(1000), 
                    100, 10, 
                    dimnames=list(NULL, paste('X', 1:10, sep=''))))
                    
  y = rowSums(x) + rnorm(100)

  # $R^2$
  linearRegressionScore(x,y)

  # Sum of Squared Error
  library('RSNNS')
  artificialNeuralNetworkScore(x,y)

  # Hit ratio
  library('e1071')
  naiveBayesScore(x,y)
}
