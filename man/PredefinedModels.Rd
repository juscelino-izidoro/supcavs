\name{artificialNeuralNetworkModel}

\alias{artificialNeuralNetworkModel}
\alias{linearRegressionModel}
\alias{naiveBayesModel}

\title{Classification and Regression Models}

\description{linearRegressionModel build a multiple linear regression and returns r-squared.
artificialNeuralNetworkModel and naiveBayesModel do classification and return iterative test error and hit ratio, respectively.
}

\usage{
linearRegressionModel(x, y, pcaMethod="prcomp", pcaParams=NULL)
artificialNeuralNetworkModel(x, y, pcaMethod=NULL, pcaParams=NULL)
naiveBayesModel(x, y, pcaMethod=NULL, pcaParams=NULL)
}

\arguments{
  \item{x}{a data frame with explanatory variables obtained from a data set.}
  \item{y}{a vector with response vector}
  \item{pcaMethod}{a string that specify the name of pca function. By default, this is defined as "prcomp"}
  \item{pcaParams}{a vector with param=value elements for pcaMethod.}
}

\details{
\code{linearRegressionModel} executes a multiple linear regression in a way that "y" is regressed by variables in data frame "x".

\code{artificialNeuralNetworkModel} executes a binary pattern recognition for classification task. It uses 66\% of the data to train an ANN and uses 34\% to test the model. On the training, this function uses the variables in "x" (the columns of data.frame) to predict the response variable "y".

\code{naiveBayesModel} executes a binary pattern classification. It uses 66\% of the data to train a NB and uses 34\% to test the model. On the training, this functions uses the variables in "x" as predictors and "y" as the response variable.
}

\value{Each function returns just one value:

\code{linearRegressionModel} returns r-squared coefficient.

\code{artificialNeuralNetworkModel} returns iterative test error.

\code{naiveBayesModel} returns hit ratio.
}

\examples{
x = as.data.frame(matrix(rnorm(1000),
                  100, 10, 
                  dimnames=list(NULL, paste('X', 1:10, sep=''))))
                  
y = rowSums(x) + rnorm(100)

# $R^2$
linearRegressionModel(x,y)

# Sum of Squared Error
library('RSNNS')
artificialNeuralNetworkModel(x,y)

# Hit ratio
library('e1071')
naiveBayesModel(x,y)
}
