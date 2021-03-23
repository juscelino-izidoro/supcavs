\name{modelPerformance}

\alias{modelPerformance}

\title{Calculate the performance of models composed by different groups of variables}

\description{When is selected some groups of variables with pcavs and spcavs, is interesting to test if those groups are able to produce good results over a model. This function lets to iterate some groups of selected variables (inside the object produced by pcavs and spcavs) testing them over a model and givin the performance of the models dispite each subset of variables.}

\usage{
modelPerformance(obj, Model = linearRegressionModel, 
                 isModelScore = T, pcaMethod = "prcomp", 
                 pcaParams = NULL)
}

\arguments{
  \item{obj}{An object of type VarSel, that have data about variable selection.}
  \item{Model}{The function that allows automatically to build a model from a data set.}
  \item{isModelScore}{This flag indicates if the performance was used as a score to select variables. It works only for supervised procedure SPCAVS.}
  \item{pcaMethod}{The name of PCA function.}
  \item{pcaParams}{The parameters for PCA function in the form c(var1=value1, var2=value2, ...).}
}

\details{\code{modelPerformance} does a test of performance over any group of selected variables contained in obj.
obj$var is a list and this contains N vectors. Each of this vectors contains the names of discarded variables. So, modelPerformance iterate each vector of discarded variables doing the intersection between the whole set of available variables in data set and the set of discarding variables. In this way, modelPerformance knows what is the set of selected variables.

Using "Model", that is a function that performs the construction and the test of a desired model, the procedure modelPerformance executes the model and get one performance measure (statistic) for each group of selected variable.
}

\value{A object of type VarSelPerformance with:
\item{x}{The replication of the data set what was used to select variables from.}
\item{y_0}{The performance of the model with all the prediction variables availables in the data set.}
\item{y}{A vector in which each element is the performance of the respective model that came from a group of selected variables.}
\item{nVars}{The number of predictor variables in the original data set.}
\item{nRegs}{The number of samples in the data set.}
\item{main}{The title that is used to label the plot.}
}

\examples{
  x = as.data.frame(matrix(rnorm(1000), 100, 10, 
                    dimnames=list(NULL, paste('X', 1:10, sep=''))))
                    
  y = rowSums(x) + rnorm(100);
  
  s = pcavs(x, y, 1, 5)
  
  p = modelPerformance(s)
  
  p
}
