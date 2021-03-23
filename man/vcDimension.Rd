\name{vcDimensionLinearRegression}
\alias{vcDimensionLinearRegression}
\alias{vcDimensionNaiveBayes}
\alias{vcDimensionArtificialNeuralNetwork}

\title{Vapnik-Chervonenkis Dimension}

\description{These functions allow to calculate Vapnik-Chervonenkis Dimension that is used in Probably Approximately Correct (PAC) Learning to get the sample complexity of a given model.}

\usage{
vcDimensionLinearRegression(d)
vcDimensionArtificialNeuralNetwork(d)
vcDimensionNaiveBayes(d)
}

\arguments{
  \item{d}{The number of dimensions (variables) in the model.}
}

\details{
\code{vcDimensionLinearRegression} allows to calculate VC dimension for Multiple Linear Model.

\code{vcDimensionArtificialNeuralNetwork} allows to calculate VC dimension for ANN, restricting that the neural network must have just one hidden layer with the number of hidden neurons equal to the number of input neurons in the input layer. This consider a ANN with just one output neuron in the output layer.

\code{vcDimensionNaiveBayes} allows to calculate VC dimension for Naive Bayes classifier. This function consider a NB to classify two classes.
}

\value{This functions returns a "float" number that represents VC-Dimension.}
