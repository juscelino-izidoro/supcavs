\name{standardizedRegressionCoefficient}

\alias{supervision}
\alias{standardizedRegressionCoefficient}
\alias{pearsonCoefficient}
\alias{spearmanCoefficient}
\alias{kendallCoefficient}

\title{Supervision coefficients}

\description{These functions calculate coefficients that measure the influence of each predictor over the response variable to help in the supervision processes inside variable selection procedure.}

\usage{
standardizedRegressionCoefficient(x, y)
pearsonCoefficient(x, y)
spearmanCoefficient(x, y)
kendallCoefficient(x, y)
}

\arguments{
  \item{x}{A matrix with data. Each row is a sample and each column is a variable.}
  \item{y}{A vector with data corresponding to each sample of x. This is the response variable.}
}

\value{Each function returns just one value:

\code{standardizedRegressionCoefficient} returns Standardized Regression Coefficient.

\code{pearsonCoefficient} returns Pearson Coefficient.

\code{spearmanCoefficient} returns Spearman Coefficient.

\code{kendallCoefficient} returns Kendall Coefficient.
}
