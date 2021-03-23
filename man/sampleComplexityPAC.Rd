\name{sampleComplexityPAC}

\alias{sampleComplexityPAC}


\title{Sample Complexity based on PAC Learning Theory}

\description{This calculates the sample complexity for a range of possible models using PAC Learning concept with VC dimension.}

\usage{
sampleComplexityPAC(nVar, maxRem, error = 0.1, signif = 0.05)
}

\arguments{
  \item{nVar}{The number of variable of initial model.}
  \item{maxRem}{The number of variable to remove from the data set that will be used to compose the following models.}
  \item{error}{The error of PAC Learning. The range of this parameter is between 0 and 1.}
  \item{signif}{The significance of PAC Learning. This parameter ranges between 0 and 1.}
}

\details{This function calculates the sample complexity for models (classifiers or regressions) that have "nVar", "nVar-1", "nVar-2", ..., "n_var-maxRem" input variables. This sample complexity is calculated using PAC Learning concept, considering some error and significance.}

\value{The returned value is a vector with one sample complexity (as element) for each possible model.}

\examples{
  sampleComplexityPAC(10, 5)
  
  sampleComplexityPAC(30, 25, .05, .01)
}
