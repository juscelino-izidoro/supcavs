\name{getEigenvaluesFromPCA}

\alias{getEigenvaluesFromPCA}
\alias{getLoadingsFromPCA}
\alias{getScoresFromPCA}

\title{An interface to get load/score matrix or eigenvalues from different PCA methods}

\description{As we have a lot of packages with a lot of PCA functions available, we need to establish an interface between the PCA functions and Variable Selection process. For this, we have two functions: getLoadingsFromPCA and getScoresFromPCA. They let us to get the load and score, respectively, from any PCA function of any package. If you are interested in a PCA function of another package not used in the SUPCAVS package, you can rewrite these functions for your needs.}

\usage{
getLoadingsFromPCA(x)
getScoresFromPCA(x)
getEigenvaluesFromPCA(x)
}

\arguments{
  \item{x}{This is a PCA object.}
}

\details{The PCA object can come from prcomp or princomp functions. It can also come from pcaMethods library.}

\value{The returned value is:

\code{getLoadingsFromPCA}: returns a matrix of loads from PCA.

\code{getScoresFromPCA}: returns a score matrix (data set transformed by PCA transformations).

\code{getEigenvalueFromPCA}: returns a eigenvalue vector.
}

\examples{
  x = matrix(rnorm(100), 20,5, 
             dimnames=list(NULL, paste('X', 1:5, sep='')))

  pc = prcomp(x)
  
  pcaLoadMatrix = getLoadingsFromPCA(pc)
  pcaScoreMatrix = getScoresFromPCA(pc)
  pcaEigenvalues = getEigenvaluesFromPCA(pc)
}
