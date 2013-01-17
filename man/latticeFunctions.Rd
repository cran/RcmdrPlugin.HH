\name{latticeFunctions}
\alias{latticeFunctions}
\alias{latticePanelFunctions}
\alias{splomFormula}
\alias{usualFormula}

\title{Support functions for the Xyplot.HH2 function.}

\description{Support functions for the Xyplot.HH2 function.}

\usage{
latticeFunctions()
latticePanelFunctions()
splomFormula(predictor, data.frame.name)
usualFormula(functionName, response, predictor, data.frame.name)
}

\arguments{
  \item{predictor, data.frame.name, functionName, response}{Arguments to functions.}
}

\value{
  For \code{latticeFunctions} and \code{latticePanelFunctions}, vector
  of function names.
  For \code{splomFormula} and \code{usualFormula}, a model formula
  containing the specified variable names.
  }

\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link{Xyplot.HH}}}
\keyword{hplot}
