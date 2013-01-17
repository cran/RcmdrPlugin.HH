\name{bestSubsetsRegressionModel.HH}
\alias{bestSubsetsRegressionModel.HH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rcmdr interface to the regsubsets function in the leaps package.}
\description{
  Menu interface to the Best Subsets Regression function.
  Selection boxes allow one response variables and one or more predictor
  variables.  All subsets are calculated.  Only the best $k$, where $k$
  is menu item, are displayed.  A graph displaying one of the following
  statistics ($R^2$, residual sum of squares, adjusted $R^2$, $C_p$,
  BIC, $s$) is displayed.  The model with highest adjusted $R^2$ is made the
  active model and its \code{summary} is displayed.
}
\usage{
bestSubsetsRegressionModel.HH()
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[leaps]{regsubsets}}}
\keyword{hplot}
\keyword{regression}
