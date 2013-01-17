\name{PlotLikertDialog}
\alias{PlotLikertDialog}
\alias{listAllLikertCapable}
\alias{LikertFormula}
\alias{LikertFormulaConstruct}
\alias{varPosnOriginal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rcmdr Menu function to specify a likert plot.}
\description{
  Please see \code{\link[HH]{likert}} for details on the
  \code{plot.likert} and related functions.
}
\usage{
PlotLikertDialog()
listAllLikertCapable(envir = .GlobalEnv, ...)
LikertFormula()
LikertFormulaConstruct(functionName, response, predictor)
varPosnOriginal(variables, type = c("all", "factor", "numeric", "nonfactor",
    "twoLevelFactor"))
}
%- maybe also 'usage' for other objects documented here.
\value{
  For \code{listAllLikertCapable}, a character vector of names of all
  objects that satisfy the search criteria in the specified
  environments.
  See \code{\link[HH]{likert}} for details on what objects are likert capable.

  \code{LikertFormulaConstruct} constructs a model formula for use by
  \code{\link[HH]{plot.likert.formula}} from its
  input arguments.

  \code{varPosnOriginal} is the same as \code{\link[Rcmdr]{varPosn}}
  except that it always keeps the variables in the same order as the
  original data.frame.
}
\arguments{
  \item{envir, \dots}{Arguments to \code{\link[base]{ls}}.}
  \item{functionName, response, predictor}{Arguments to functions.}
  \item{variables, type}{See \code{\link[Rcmdr]{varPosn}} for discussion.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\seealso{\code{\link[HH]{likert}}}
\keyword{hplot}
