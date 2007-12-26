\name{AOVModel}
\alias{activeAOVModel}
\alias{ActiveAOVModel}
\alias{activeAOVModelP}
\alias{AOVModelsP}
\alias{checkActiveAOVModel}
\alias{is.AOVModel}
\alias{listAOVModels}
\title{Support functions for Analysis of Variance objects in RcmdrPlugin.HH.}
\description{
  List or identify all \code{"aov"} objects or all \code{"lm"} objects
  with at least one factor in its terms.
}
\usage{
 activeAOVModel(model)
 ActiveAOVModel(name)
 activeAOVModelP(model)
 AOVModelsP(n = 1)
 checkActiveAOVModel()
 is.AOVModel(object)
 listAOVModels(envir = .GlobalEnv, ...)
}
\arguments{
  \item{name}{quoted name.}
  \item{model}{the name of a model, as a character string, or TRUE or FALSE, depending upon the function.}
  \item{n}{the name of a model, as a character string, or TRUE or FALSE, depending upon the function.}
  \item{object}{an object (depends on context).}
  \item{envir}{the environment to be searched; should generally be left at the default.}
  \item{\dots}{other arguments.}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }
\keyword{misc}
