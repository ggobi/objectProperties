\name{comp}
\alias{PositiveInteger}
\title{Bounded types for properties}
\usage{PositiveInteger(object)
}
\description{This set of classes define different numerical object with restriction on it.}
\details{These special classes could be registered as signaling fields by calling
\code{signalingFields} or \code{signalingField}, or using \code{setProperties},
so they could be used for GUI design, and changing of the fields automatically
validate the current value

The construction of these objects has validation with them, please see the example.
\describe{
\item{PositiveInteger(object)}{Construct a \code{PositiveInteger} object}
\item{NonpositiveInteger(object)}{Construct a \code{NonpositiveInteger} object}
\item{NegativeInteger(object)}{Construct a \code{NegativeInteger} object}
\item{NonnegativeInteger(object)}{Construct a \code{NonnegativeInteger} object}
}}
\alias{PositiveInteger}
\alias{PositiveInteger-class}
\alias{NonnegativeInteger}
\alias{NonnegativeInteger-class}
\alias{NegativeInteger}
\alias{NegativeInteger-class}
\alias{NonpositiveInteger}
\alias{NonpositiveInteger-class}
\author{Tengfei Yin, Michael Lawrence}
\arguments{\item{object}{object to be coerced}
}
\examples{## Constructors
require(objectProperties)
obj <- PositiveInteger(1)
obj <- NonnegativeInteger(0)
obj <- NegativeInteger(-1)
obj <- NonpositiveInteger(0)
## setting as properties
filt.gen <- setRefClass("Filter",
properties(list(cutoff = "NonnegativeInteger",
weight = "PositiveInteger")), contains = "PropertySet")
## new property instance
obj <- filt.gen$new(cutoff = 0, weight = 1)
obj$properties()
as.list(obj)
## get the value
obj$cutoff
## set the value
obj$cutoff <- 30
## the class doesn't change
## if you pass a value which out of boundary, it will throw an error message
obj$cutoff
class(obj$cutoff)}

