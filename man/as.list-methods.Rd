\name{as.list-methods}
\alias{as.list,PropertySet-method}
\alias{as.list}
\alias{show,PropertySet-method}
\title{Coercion to \code{list}}
\description{Coercion from \code{Properties} to \code{list}.}
\details{This coersion only return a list of properties instances. 
filtering out singal function and other fields which are
not properties.}
\value{A list of properties instance.}
\author{Tengfei Yin}
\docType{methods}
\arguments{\item{x}{A \code{Properties} object.}
}
\examples{
filt.gen <- setRefClass("Filter", properties(list(cutoff = "NonnegativeInteger",
                                                weight = "PositiveInteger")),
                                                       contains = "PropertySet")
obj <- filt.gen$new(cutoff = NonnegativeInteger(0),
                    weight = PositiveInteger(1))
obj$properties()
as.list(obj)
}

