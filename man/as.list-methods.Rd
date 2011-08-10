\name{as.list-methods}
\alias{as.list,Properties-method}
\alias{as.list}
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
\examples{## setting as properties
filt.gen <- setProperties("Filter", list(cutoff = "NonnegativeInteger",
weight = "PositiveInteger"))
## new property instance
obj <- filt.gen$new(cutoff = 0, weight = 1)
obj$properties()
as.list(obj)}

