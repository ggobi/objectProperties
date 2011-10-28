\name{PropertySet-class}
\alias{PropertySet-class}
\alias{PropertySet-class}
\title{PropertySet-class}
\description{The \code{PropertySet} class is a collection of properties and is
useful as a data model, e.g., for storing the parameters of some
operation.

\code{setPropertySet} is a simple wrapper around \code{\link[methods]{setRefClass}} for
creating subclasses of \code{\linkS4class{PropertySet}}. It
ensures that all fields of the subclass are defined via
\code{\link{properties}}.
}
\details{\code{PropertySet-class}: \code{PropertySet} object has following methods, where \code{x} is
a \code{PropertySet} object:

\describe{
\item{}{\code{x$properties()}: Return the classes of the properties as a
named character vector. Compare to the \code{fields} method on
\link[methods:setRefClass]{a reference class generator}}.
\item{}{\code{as.list(x)}: Returns a named list of the property values.}
}

When any property in the set changes, the \code{changed(name)}
signal is emitted, where \code{name} is the name of the property
that changed.

}
\alias{setPropertySet}
\author{Michael Lawrence, Tengfei Yin}
\examples{filt.gen <- setRefClass("Filter", properties(fields = list(cutoff = "numeric",
                                                           weight = "numeric"),
                                  prototype = list(cutoff = 0, weight = 1)),
                                  contains = "PropertySet")
obj <- filt.gen$new()
obj
obj$properties()
as.list(obj)
obj$changed$connect(function(name) print(name))
obj$cutoffChanged$connect(function() print(paste("change to", obj$cutoff)))
obj$cutoff <- 0
obj$cutoff <- 2
obj$weight <- 3


## use setPropertySet, the same thing as above
filt.gen <- setPropertySet("Filter", fields = list(cutoff = "numeric",
                                         weight = "numeric"),
                           prototype = list(cutoff = 0, weight = 1))

obj <- filt.gen$new()
obj
obj$properties()
as.list(obj)
obj$changed$connect(function(name) print(name))
obj$cutoffChanged$connect(function() print(paste("change to", obj$cutoff)))
obj$cutoff <- 0
obj$cutoff <- 2
obj$weight <- 3}
\alias{setPropertySet}
\usage{setPropertySet(Class, fields=list(), prototype=list(), contains="PropertySet", ...,
    where=topenv(parent.frame()))
}
\value{\code{setPropertySet}: the class generator object

}
\arguments{\item{Class}{class name}
\item{fields}{list of fields}
\item{prototype}{list of default values, as in
\code{\link[methods]{setClass}}.}
\item{contains}{superclasses, one of which must extend PropertySet}
\item{...}{additional arguments to \code{setRefClass}}
\item{where}{the environment in which to define the class}
}

