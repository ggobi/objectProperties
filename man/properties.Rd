\name{properties}
\alias{properties}
\title{Properties signaling fileds}
\usage{properties(fields, prototype=list(), signalName="changed",
    globalSignal=length(fields) > 1)
}
\description{Convenience function for defining a set of reference class fields
that signals when set.}
\details{When constructing signaling fields in this way, each field has the
ability to register its own signal and at the same time, there is
one top level signal which could be emitted no matter which field
changes. Please see the example to learn to register global signal
and individual signal.}
\value{A list that is easily concatenated into the field list}
\author{Michael Lawrence, Tengfei Yin}
\arguments{\item{fields}{list of names of the field and associated fields class}
\item{prototype}{}
\item{signalName}{Default name is "changed". A global signal for
properties are defined with this specified name, whichever the
properties changed, this signal will be emitted and the name of
trigered field will be captured. Please check the example.}
\item{globalSignal}{Logical value. Default is based on how many
fields passed, if it's more than one, then set to TRUE and create
a global signal specified by signalName. If FALSE, this will not
create any global signal.}
}
\examples{require(objectProperties)
## Passing fields more than one will create global signal
gplist <- list(size = "numeric",
               color = "character")
GraphPars.gen <- setRefClass("GraphPropertySet",
                             fields = properties(gplist,
                               signalName = "GraphSignal"),
                             contains = "PropertySet")
obj <- GraphPars.gen$new(size = 1, color = "red")
class(obj)
## show the properties
obj$properties()
## convert the properties to a list
as(obj, "list")
as.list(obj)
## register global signals
obj$GraphSignal$connect(function(name){
  cat(name, "changed\n")
})
obj$size <- 2
## Passing one field more than one will not create global signal
gplist <- list(size = "numeric")
GraphPars.gen <- setRefClass("GraphPropertySet",
                             fields = properties(gplist,
                               signalName = "GraphSignal"),
                             contains = "PropertySet")
obj <- GraphPars.gen$new(size = 1)
## obj$GraphSignal is not there
obj$sizeChanged$connect(function(){
  print("size changed")
})

obj$size <- 3

## globalSignal set to FALSE
gplist <- list(size = "numeric",
               color = "character")
GraphPars.gen <- setRefClass("GraphPropertySet",
                             fields = properties(gplist,
                               signalName = "GraphSignal",
                               globalSignal = FALSE))
obj <- GraphPars.gen$new(size = 1, color = "red")
## obj$GraphSignal is not there even though we have multiple fields}

