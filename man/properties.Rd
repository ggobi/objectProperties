\name{properties}
\alias{properties}
\title{Properties signaling fileds}
\usage{properties(fields=list(), prototype=list())
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
\item{prototype}{A list of values declaring a default value for a field.}
}
\examples{## we could pass prototype as in S4
GPars.gen <- setRefClass("GraphicProperties",
                fields = properties(fields = list(size = "numeric",
                                                  color = "character"),
                                    prototype = list(size =1,
                                                     color = "red")))

obj <- GPars.gen$new()
## since it's not PropertySet, no global signal
## let's register individual signal
obj$sizeChanged$connect(function(){
  print("size changed")
})
## emit signal
obj$size <- 3
## no signal
obj$color <- "black"}

