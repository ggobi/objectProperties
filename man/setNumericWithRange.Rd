\name{setNumericWithRange}
\alias{setNumericWithRange}
\alias{NumericWithRange-class}
\title{Define a speicific range object}
\usage{setNumericWithRange(prefix = "Numeric", min, max, where=topenv(parent.frame()))
}
\description{This class creator is used to define a special property for numeric range,
which could be used for UI design and could be setted as signaling field,
so it will support validation on the input.}
\details{The purpose of creating such a class genenrator is to define a special
range properties which could be set as singaling field, such as \code{Properties}
object. Then validation will be turned on automatically to make sure the current
value is within the defined range. This is particular useful when you try to
design a slider widget of such a property, let's say, a alpha blending slider.}
\value{A S4 class name in R(< 2.15) and a generator function in R(>= 2.15)}
\alias{NumericWithMin0Max1-class}
\alias{setNumericWithRange}
\author{Tengfei Yin}
\arguments{\item{prefix}{Prefix for new class name.Default is "Numeric"}
\item{min}{Minimal value for this range object.}
\item{max}{Maximal value for this range object.}
\item{where}{the environment in which to store or remove the definition.
  Defaults to the top-level environment of the calling function.}
}
\examples{
num1to100.gen <- setNumericWithRange(min = 1, max = 100)
par.gen <- setRefClass("Graph",
                       properties(list(size = "NumericWithMin1Max100")))
pars <- par.gen$new(size = new("NumericWithMin1Max100", 5))
pars$size #current value is 5
try(pars$size <- 300) # out of range error
pars$size <- 10 #works

## Positive Integer
par.gen <- setRefClass("PI", properties(list(size  = "PositiveInteger"),
                                        list(size = PositiveInteger(2))))
obj <- par.gen$new()
## error
try(obj$size <- -1)
obj$size <- 3
}


