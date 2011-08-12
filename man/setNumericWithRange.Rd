\name{setNumericWithRange}
\alias{setNumericWithRange}
\title{Define a speicific range object}
\usage{setNumericWithRange(prefix, min, max, where=topenv(parent.frame()))
}
\description{This class creator is used to define a special property for numeric range,
which could be used for UI design and could be setted as signaling field,
so it will support validation on the input.}
\details{The purpose of creating such a class genenrator is to define a special
range properties which could be set as singaling field, such as \code{Parameters}
object. Then validation will be turned on automatically to make sure the current
value is within the defined range. This is particular useful when you try to
design a slider widget of such a property, let's say, a alpha blending slider.}
\value{A S4 class name.}
\alias{NumericWithMin0Max1-class}
\alias{setNumericWithRange}
\author{Tengfei Yin}
\arguments{\item{prefix}{Prefix for new class name.}
\item{min}{Minimal value for this range object.}
\item{max}{Maximal value for this range object.}
\item{where}{the environment in which to store or remove the definition.
Defaults to the top-level environment of the calling function.}
}
