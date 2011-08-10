\name{Enum-class}
\alias{setSingleEnum}
\title{Enumerated types}
\usage{setSingleEnum(prefix, levels, contains=character(), where=topenv(parent.frame()))
}
\description{R functions often have parameters with enumerated values. These are
typically passed as a character vector and resolved using
match.arg(). The \code{Enum} structure is very similar to that of a factor,
except the data is character, not integer and with appropriate validation.}
\details{The \code{SingleEnum} object is different from simple factor.
It validates the value to see if it's in the defined levels during construction.
and only the value within defined levels is allowed to be set as current chosen value
when it is created as property.
It is particularly useful for GUI design, such as creating a drop list or ratio buttons for exclusive choice,
you can only choose one item within certain choices at one time.

The \code{MultipleEnum} has the same design with \code{SingleEnum}, except
it support multiple choices. So for GUI level, it could be used for creating
check boxes.

The \code{Enum} class is a Class union for \code{SingleEnum} and \code{MultipleEnum}}
\value{\code{setSingleEnum} return a class name for \code{SingleEnum}
subclass.\code{setMultipleEnum} return a class name for \code{MultipleEnum} subclass.}
\alias{SingleEnum-class}
\alias{MultipleEnum-class}
\alias{setMultipleEnum}
\alias{setSingleEnum}
\alias{Enum-class}
\author{Tengfei Yin, Michael Lawrence}
\arguments{\item{prefix}{Prefix for new subclass of \code{SingleEnum} or \code{MultipleEnum},
e.g. if prefix is "Geom", the new subclass name would be \code{GeomSingleEnum} after
calling \code{setSingleEnum}.}
\item{levels}{An vector of characters which define the levels for this class.}
\item{contains}{What class does this class extended besides \code{SingleEnum}.}
\item{where}{the environment in which to store or remove the definition.
Defaults to the top-level environment of the calling function.}
}
\examples{## ----------------------------------------------------------------------
##                   setSingleEnum
## ----------------------------------------------------------------------
library(objectProperties)
ShapeEnumClassName <- setSingleEnum("Shape",
                                levels = c("circle", "line", "rectangle"))

ShapeEnumClassName
obj <- new(ShapeEnumClassName, "circle")
## this is equivilent to
obj <- new("ShapeSingleEnum", "circle")
obj

## Error message
err <- try(obj <- new("ShapeSingleEnum", "square"), silent = TRUE)
print(err)

obj <- "triangle" # doesn't check, because it's not signal field.
obj # it's not SingleEnum object anymore, be careful.
class(obj) # just character

## only set it as signaling field, allow you to assign the value and
## validate it.
par.gen <- setProperties("Graph", list(shape = "ShapeSingleEnum"))
pars <- par.gen$new(shape = new("ShapeSingleEnum", "circle"))
pars$shape
pars$shape <- "line"
pars$shape
class(pars$shape)# still a SingleEnum
err <- try(pars$shape <- "square", silent = TRUE) ## Error it try to validate the input.
print(err)
pars$shape <- "line" # works

## ----------------------------------------------------------------------
##                   setMultipleEnum
## ----------------------------------------------------------------------
ShapeEnumClassName <- setMultipleEnum("Shape",
                                levels = c("circle", "line", "rectangle"))

ShapeEnumClassName
obj <- new(ShapeEnumClassName, c("circle", "line"))
## this is equivilent to
obj <- new("ShapeMultipleEnum", c("circle", "line"))

obj
err <- try(obj <- new("ShapeMultipleEnum", "square"), silent = TRUE) # Error message
print(err)

obj <- "triangle" # doesn't check, because it's not signal field.
obj # it's not SingleEnum object anymore, be careful.
class(obj) # just character

## only set it as signaling field, allow you to assign the value and
## validate it.
par.gen <- setProperties("Graph", list(shape = "ShapeMultipleEnum"))
pars <- par.gen$new(shape = new("ShapeMultipleEnum", c("circle", "line")))
pars$shape
pars$shape <- c("line", "rectangle")
pars$shape
class(pars$shape)# still a MultipleEnum
err <- try(pars$shape <- c("square", "line"), silent = TRUE) #Error message, because it try to validate the input.
print(err)}

