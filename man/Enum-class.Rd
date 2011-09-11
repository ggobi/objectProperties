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
when it is created as property. It is particularly useful for GUI design,
such as creating a drop list or ratio buttons for exclusive choice,
you can only choose one item within certain choices at one time.
\code{setSingleEnum} will create a S4 subclass for \code{SingleEnum},
and return the class name. 

The \code{MultipleEnum} has the same design with \code{SingleEnum}, except
it support multiple choices. So for GUI level, it could be used for creating
check boxes. \code{setMultipleEnum} will create a S4 subclass for \code{MultipleEnum},
and return the class name. 

The \code{Enum} class is a Class union for \code{SingleEnum} and \code{MultipleEnum}

\code{Color} class is a special character, this properties could be used for creating
a widgets which showing a color picker pallete and a text input field, a simple
character object will be only treated as simple text in the UI.

\code{ColorEnum} class is a VIRTUAL class, which including a set of
\code{SingleEnum} subclass, when creating widget based on this property,
it should be treated as a special color droplist, instead of showing
a droplist of levels of text, it shows a drop list of colors, the levels
are treated as color in this class. 
\code{setColorEnum} is a convenient class generator function for single enum
of \code{ColorEnum} and it return a class name.

\code{GlyphEnum} class is a VIRTUAL class, which including a set of
\code{SingleEnum} subclass, when creating widget based on this property,
it should be treated as a special glyph droplist, instead of showing
a droplist of levels of text, it shows a drop list of different glyphs, the levels
are treated as glyphs in this class. Different engine genenerate icons for different
glyphs, such as different point size, line type, etc.
\code{setGlyphEnum} is a convenient class generator function for single enum
of \code{GlyphEnum} and it return a class name.}
\value{\code{setSingleEnum} return a class name for \code{SingleEnum}
subclass.\code{setMultipleEnum} return a class name for \code{MultipleEnum} subclass.
\code{setColorEnum} return a class name for \code{ColorEnum} subclass which is also
a \code{SingleEnum}. \code{setGlyphEnum} return a class name for \code{GlyphEnum}
subclass which is also a \code{SingleEnum}.}
\alias{SingleEnum-class}
\alias{MultipleEnum-class}
\alias{setMultipleEnum}
\alias{setSingleEnum}
\alias{Enum-class}
\alias{Color-class}
\alias{ColorEnum-class}
\alias{setColorEnum}
\alias{GlyphEnum-class}
\alias{setGlyphEnum}
\author{Tengfei Yin, Michael Lawrence}
\arguments{\item{prefix}{Prefix for new subclass of \code{SingleEnum} or \code{MultipleEnum},
e.g. if prefix is "Geom", the new subclass name would be \code{GeomSingleEnum} after
calling \code{setSingleEnum}.}
\item{levels}{An vector of characters which define the levels for this class.}
\item{contains}{What class does this class extended besides \code{SingleEnum}.}
\item{where}{the environment in which to store or remove the definition.
Defaults to the top-level environment of the calling function.}
}
\examples{
## ----------------------------------------------------------------------
##                   setSingleEnum
## ----------------------------------------------------------------------
require(objectProperties)
ShapeEnumClassName <- setSingleEnum("Shape",
                                levels = c("circle", "line", "rectangle"))

ShapeEnumClassName
obj <- new(ShapeEnumClassName, "circle")
## this is equivilent to
obj <- new("ShapeSingleEnum", "circle")
obj

## Error message
## obj <- new("ShapeSingleEnum", "square")

obj <- "triangle" # doesn't check, because it's not signal field.
obj # it's not SingleEnum object anymore, be careful.
class(obj) # just character

## only set it as signaling field, allow you to assign the value and
## validate it.
par.gen <- setRefClass("Graph", properties(list(shape = "ShapeSingleEnum")))
pars <- par.gen$new(shape = new("ShapeSingleEnum", "circle"))
pars$shape
pars$shape <- "line"
pars$shape
class(pars$shape)# still a SingleEnum
## pars$shape <- "square" ## Error it try to validate 
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
## new("ShapeMultipleEnum", "square") # Error message

obj <- "triangle" # doesn't check, because it's not signal field.
obj # it's not SingleEnum object anymore, be careful.
class(obj) # just character

## only set it as signaling field, allow you to assign the value and
## validate it.
par.gen <- setRefClass("Graph",
                       properties(list(shape = "ShapeMultipleEnum")))
pars <- par.gen$new(shape = new("ShapeMultipleEnum", c("circle", "line")))
pars$shape
pars$shape <- c("line", "rectangle")
pars$shape
class(pars$shape)# still a MultipleEnum
## pars$shape <- c("square", "line")
#Error message, because it try to validate the input.

## Color Single Enum
bgColorSingleEnum <- setColorEnum("bgColor", levels = c("black", "white", "gray"))
obj <- new(bgColorSingleEnum, "black")
obj
## Glyph Single Enum
PointSizeSingleEnum <- setGlyphEnum("PointSize", levels = c("1", "2", "5", "10"), contains = "GlyphEnum")
obj <- new(PointSizeSingleEnum, "1")
obj
}

