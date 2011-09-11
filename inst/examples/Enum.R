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
