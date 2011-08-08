ShapeEnumClassName <- setSingleEnum("Shape",
                                levels = c("circle", "line", "rectangle"))

ShapeEnumClassName
obj <- new(ShapeEnumClassName, "circle")
## this is equivilent to
obj <- new("ShapeSingleEnum", "circle")
obj
obj <- new("ShapeSingleEnum", "square") # Error message

obj <- "triangle" # doesn't check, because it's not signal field.
obj # it's not SingleEnum object anymore, be careful.
class(obj) # just character

## only set it as signaling field, allow you to assign the value and
## validate it.
library(objectSignals)
par.gen <- setParameters("Graph", list(shape = "ShapeSingleEnum"))
pars <- par.gen$new(shape = new("ShapeSingleEnum", "circle"))
pars$shape
pars$shape <- "line"
pars$shape
class(pars$shape)# still a SingleEnum
pars$shape <- "square" #Error it try to validate the input.
pars$shape <- "line" # works
