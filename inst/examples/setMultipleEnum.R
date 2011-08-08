ShapeEnumClassName <- setMultipleEnum("Shape",
                                levels = c("circle", "line", "rectangle"))

ShapeEnumClassName
obj <- new(ShapeEnumClassName, c("circle", "line"))
## this is equivilent to
obj <- new("ShapeMultipleEnum", c("circle", "line"))

obj
obj <- new("ShapeMultipleEnum", "square") # Error message

obj <- "triangle" # doesn't check, because it's not signal field.
obj # it's not SingleEnum object anymore, be careful.
class(obj) # just character

## only set it as signaling field, allow you to assign the value and
## validate it.
library(objectSignals)
par.gen <- setParameters("Graph", list(shape = "ShapeMultipleEnum"))
pars <- par.gen$new(shape = new("ShapeMultipleEnum", c("circle", "line")))
pars$shape
pars$shape <- c("line", "rectangle")
pars$shape
class(pars$shape)# still a MultipleEnum
pars$shape <- c("square", "line") #Error message, because it try to validate the input.

