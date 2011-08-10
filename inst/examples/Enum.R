## ----------------------------------------------------------------------
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
print(err)

