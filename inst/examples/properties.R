require(objectProperties)
## Passing fields more than one will create global signal
gplist <- list(size = "numeric",
               color = "character")
GraphPars.gen <- setRefClass("GraphPropertySet",
                             fields = properties(gplist),
                             contains = "PropertySet")
obj <- GraphPars.gen$new(size = 1, color = "red")
class(obj)
## show the properties
obj$properties()
## convert the properties to a list
as(obj, "list")
as.list(obj)
## register global signals
obj$changed$connect(function(name) {
  cat(name, "changed\n")
})
obj$size <- 2

## If we do not inherit from PropertSet, there is no global signal
gplist <- list(size = "numeric")
GraphPars.gen <- setRefClass("GraphProperties",
                             fields = properties(gplist))
obj <- GraphPars.gen$new(size = 1)
## obj$GraphSignal is not there, but sizeChanged remains
obj$sizeChanged$connect(function(){
  print("size changed")
})
obj$size <- 3

## We can pass default values in 'prototype'
gplist <- list(size = "numeric",
               color = "character")
GraphPars.gen <- setRefClass("GraphPropertySet",
                             fields = properties(gplist,
                               prototype = list(size = 1, color = "red")),
                             contains = "PropertySet")
obj <- GraphPars.gen$new()
obj
