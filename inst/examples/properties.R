require(objectProperties)
## Passing fields more than one will create global signal
gplist <- list(size = "numeric",
               color = "character")
GraphPars.gen <- setRefClass("GraphPropertySet",
                             fields = properties(gplist,
                               signalName = "GraphSignal"),
                             contains = "PropertySet")
obj <- GraphPars.gen$new(size = 1, color = "red")
class(obj)
## show the properties
obj$properties()
## convert the properties to a list
as(obj, "list")
as.list(obj)
## register global signals
obj$GraphSignal$connect(function(name){
  cat(name, "changed\n")
})
obj$size <- 2
## Passing one field more than one will not create global signal
gplist <- list(size = "numeric")
GraphPars.gen <- setRefClass("GraphPropertySet",
                             fields = properties(gplist,
                               signalName = "GraphSignal"),
                             contains = "PropertySet")
obj <- GraphPars.gen$new(size = 1)
## obj$GraphSignal is not there
obj$sizeChanged$connect(function(){
  print("size changed")
})

obj$size <- 3

## globalSignal set to FALSE
gplist <- list(size = "numeric",
               color = "character")
GraphPars.gen <- setRefClass("GraphPropertySet",
                             fields = properties(gplist,
                               signalName = "GraphSignal",
                               globalSignal = FALSE))
obj <- GraphPars.gen$new(size = 1, color = "red")
## obj$GraphSignal is not there even though we have multiple fields
