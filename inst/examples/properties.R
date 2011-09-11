require(objectProperties)
gplist <- list(size = "numeric",
               color = "character")
GraphPars.gen <- setProperties("Graph", gplist, signalName = "GraphSignal")
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
