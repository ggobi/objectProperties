### =========================================================================
### Parameters objects
### -------------------------------------------------------------------------
pars.gen <- setRefClass("Properties", contains = "VIRTUAL",
                        methods = list(
                          parameters = function(){
                            flds <- getRefClass()$fields()
                            idx <- !(flds %in%
                                     c("activeBindingFunction",
                                       "Signal","function",
                                     "functionORNULL"))
                            flds <- flds[idx]
                            idx <- !grepl("^\\.init.", names(flds))
                            flds <- flds[idx]
                            cls <- as.character(flds)
                            valnames <- gsub("\\.","",names(flds))
                            names(cls) <- valnames
                            cls
                          }))

setAs("Properties", "list", function(from) {
  nms <- names(from$getRefClass()$fields())
  nms <- setdiff(grep("^[^.]+",nms, value = TRUE), "changed")
  lst <- lapply(nms, function(x){
    do.call("$", c(from, x))
  })
  names(lst) <- nms
  lst
})

##' Encapsulates the properties for an operation.
##'
##' Each type of object should have a corresponding subclass of Properties.
##' 
##' @title Properties class
##' @param prefix Prefix for new subclass of \code{Properties}, e.g. if prefix is
##' "Graphic", the new subclass name would be \code{GraphicProperties}.
##' @param properties A list of properties with the names of parameter
##' and class it belongs to, those properties are set as signaling fields.
##' @param contains What class does this class extended besides \code{Properties}.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function
##' (the global environment for ordinary computations, and the
##' environment or namespace of a package in the source code for
##' that package).
##' @param signalName Default name is "changed". A global signal for properties are
##' defined with this specified name, whichever the properties changed, this signal
##' will be emitted.
##' @return A reference class generator for subclass.
##' @aliases Properties-class
##' @author Tengfei Yin, Michael Lawrence
setProperties <- function(prefix, properties,
                          contains = character(),
                          where = topenv(parent.frame()),
                          signalName = "changed"){

  contains = c(contains, "Properties")
  paramClassName <- paste(prefix, "Properties", sep = "")
### FIXME: temporary workaround for setRefClass not accepting empty fields list
  if (length(properties))
    setRefClass(paramClassName,
                fields = signalingFields(properties, signalName = signalName),
                contains = contains, where = where)
  else
    setRefClass(paramClassName, contains = contains, where = where)
}

## setMethod("as.list", "Properties", function(x){
##   x <- as(x, "list")
##   x
## })


