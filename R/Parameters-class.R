### =========================================================================
### Parameters objects
### -------------------------------------------------------------------------
pars.gen <- setRefClass("Parameters", contains = "VIRTUAL",
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


setAs("Parameters", "list", function(from) {
  nms <- names(from$getRefClass()$fields())
  nms <- setdiff(grep("^[^.]+",nms, value = TRUE), "changed")
  lst <- lapply(nms, function(x){
    do.call("$", c(from, x))
  })
  names(lst) <- nms
  lst
})

##' Encapsulates the parameters for an operation.
##'
##' Each type of object should have a corresponding subclass of Parameters.
##' 
##' @title Parameters class
##' @param prefix Prefix for new subclass of \code{Parameters}, e.g. if prefix is
##' "Graphic", the new subclass name would be \code{GraphicParameters}.
##' @param parameters A list of parameters with the names of parameter
##' and class it belongs to, those parameters are set as signaling fields.
##' @param contains What class does this class extended besides \code{Parameters}.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function
##' (the global environment for ordinary computations, and the
##' environment or namespace of a package in the source code for
##' that package).
##' @param signalName Default name is "changed". A global signal for parameters are
##' defined with this specified name, whichever the parameters changed, this signal
##' will be emitted.
##' @return A reference class generator for subclass.
##' @example objectProperties/inst/examples/setParameters.R
##' @author Tengfei Yin <yintengfei@gmail.com>, Michael Lawrence
setParameters <- function(prefix, parameters,
                          contains = character(),
                          where = topenv(parent.frame()),
                          signalName = "changed"){

  contains = c(contains, "Parameters")
  paramClassName <- paste(prefix, "Parameters", sep = "")
### FIXME: temporary workaround for setRefClass not accepting empty fields list
  if (length(parameters))
    setRefClass(paramClassName,
                fields = signalingFields(parameters, signalName = signalName),
                contains = contains, where = where)
  else
    setRefClass(paramClassName, contains = contains, where = where)
}

setMethod("as.list", "Parameters", function(x,...){
  x <- as(x, "list")
  x
})


