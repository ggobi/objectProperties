### =========================================================================
### Parameters objects
### -------------------------------------------------------------------------
setRefClass("Properties", contains = "VIRTUAL",
            methods = list(
              properties = function(){
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
  nms <- from$properties()
  lst <- lapply(names(nms), function(x){
    from$field(x)
  })
  names(lst) <- names(nms)
  lst
})

##' Encapsulates the properties for an operation.
##'
##' Each type of object should have a corresponding subclass of Properties.
##' \code{setProperties} is a convenient subclass generator for class
##' \code{Properties}, user could pass a list of different types of variables
##' and return a specific \code{Properties} subclass generator, this new defined object
##' store those properties as signaling fields, so it's able to connect signal to
##' listen to individual property or the set as a whole. In this way, validation is
##' enabled when user try to set the properties to new value.
##' 
##' \code{Properties} object has following methods
##' \describe{
##'  \item{properties()}{Return the defined properties name and class.
##' You can also coerce the \code{Properties} object to a list by calling
##' as.list on the object}
##' }
##' @title Properties class
##' @param prefix Prefix for new subclass of \code{Properties}, e.g. if prefix is
##' "Graphic", the new subclass name would be \code{GraphicProperties}.
##' @param properties A list of properties with the names
##' and class it belongs to, those properties are set as signaling fields.
##' @param contains What class does this class extended besides \code{Properties}.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function
##' (the global environment for ordinary computations, and the
##' environment or namespace of a package in the source code for
##' that package).
##' @param signalName Default name is "changed". A global signal for properties are
##' defined with this specified name, whichever the properties changed, this signal
##' will be emitted and the name of trigered field will be captured. Please check the
##' example.
##' @return A reference class generator for subclass.
##' @example objectProperties/inst/examples/setProperties.R
##' @aliases Properties-class
##' @aliases setProperties
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

##' Coercion from \code{Properties} to \code{list}.
##'
##' This coersion only return a list of properties instances. 
##' filtering out singal function and other fields which are
##' not properties.
##' @title Coercion to \code{list}
##' @param x A \code{Properties} object.
##' @return A list of properties instance.
##' @author Tengfei Yin
##' @docType methods
##' @rdname as.list-methods
##' @examples
##' ## setting as properties
##' filt.gen <- setProperties("Filter", list(cutoff = "NonnegativeInteger",
##' weight = "PositiveInteger"))
##' ## new property instance
##' obj <- filt.gen$new(cutoff = 0, weight = 1)
##' obj$properties()
##' as.list(obj)
setMethod("as.list", "Properties", function(x){
  x <- as(x, "list")
  x
})


