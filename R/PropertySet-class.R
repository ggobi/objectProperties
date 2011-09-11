### =========================================================================
### PropertySet objects
### -------------------------------------------------------------------------
##' The \code{PropertySet} class is a collection of properties. Useful
##' for grouping properties within a class, e.g., for storing the
##' parameters of some operation.
##'
##' \code{PropertySet} object has following methods \describe{
##' \item{properties()}{Return the defined properties name and class.
##' You can also coerce the \code{PropertySet} object to a list by
##' calling as.list on the object}
##' }
##' @name PropertySet-class
##' @title PropertySet-class
##' @author Michael Lawrence, Tengfei Yin
##' @rdname PropertySet-class
setRefClass("PropertySet", contains = "VIRTUAL",
            methods = list(
              properties = function() {
                fieldClasses <- getRefClass()$fields()
                fieldNames <- names(fieldClasses)
                fieldNames <- grep("^\\.init\\.",
                                   grep("^\\.", fieldNames, value = TRUE),
                                   invert = TRUE, value = TRUE)
                fieldClasses <- fieldClasses[fieldNames]
                names(fieldClasses) <- sub("^\\.", "", fieldNames)
                fieldClasses[!fieldClasses %in% "Signal"]
              }))


##' Convenience function for defining a set of reference class fields
##' that signals when set.
##' 
##' When constructing signaling fields in this way, each field has the
##' ability to register its own signal and at the same time, there is
##' one top level signal which could be emitted no matter which field
##' changes. Please see the example to learn to register global signal
##' and individual signal.
##' @title Properties signaling fileds 
##' @param fields list of names of the field and associated fields class 
##' @param prototype 
##' @param signalName Default name is "changed". A global signal for
##' properties are defined with this specified name, whichever the
##' properties changed, this signal will be emitted and the name of
##' trigered field will be captured. Please check the example.
##' @param globalSignal Logical value. Default is based on how many
##' fields passed, if it's more than one, then set to TRUE and create
##' a global signal specified by signalName. If FALSE, this will not
##' create any global signal.
##' @return A list that is easily concatenated into the field list
##' @author Michael Lawrence, Tengfei Yin
##' @example objectProperties/inst/examples/properties.R
##' @export
properties <- function(fields, prototype = list(), signalName = "changed",
                       globalSignal = length(fields) >1)
{
  if (!length(fields))
    return(list())
  .fieldNames <- paste(".", names(fields), sep = "")
  .initNames <- paste(".init.", names(fields), sep = "")
  hasPrototype <- names(fields) %in% names(prototype)
  if (any(sapply(fields, is.function) & hasPrototype))
    stop("An active binding field cannot have a prototype")
  activeFields <- mapply(function(fieldClass, fieldName, .fieldName, initName,
                                  hasPrototype, prototype, thisSignal,
                                  globalSignal)
  {
    as.function(c(alist(val=), substitute({
      if (missing(val)) {
        if (hasPrototype && !length(initName)) {
          .fieldName <<- prototype
          initName <<- TRUE
        }
        .fieldName
      } else {
        if (!is.function(fieldClass)) {
          if (!is(val, fieldClass))
            stop("Cannot set an object of type '", class(val), "' on '",
                 fieldName, "', a field of type '", fieldClass, "'")
          else val <- as(val, fieldClass, strict = FALSE)
        }
        ## careful here; if field is active binding, it might not change
        oldVal <- .fieldName
        .fieldName <<- val
        if (hasPrototype) initName <<- TRUE
        if (!identical(oldVal, .fieldName)) {
          if(globalSignal)
            signalName$emit(fieldName)
          thisSignal$emit()
        }
      }
    }, list(.fieldName = as.name(.fieldName),
            fieldClass = fieldClass, fieldName = fieldName,
            thisSignal = as.name(thisSignal),
            initName = as.name(initName),
            hasPrototype = hasPrototype,
            prototype = prototype,
            signalName = as.name(signalName),
            globalSignal = globalSignal))))
  }, fields, names(fields), .fieldNames, .initNames, hasPrototype,
     prototype[names(fields)], paste(names(fields), "Changed", sep = ""),
                         globalSignal)
  indSigs <- lapply(names(fields), function(nm) {
    nm <- paste(nm, "Changed", sep = "")
    fieldWithPrototype(nm, "Signal", Signal())
  })
  if(globalSignal)
    c(activeFields, structure(fields, names = .fieldNames),
      structure(rep("logical", sum(hasPrototype)),
                names = .initNames[hasPrototype]),
      fieldWithPrototype(signalName, "Signal", Signal(name)),
      unlist(indSigs))
  else
    c(activeFields, structure(fields, names = .fieldNames),
      structure(rep("logical", sum(hasPrototype)),
                names = .initNames[hasPrototype]),
      unlist(indSigs))
}

##' Coercion from \code{PropertySet} to \code{list}.
##'
##' This coersion only return a list of properties instances. 
##' filtering out singal function and other fields which are
##' not properties.
##' @title Coercion to \code{list}
##' @param x A \code{PropertySet} object.
##' @return A list of properties instance.
##' @author Tengfei Yin
##' @docType methods
##' @rdname as.list-methods
##' @aliases as.list
##' @aliases as.list,PropertySet-method
##' @aliases show,PropertySet-method
##' @examples
##' filt.gen <- setRefClass("Filter", properties(list(cutoff = "NonnegativeInteger",
##'                                                 weight = "PositiveInteger")),
##'                            contains = "PropertySet")
##' obj <- filt.gen$new(cutoff = NonnegativeInteger(0),
##'                     weight = PositiveInteger(1))
##' obj$properties()
##' as.list(obj)
setMethod("as.list", "PropertySet", function(x) {
  x <- as(x, "list")
  x
})

setAs("PropertySet", "list", function(from) {
  nms <- from$properties()
  lst <- lapply(names(nms), function(x){
    from$field(x)
  })
  names(lst) <- names(nms)
  lst
})

setMethod("show", "PropertySet", function(object) {
  show(as.list(object))
})
