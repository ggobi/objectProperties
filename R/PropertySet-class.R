### =========================================================================
### PropertySet objects
### -------------------------------------------------------------------------

## Conceptual crisis

## A property is a special field that supports:
## - default value
## - changed signal
## - automatic coercion

## Do we want to make any of those features optional? A default value
## is optional, but signals are always emitted and coercion always
## occurs. This is in-line with GTK+ and is probably OK for now.

## Our initial motivation for properties was that there would be a
## data model that was completely defined by a set of properties. This
## is the role played by PropertySet. However, it is also useful to
## define properties within a larger class. In that case, how to
## retrieve the set of properties (as separate from the fields)? Is it
## needed? Good question. The only time we have seen a need for
## introspecting properties has been when generating interfaces for
## e.g. GGobiTransform parameters or ProxyGRanges parameters. Cranvas
## would probably use them for a BrushParameters class. So probably not.

## Signal thoughts: We currently have a global signal 'changed(name)',
## as well as individual signals '[name]Changed'. This is convenient,
## because the user can listen to single property changes, or all
## changes. However, it is complicated, because it requires us to
## synchronize both signals. Otherwise, if something has changed in
## the underlying data of a dynamic data model, the class will need to
## emit both signals.

## Here is another thought: do we want to define properties as a group
## or individually? This is more or less signalingField()
## vs. signalingFields(). Compare their syntax:

## setRefClass("ClassWithProps", property("A", "character", default = "foo"),
##             property("B", "integer", default = 0L))
## vs:
## setRefClass("ClassWithProps",
##             properties(list(A = "character", B = "integer"),
##                        prototype = list(A = "foo", B = 0L))

## Probably like the second one best. It is consistent with setClass()
## and makes the prototype more obvious. And it's just cleaner.

## Is there still a need for signalingField[s], given properties()?
## The functionality would be largely redundant, so no.

## Should the global signal be defined in PropertySet, rather than via
## properties()? The weird thing about a global signal is that it
## pertains to a "set" of properties, even though there is no formal
## "set" when not using PropertySet. We should probably move it.

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
##' @param prototype A list of values declaring a default value for a field.
##' @param signalName Default name is "changed". A global signal for
##' properties are defined with this specified name, whichever the
##' properties changed, this signal will be emitted and the name of
##' trigered field will be captured. Please check the example.
##' @return A list that is easily concatenated into the field list
##' @author Michael Lawrence, Tengfei Yin
##' @example objectProperties/inst/examples/properties.R
##' @export
properties <- function(fields, prototype = list(), signalName = "changed")
{
  if (!length(fields))
    return(list())
  .fieldNames <- paste(".", names(fields), sep = "")
  .initNames <- paste(".init.", names(fields), sep = "")
  hasPrototype <- names(fields) %in% names(prototype)
  if (any(sapply(fields, is.function) & hasPrototype))
    stop("An active binding field cannot have a prototype")
  activeFields <- mapply(function(fieldClass, fieldName, .fieldName, initName,
                                  hasPrototype, prototype, thisSignal)
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
          coercedVal <- try(as(val, fieldClass, strict = FALSE), silent = TRUE)
          if (inherits(coercedVal, "try-error"))
            stop("Cannot set an object of type '", class(val), "' on '",
                 fieldName, "', a property of type '", fieldClass, "'")
          else val <- coercedVal
        }
        ## careful here; if field is active binding, it might not change
        oldVal <- .fieldName
        .fieldName <<- val
        if (hasPrototype) initName <<- TRUE
        if (!identical(oldVal, .fieldName)) {
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
            signalName = as.name(signalName)))))
  }, fields, names(fields), .fieldNames, .initNames, hasPrototype,
     prototype[names(fields)], paste(names(fields), "Changed", sep = ""))
  indSigs <- lapply(names(fields), function(nm) {
    nm <- paste(nm, "Changed", sep = "")
    fieldWithPrototype(nm, "Signal", Signal())
  })
  c(activeFields, structure(fields, names = .fieldNames),
    structure(rep("logical", sum(hasPrototype)),
              names = .initNames[hasPrototype]),
    fieldWithPrototype(signalName, "Signal", Signal(name)),
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
