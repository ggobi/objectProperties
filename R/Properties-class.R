### =========================================================================
### Properties objects
### -------------------------------------------------------------------------
setRefClass("Properties", contains = "VIRTUAL",
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

##' Encapsulates the properties for an operation.
##'
##' Each type of object should have a corresponding subclass of
##' Properties.  \code{setProperties} is a convenient subclass
##' generator for class \code{Properties}. The user may pass a named
##' list of classes, in the same form as for the fields of
##' \code{\link[methods]{setRefClass}}.  This function returns a
##' specific \code{Properties} subclass generator, instances of which
##' store those properties. Whenever a property is modified, a signal,
##' named in the form \code{[property]Changed}, will be emitted . If
##' any property is modified, the signal named according to
##' \code{signalName} is emitted. 
##' 
##' \code{Properties} object has following methods
##' \describe{
##'  \item{properties()}{Return the defined properties name and class.
##' You can also coerce the \code{Properties} object to a list by calling
##' as.list on the object}
##' }
##' @title Properties class
##' @param prefix Prefix for new subclass of \code{Properties},
##' e.g. if prefix is "Graphic", the new subclass name would be
##' \code{GraphicProperties}.
##' @param properties A list of properties with the names
##' and class it belongs to, those properties are set as signaling fields.
##' @param contains What class does this class extended besides
##' \code{Properties}.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function
##' (the global environment for ordinary computations, and the
##' environment or namespace of a package in the source code for
##' that package).
##' @param signalName Default name is "changed". A global signal for
##' properties are defined with this specified name, whichever the
##' properties changed, this signal will be emitted and the name of
##' trigered field will be captured. Please check the example.
##' @param suffix The class name suffix. By convention, this is "Properties".
##' @return A reference class generator for subclass.
##' @example objectProperties/inst/examples/setProperties.R
##' @aliases Properties-class
##' @aliases setProperties
##' @author Tengfei Yin, Michael Lawrence
setProperties <- function(prefix, properties,
                          contains = character(),
                          signalName = "changed",
                          suffix = "Properties",
                          where = topenv(parent.frame()))
{
  contains = c(contains, "Properties")
  paramClassName <- paste(prefix, suffix, sep = "")
  fields <- signalingFields(properties, signalName = signalName)
  setRefClass(paramClassName, fields = fields, contains = contains,
              where = where)
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

setAs("Properties", "list", function(from) {
  nms <- from$properties()
  lst <- lapply(names(nms), function(x){
    from$field(x)
  })
  names(lst) <- names(nms)
  lst
})

