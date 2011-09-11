setClass("PositiveInteger", contains = c("integer"),
         validity = function(object) {
           if (object <= 0)
             "values must be > 0"
         })

##' This set of classes define different numerical object with restriction on it.
##' 
##' These special classes could be registered as signaling fields by calling
##' \code{signalingFields} or \code{signalingField}, or using \code{setProperties},
##' so they could be used for GUI design, and changing of the fields automatically
##' validate the current value
##' 
##' The construction of these objects has validation with them, please see the example.
##' \describe{
##'    \item{PositiveInteger(object)}{Construct a \code{PositiveInteger} object}
##'    \item{NonpositiveInteger(object)}{Construct a \code{NonpositiveInteger} object}
##'    \item{NegativeInteger(object)}{Construct a \code{NegativeInteger} object}
##'    \item{NonnegativeInteger(object)}{Construct a \code{NonnegativeInteger} object}
##' }
##' @title Bounded types for properties
##' @param object object to be coerced
##' @examples
##' ## Constructors
##' require(objectProperties)
##' obj <- PositiveInteger(1)
##' obj <- NonnegativeInteger(0)
##' obj <- NegativeInteger(-1)
##' obj <- NonpositiveInteger(0)
##' ## setting as properties
##' filt.gen <- setRefClass("Filter",
##'             properties(list(cutoff = "NonnegativeInteger",
##'             weight = "PositiveInteger")), contains = "PropertySet")
##' ## new property instance
##' obj <- filt.gen$new(cutoff = 0, weight = 1)
##' obj$properties()
##' as.list(obj)
##' ## get the value
##' obj$cutoff
##' ## set the value
##' obj$cutoff <- 30
##' ## the class doesn't change
##' ## if you pass a value which out of boundary, it will throw an error message
##' obj$cutoff
##' class(obj$cutoff)
##' @aliases PositiveInteger
##' @aliases PositiveInteger-class
##' @aliases NonnegativeInteger
##' @aliases NonnegativeInteger-class
##' @aliases NegativeInteger
##' @aliases NegativeInteger-class
##' @aliases NonpositiveInteger
##' @aliases NonpositiveInteger-class
##' @rdname comp
##' @author Tengfei Yin, Michael Lawrence
PositiveInteger <- function(object){
  new("PositiveInteger", object)
}

setClass("NonnegativeInteger", contains = c("integer"),
         validity = function(object) {
           if (object < 0)
             "values must be >= 0"
         })

NonnegativeInteger <- function(object){
  new("NonnegativeInteger", object)
}


setClass("NegativeInteger", contains = c("integer"),
         validity = function(object) {
           if (object >= 0)
             "values must be < 0"
         })

NegativeInteger <- function(object){
  new("NegativeInteger", object)
}

setClass("NonpositiveInteger", contains = c("integer"),
         validity = function(object) {
           if (object > 0)
             "values must be <= 0"
         })

NonpositiveInteger <- function(object){
  new("NonpositiveInteger", object)
}

##' This class creator is used to define a special property for numeric range,
##' which could be used for UI design and could be setted as signaling field,
##' so it will support validation on the input.
##'
##' The purpose of creating such a class genenrator is to define a special
##' range properties which could be set as singaling field, such as \code{Properties}
##' object. Then validation will be turned on automatically to make sure the current
##' value is within the defined range. This is particular useful when you try to
##' design a slider widget of such a property, let's say, a alpha blending slider.
##' @title Define a speicific range object
##' @param prefix Prefix for new class name.
##' @param min Minimal value for this range object.
##' @param max Maximal value for this range object.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function.
##' @return A S4 class name.
##' @aliases NumericWithMin0Max1-class
##' @aliases setNumericWithRange
##' @author Tengfei Yin
##' @example objectProperties/inst/examples/setNumericWithRange.R
setNumericWithRange <- function(prefix, min, max, where = topenv(parent.frame())){
  cls <- paste(prefix, "With", "Min", min, "Max", max, sep = "")
  setClass(cls, contains = "numeric",
           validity = function(object){
             if(object<min | object >max)
               paste("values must be within", min, "and", max)
           }, where = where)
}

## NumericWithMin0Max1
setNumericWithRange("Numeric", min = 0, max = 1)
