setClass("PositiveInteger", contains = c("integer"),
         validity = function(object) {
           if (object <= 0)
             "values must be > 0"
         })

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

## virtual class NumericWithRange
setClass("NumericWithRange", contains = c("VIRTUAL"),
         representation(min = "numeric",
                       max = "numeric"))

setNumericWithRange <- function(prefix = "Numeric", min, max, where = topenv(parent.frame())){
  cls <- paste(prefix, "With", "Min", min, "Max", max, sep = "")
  setClass(cls, contains = c("NumericWithRange", "numeric"),
           prototype = prototype(min = min, max = max),
           validity = function(object){
             if(object<min | object >max)
               paste("values must be within", min, "and", max)
           }, where = where)
}

## NumericWithMin0Max1
setNumericWithRange("Numeric", min = 0, max = 1)

setClass("IntegerWithRange", contains = c("VIRTUAL"),
         representation(min = "integer",
                        max = "integer"))

setIntegerWithRange <- function(prefix = "Integer", min, max, where = topenv(parent.frame())){
  cls <- paste(prefix, "With", "Min", min, "Max", max, sep = "")
  setClass(cls, contains = c("IntegerWithRange", "integer"),
           prototype = prototype(min = min, max = max),
           validity = function(object){
             if(is.na(object) | !is.integer(object)){
                paste("values must be integer")
             }else if(object<min | object >max){
               paste("values must be within", min, "and", max)
             }
           }, where = where)
}



