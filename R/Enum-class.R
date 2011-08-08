setClass("SingleEnum", representation(levels = "character"),
         contains = c("character"))

setClass("MultipleEnum", representation(levels = "character"),
         contains = c("character"))

setClassUnion("Enum", c("SingleEnum","MultipleEnum"))

## R functions often have parameters with enumerated values. These are
## typically passed as a character vector and resolved using
## match.arg(). SingleEnum is a base class for representing specific
## enumerations with subclasses. The structure is very similar to that
## of a factor, except the data is character, not integer.  It is a
## little odd to store the levels in the instance, but it is
## expedient.


##' R functions often have parameters with enumerated values. These are
##' typically passed as a character vector and resolved using
##' match.arg(). SingleEnum is a base class for representing specific
##' enumerations with several levels but only has one current choosed value.
##' The structure is very similar to that of a factor, except the data
##' is character, not integer.
##'
##' The \code{SingleEnum} object is different from simple factor in two ways. Firstly,
##' it validate the value to see if it's in the defined levels when you define it
##' as signaling field. Secondly, for \code{SingleEnum}, only the value within
##' defined levels is allowed to be assigned . This will be
##' checked during the construction of
##' a new object. What's more, it is particularly useful for GUI design,
##' such as creating a droplist, you can only choose one item within certain choices
##' at one time.Please see the examples to check this differece.
##' 
##' @title Define a subclass of SingalEnum.
##' @param prefix Prefix for new subclass of \code{SingleEnum}, e.g. if prefix is
##' "Geom", the new subclass name would be \code{GeomSingleEnum}.
##' @param levels An vector of characters whic define the levels of this enum object.
##' @param contains What class does this class extended besides \code{SingleEnum}.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function.
##' @return S4 class name for this subclass of \code{SingleEnum}.
##' @example objectProperties/inst/examples/setSingleEnum.R
##' @author Michael Lawrence, Tengfei Yin <yintengfei@gmail.com>
setSingleEnum <- function(prefix, levels,
                          contains = character(),
                    where = topenv(parent.frame())) {
  setClass(paste(prefix, "SingleEnum", sep = ""),
           prototype = prototype(levels = levels),
           contains = c("SingleEnum", contains),
           validity = function(object) {
             if (!object %in% levels(object))
               paste("value '", object, "' does not belong to level set",
                     sep = "")
           },
           where = where)
}


##' R functions often have parameters with enumerated values. These are
##' typically passed as a character vector and resolved using
##' match.arg(). MultipleEnum is a base class for representing specific
##' enumerations with several levels which could have multiple choosed value.
##' The structure is very similar to that of a factor, except the data
##' is character, not integer.
##'
##' The \code{MultipleEnum} object is different from simple factor in two ways. Firstly,
##' it validate the value to see if it's in the defined levels when you define it
##' as signaling field. Secondly, for \code{MultipleEnum}, only the value within
##' defined levels are allowed to be assigned . This will be
##' checked during the construction of
##' a new object. What's more, it is particularly useful for GUI design,
##' such as creating a group of radio buttons,
##' you can choose more than one items within certain choices
##' at one time.Please see the examples.
##' 
##' @title Define a subclass of MultipleEnum.
##' @param prefix Prefix for new subclass of \code{MultipleEnum}, e.g. if prefix is
##' "Geom", the new subclass name would be \code{GeomMultipleEnum}.
##' @param levels An vector of characters whic define the levels of this enum object.
##' @param contains What class does this class extended besides \code{MultipleEnum}.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function.
##' @return S4 class name for this subclass of \code{MultipleEnum}.
##' @example objectProperties/inst/examples/setMultipleEnum.R
##' @author Michael Lawrence, Tengfei Yin <yintengfei@gmail.com>
setMultipleEnum <- function(prefix, levels,
                            contains = character(),
                            where = topenv(parent.frame())) {
  setClass(paste(prefix, "MultipleEnum", sep = ""),
           prototype = prototype(levels = levels),
           contains = c("MultipleEnum", contains),
           validity = function(object) {
             if (any(!object %in% levels(object)))
               paste("value '", object, "' does not belong to level set",
                     sep = "")
           },
           where = where)
}

setClass("PositiveInteger", contains = "integer",
         validity = function(object) {
           if (object <= 0)
             "values must be > 0"
         })

setClass("NonnegativeInteger", contains = "integer",
         validity = function(object) {
           if (object < 0)
             "values must be >= 0"
         })


setClass("NegativeInteger", contains = "integer",
         validity = function(object) {
           if (object >= 0)
             "values must be < 0"
         })

setClass("NonpositiveInteger", contains = "integer",
         validity = function(object) {
           if (object > 0)
             "values must be <= 0"
         })

##' This class creator is used to define a special property for numeric range,
##' which could be used for UI design and could be setted as signaling field,
##' so it will support validation on the input.
##'
##' The purpose of creating such a class genenrator is to define a special
##' range properties which could be set as singaling field, such as \code{Parameters}
##' object. Then validation will be turned on automatically to make sure the current
##' value is within the defined range. This is particular useful when you try to
##' design a slider widget of such a property, let's say, a alpha blending slider.
##' 
##' @title Define a speicific range object
##' @param prefix Prefix for new class name.
##' @param min Minimal value for this range object.
##' @param max Maximal value for this range object.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function.
##' @return A S4 class name.
##' @example objectProperties/inst/examples/setNumericWithRange.R
##' @author Tengfei Yin <yintengfei@gmail.com>
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


