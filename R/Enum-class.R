setClass("SingleEnum", representation(levels = "character"),
         contains = c("character"))

setClass("MultipleEnum", representation(levels = "character"),
         contains = c("character"))

## setClassUnion("Enum", c("SingleEnum","MultipleEnum"))

##' R functions often have parameters with enumerated values. These are
##' typically passed as a character vector and resolved using
##' match.arg(). The \code{Enum} structure is very similar to that of a factor,
##' except the data is character, not integer and with appropriate validation.
##'
##' The \code{SingleEnum} object is different from simple factor.
##' It validates the value to see if it's in the defined levels during construction.
##' and only the value within defined levels is allowed to be set as current chosen value
##' when it is created as property.
##' It is particularly useful for GUI design, such as creating a drop list or ratio buttons for exclusive choice,
##' you can only choose one item within certain choices at one time.
##'
##' The \code{MultipleEnum} has the same design with \code{SingleEnum}, except
##' it support multiple choices. So for GUI level, it could be used for creating
##' check boxes.
##' 
##' The \code{Enum} class is a Class union for \code{SingleEnum} and \{MultipleEnum}
##' @title Enumerated types
##' @param prefix Prefix for new subclass of \code{SingleEnum} or \code{MultipleEnum},
##' e.g. if prefix is "Geom", the new subclass name would be \code{GeomSingleEnum} after
##' calling \code{setSingleEnum}.
##' @param levels An vector of characters which define the levels for this class.
##' @param contains What class does this class extended besides \code{SingleEnum}.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function.
##' @return \code{setSingleEnum} return a class name for \code{SingleEnum} subclass.
##' \code{setMultipleEnum} return a class name for \code{MultipleEnum} subclass.
##' @seealso \code{\link{setMultipleEnum}}
##' @aliases SingleEnum-class
##' @aliases MultipleEnum-class
##' @aliases setMultipleEnum
##' @rdname  Enum-class
##' @author Tengfei Yin, Michael Lawrence
setSingleEnum <- function(prefix, levels,
                          contains = character(),
                    where = topenv(parent.frame())) {
  setClass(paste(prefix, "SingleEnum", sep = ""),
           prototype = prototype(levels = levels),
           contains = c("SingleEnum", contains),
           validity = function(object) {
             if (!object %in% levels(object))
               paste("value '", object, "' does not belong to level set",
                     paste("\n(", toString(levels(object)),")"),
                     sep = "")
           },
           where = where)
}

setMultipleEnum <- function(prefix, levels,
                            contains = character(),
                            where = topenv(parent.frame())) {
  setClass(paste(prefix, "MultipleEnum", sep = ""),
           prototype = prototype(levels = levels),
           contains = c("MultipleEnum", contains),
           validity = function(object) {
             if (any(!object %in% levels(object)))
               paste("value '", object, "' does not belong to level set",
                     paste("\n(", toString(levels(object)),")"),
                     sep = "")
           },
           where = where)
}



