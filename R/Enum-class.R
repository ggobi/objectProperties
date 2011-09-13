setClass("SingleEnum", representation(levels = "character"),
         contains = c("character"))

setMethod("initialize", "SingleEnum", function(.Object, ...) {
  if (!length(list(...)))
    callNextMethod(.Object, head(levels(.Object), 1))
  else callNextMethod()
})

setClass("MultipleEnum", representation(levels = "character"),
         contains = c("character"))

setClassUnion("Enum", c("SingleEnum","MultipleEnum"))

##' R functions often have parameters with enumerated values. These are
##' typically passed as a character vector and resolved using
##' match.arg(). The \code{Enum} structure is very similar to that of a factor,
##' except the data is character, not integer and with appropriate validation.
##'
##' The \code{SingleEnum} object is different from simple factor.
##' It validates the value to see if it's in the defined levels during construction.
##' and only the value within defined levels is allowed to be set as current chosen value
##' when it is created as property. It is particularly useful for GUI design,
##' such as creating a drop list or ratio buttons for exclusive choice,
##' you can only choose one item within certain choices at one time.
##' \code{setSingleEnum} will create a S4 subclass for \code{SingleEnum},
##' and return the class name. 
##'
##' The \code{MultipleEnum} has the same design with \code{SingleEnum}, except
##' it support multiple choices. So for GUI level, it could be used for creating
##' check boxes. \code{setMultipleEnum} will create a S4 subclass for \code{MultipleEnum},
##' and return the class name. 
##' 
##' The \code{Enum} class is a Class union for \code{SingleEnum} and \code{MultipleEnum}
##'
##' \code{Color} class is a special character, this properties could be used for creating
##' a widgets which showing a color picker pallete and a text input field, a simple
##' character object will be only treated as simple text in the UI.
##'
##' \code{ColorEnum} class is a VIRTUAL class, which including a set of
##' \code{SingleEnum} subclass, when creating widget based on this property,
##' it should be treated as a special color droplist, instead of showing
##' a droplist of levels of text, it shows a drop list of colors, the levels
##' are treated as color in this class. 
##' \code{setColorEnum} is a convenient class generator function for single enum
##' of \code{ColorEnum} and it return a class name.
##'
##' \code{GlyphEnum} class is a VIRTUAL class, which including a set of
##' \code{SingleEnum} subclass, when creating widget based on this property,
##' it should be treated as a special glyph droplist, instead of showing
##' a droplist of levels of text, it shows a drop list of different glyphs, the levels
##' are treated as glyphs in this class. Different engine genenerate icons for different
##' glyphs, such as different point size, line type, etc.
##' \code{setGlyphEnum} is a convenient class generator function for single enum
##' of \code{GlyphEnum} and it return a class name.
##' @title Enumerated types
##' @param prefix Prefix for new subclass of \code{SingleEnum} or \code{MultipleEnum},
##' e.g. if prefix is "Geom", the new subclass name would be \code{GeomSingleEnum} after
##' calling \code{setSingleEnum}.
##' @param levels An vector of characters which define the levels for this class.
##' @param contains What class does this class extended besides \code{SingleEnum}.
##' @param where the environment in which to store or remove the definition.
##' Defaults to the top-level environment of the calling function.
##' @return \code{setSingleEnum} return a class name for \code{SingleEnum}
##' subclass.\code{setMultipleEnum} return a class name for \code{MultipleEnum} subclass.
##' \code{setColorEnum} return a class name for \code{ColorEnum} subclass which is also
##' a \code{SingleEnum}. \code{setGlyphEnum} return a class name for \code{GlyphEnum}
##' subclass which is also a \code{SingleEnum}.
##' @aliases SingleEnum-class
##' @aliases MultipleEnum-class
##' @aliases setMultipleEnum
##' @aliases setSingleEnum
##' @aliases Enum-class
##' @aliases Color-class
##' @aliases ColorEnum-class
##' @aliases setColorEnum
##' @aliases GlyphEnum-class
##' @aliases setGlyphEnum
##' @rdname  Enum-class
##' @author Tengfei Yin, Michael Lawrence
##' @example objectProperties/inst/examples/Enum.R
setSingleEnum <- function(prefix, levels,
                          contains = character(),
                          where = topenv(parent.frame()))
{
  if (!length(levels))
    stop("'levels' must contain at least one element")
  setClass(paste(prefix, "SingleEnum", sep = ""),
           prototype = prototype(levels = levels),
           contains = c("SingleEnum", contains),
           validity = function(object) {
             if (length(object) != 1L)
               "object must be of length 1"
             else if (!object %in% levels(object))
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

setClass("Color", contains = c("character"))
setClass("ColorEnum", contains = c("VIRTUAL"))
setColorEnum <- function(name, levels = character(), contains = character(),
                         where = topenv(parent.frame())){
  setSingleEnum(name, levels = levels, where = where, contains = c("ColorEnum", contains))
}

setClass("GlyphEnum", contains = c("VIRTUAL"))
setGlyphEnum <- function(name, levels = character(), contains = character(),
                         where = topenv(parent.frame())){
  setSingleEnum(name, levels = levels, where = where, contains = c("GlyphEnum", contains))
}


