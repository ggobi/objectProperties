setClass("Enum", contains = "character")

setClass("SingleEnum", representation(levels = "character"),
         contains = "Enum")

setMethod("initialize", "SingleEnum", function(.Object, ...) {
  if (!length(list(...)))
    callNextMethod(.Object, head(levels(.Object), 1))
  else callNextMethod()
})

setClass("MultipleEnum", representation(levels = "character"),
         contains = "Enum")



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

## levels
setGeneric("levels")
setMethod("levels", "Enum", function(x){
  x@levels
})

setReplaceMethod("levels", "Enum", function(x, value){
  x@levels <- value
  if(validObject(x)){
    x
  }
})


setClass("Color", contains = c("character"))
Color <- function(obj){
  new("Color", obj)
}

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




SingleEnum <- function(object, where = topenv(parent.frame())){
  .nm <- as.character(object)
  .nms <- paste(.nm, "factor", sep = "")
  f.gen <- setSingleEnum(.nms, levels = levels(object), where = where)
  f.gen(as.character(object))
}

MultipleEnum <- function(object, where = topenv(parent.frame())){
  .nm <- as.character(object)
  .nms <- paste(.nm, "factor", sep = "")
  f.gen <- setMultipleEnum(.nms, levels = levels(object), where = where)
  f.gen(as.character(object))
}



