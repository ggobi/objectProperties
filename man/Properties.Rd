\name{setProperties}
\alias{setProperties}
\title{Properties class}
\usage{setProperties(prefix, properties, contains=character(), where=topenv(parent.frame()),
    signalName="changed")
}
\description{Encapsulates the properties for an operation.}
\details{Each type of object should have a corresponding subclass of Properties.
\code{setProperties} is a convenient subclass generator for class
\code{Properties}, user could pass a list of different types of variables
and return a specific \code{Properties} subclass generator, this new defined object
store those properties as signaling fields, so it's able to connect signal to
listen to individual property or the set as a whole. In this way, validation is
enabled when user try to set the properties to new value.

\code{Properties} object has following methods
\describe{
\item{properties()}{Return the defined properties name and class.
You can also coerce the \code{Properties} object to a list by calling
as.list on the object}
}}
\value{A reference class generator for subclass.}
\alias{Properties-class}
\alias{setProperties}
\author{Tengfei Yin, Michael Lawrence}
\arguments{\item{prefix}{Prefix for new subclass of \code{Properties}, e.g. if prefix is
"Graphic", the new subclass name would be \code{GraphicProperties}.}
\item{properties}{A list of properties with the names
and class it belongs to, those properties are set as signaling fields.}
\item{contains}{What class does this class extended besides \code{Properties}.}
\item{where}{the environment in which to store or remove the definition.
Defaults to the top-level environment of the calling function
(the global environment for ordinary computations, and the
environment or namespace of a package in the source code for
that package).}
\item{signalName}{Default name is "changed". A global signal for properties are
defined with this specified name, whichever the properties changed, this signal
will be emitted and the name of trigered field will be captured. Please check the
example.}
}
\examples{require(objectProperties)
gplist <- list(size = "numeric",
               color = "character")
GraphPars.gen <- setProperties("Graph", gplist, signalName = "GraphSignal")
obj <- GraphPars.gen$new(size = 1, color = "red")
class(obj)
## show the properties
obj$properties()
## convert the properties to a list
as(obj, "list")
as.list(obj)
## register global signals
obj$GraphSignal$connect(function(name){
  cat(name, "changed\n")
})
obj$size <- 2}

