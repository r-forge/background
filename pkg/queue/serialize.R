serialize.interface <- setRefClass("serialize.interface",
  methods=list(
    serialize <- function() stop("The serialize method has not been implemented")
  )
)

setClass("serialize")
setGeneric("unserialize", function(p) standardGeneric("unserialize"))
setGeneric("serialize", function(x) standardGeneric("serialize"))

superclasses <- function(x, what) {
  if (class(x) != "refObjectGenerator") {
    stop("The object passed is not a reference class")
  }
  meth <- x$methods()
  meth <- meth[grep("#", meth)]
  unique(matrix(unlist(strsplit(meth, "#")), ncol=2, byrow=TRUE)[,2])
}

serialize <- function(x) {
  if ("serialize.interface" %in% superclasses(x)) {
    x$serialize()
  } else {
    stop("The object does not inherit from the serialize.interface")
  }
}
