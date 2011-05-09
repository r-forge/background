pickle.interface <- setRefClass("pickle.interface",
  methods=list(
    pickle <- function() stop("The pickle method has not been implemented")
  )
)

setClass("pickle")
setGeneric("unpickle", function(p) standardGeneric("unpickle"))
setGeneric("pickle", function(x) standardGeneric("unpickle"))

superclasses <- function(x, what) {
  if (class(x) != "refObjectGenerator") {
    stop("The object passed is not a reference class")
  }
  meth <- x$methods()
  meth <- meth[grep("#", meth)]
  unique(matrix(unlist(strsplit(meth, "#")), ncol=2, byrow=TRUE)[,2])
}

pickle <- function(x) {
  if ("pickle.interface" %in% superclasses(x)) {
    x$pickle()
  } else {
    stop("The object does not inherit from the pickle.interface")
  }
}
