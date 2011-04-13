
bg <- function(expr, par.gen=options()$background.backend) {
  if (is.null(options()$background.backend)) {
    warning("No backend specified, sequential will be used")
    options(background.backend="sequential.process")
  }
  process <- eval(parse(text=paste(par.gen, '$new()', sep='')))
  process$task(substitute(expr))
  process
}

fg <- function( process ) {
  process$value()
}

done <- function( process ) {
  process$done()
}

kill <- function( process ) {
  process$kill()
}

killed <- function( process ) {
  process$killed()
}

backend <- function( backendName=NULL ) {
  if (is.null(backendName)) {
    if (is.null(options()$background.backend)) {
      stop("No backend has been specified")
    } else {
      return(options()$background.backend)
    }
  }
  # For right now I can only check to see if the backendName exists.
  # It would be nice to see if it inherits from process.interface.
  if (!exists(backendName)) {
    stop(paste("The reference class", backendName, "could not be found"))
  } else {
    options(background.backend=backendName)
  }
  return(TRUE)
}

superclasses <- function(x, what) {
  if (class(x) != "refObjectGenerator") {
    stop("The object passed is not a reference class")
  }
  meth <- x$methods()
  meth <- meth[grep("#", meth)]
  unique(matrix(unlist(strsplit(meth, "#")), ncol=2, byrow=TRUE)[,2])
}

# The following function is almost the same as the parallel (and mcparallel)
# functions from the multicore library except that it has an extra
# field to specify the evinronment.
#mcpar <- function (expr, env, name, mc.set.seed = FALSE, silent = FALSE)
#{
#    f <- multicore::fork()
#    if (inherits(f, "masterProcess")) {
#        on.exit(multicore::exit(1, structure("fatal error in wrapper code",
#            class = "try-error")))
#        if (isTRUE(mc.set.seed))
#            set.seed(Sys.getpid())
#        if (isTRUE(silent))
#            multicore:::closeStdout()
#        multicore::sendMaster(try(eval(expr, env), silent = TRUE))
#        multicore::exit(0)
#    }
#    if (!missing(name) && !is.null(name))
#        f$name <- as.character(name)[1]
#    class(f) <- c("parallelJob", class(f))
#    f
#}

