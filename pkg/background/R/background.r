
process.interface <- setRefClass("process.interface",
  fields = list(.evaluated="logical", .has.task="logical", .expr="ANY", 
    .env="environment", .val="ANY", .killed="logical"),

  methods=list(

    inherits.from.process.interface=function() TRUE,

    initialize=function() {
      .killed <<- FALSE
      .evaluated <<- FALSE
      .self
    },

    copy=function(shallow) stop("Background objects cannot be deep copied."),

    task=function() stop("The task method has not been implemented."),

    done=function() stop("The done method has not been implemented."),

    value=function() stop("The value method has not been implemented."),

    kill=function() stop("The kill method has not been implemented."),

    check.killed=function() {
      if (.killed) {
        stop("This background object has been killed")
      }
    },

    killed=function() .killed

  )
)
#assign("process.interface", process.interface, .GlobalEnv)

sequential.process <- setRefClass("sequential.process",
  contains="process.interface",

  methods = list(

    initialize=function() {
      .has.task <<- FALSE
      callSuper()
      .self
    },

    task=function(evalExpr, evalEnv=parent.frame()) {
      .self$check.killed()
      if (.has.task)
        stop("This background object has already has a task")
      .env <<- evalEnv
      .expr <<- evalExpr
      print(class(evalExpr))
      .has.task <<- TRUE
    },

    done=function() {
      .self$check.killed()
      TRUE
    },

    value=function() {
      .self$check.killed()
      if (!.evaluated) {
        .val <<- eval(.expr, envir=.env)
        .evaluated <<- TRUE
      }
      .val
    },

    kill=function() {
      .self$check.killed()
      .killed <<- TRUE
      TRUE
    }

  )
)

bg <- function(expr, par.gen=options()$background.backend) {
  if (is.null(options()$background.backend)) {
    warning("No backend specified, sequential will be used")
    options(background.backend="sequential.process")
    par.gen <- "sequential.process"
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

