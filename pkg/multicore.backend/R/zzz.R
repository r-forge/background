
multicore.process <- setRefClass("multicore.process",
  contains="process.interface",
  fields = list(pj="ANY"),

  methods = list(

    initialize=function() {
      callSuper()
    },

    task=function(evalExpr, evalEnv=parent.frame()) {
      .self$check.killed()
      if (evaluated)
        stop("This background object has already evaluated an expression")
      env <<- evalEnv
      expr <<- evalExpr
      has.task <<- TRUE
      pj <<- background:::mcpar(expr, env)
    },

    value=function() {
      .self$check.killed()
      if (!evaluated) {
        v <- collect(pj)
        if (length(v) == 0) {
          val <<- NULL
        } else {
          val <<- v[[1]]
        }
        evaluated <<- TRUE
      }
      val
    },

    done=function() {
      .self$check.killed()
      ret <- FALSE
      if (evaluated) {
        ret <- TRUE
      } else {
        v <- collect(pj, wait=FALSE)
        if( !is.null(v) ) {
          if (length(v) == 0) {
            val <<- NULL
          } else {
            val <<- v[[1]]
          }
          evaluated <<- TRUE
        }
      }
      ret
    },

    kill=function() {
      .self$check.killed()
      multicore::kill(pj)
      killed <<- TRUE
      TRUE
    }
  
  )
)
assign("multicore.process", multicore.process, .GlobalEnv)

.onLoad <- function(libname, pkgname) {
  backend <- options()$background.backend
  if (!is.null(backend) && !(backend=="sequential.process")) {
    warning("A backend has previously been set.  Multicore is now the backend")
  }
  options(background.backend="multicore.process")
}
