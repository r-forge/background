
require(methods)
require(utils)


process.interface <- setRefClass("process.interface",
  fields = list(evaluated="logical", has.task="logical", expr="call", 
    env="environment", val="ANY", killed="logical"),

  methods=list(

    inherits.from.process.interface=function() TRUE,

    initialize=function() {
      killed <<- FALSE
      evaluated <<- FALSE
      .self
    },

    copy=function(shallow) stop("Future objects cannot be deep copied."),

    task=function() stop("The task method has not been implemented."),

    done=function() stop("The done method has not been implemented."),

    value=function() stop("The value method has not been implemented."),

    kill=function() stop("The kill method has not been implemented."),

    check.killed=function() {
      if (killed) {
        stop("This background object has been killed")
      }
    },

    killed=function() killed

  )
)
assign("process.interface", process.interface, .GlobalEnv)

sequential.process <- setRefClass("sequential.process",
  contains="process.interface",

  methods = list(

    initialize=function() {
      has.task <<- FALSE
      callSuper()
      .self
    },

    task=function(evalExpr, evalEnv=parent.frame()) {
      .self$check.killed()
      if (has.task)
        stop("This background object has already has a task")
      env <<- evalEnv
      expr <<- evalExpr
      has.task <<- TRUE
    },

    done=function() {
      .self$check.killed()
      TRUE
    },

    value=function() {
      .self$check.killed()
      if (!evaluated) {
        val <<- eval(expr, envir=env)
        evaluated <<- TRUE
      }
      val
    },

    kill=function() {
      .self$check.killed()
      killed <<- TRUE
      TRUE
    }

  )
)
assign("sequential.process", sequential.process, .GlobalEnv)

#multicore.process <- setRefClass("multicore.process",
#  contains="process.interface",
#  fields = list(pj="ANY"),
#
#  methods = list(
#
#    initialize=function() {
#      callSuper()
#    },
#
#    task=function(evalExpr, evalEnv=parent.frame()) {
#      .self$check.killed()
#      if (evaluated)
#        stop("This background object has already evaluated an expression")
#      env <<- evalEnv
#      expr <<- evalExpr
#      has.task <<- TRUE
#      pj <<- background:::mcpar(expr, env)
#    },
#
#    value=function() {
#      .self$check.killed()
#      if (!evaluated) {
#        v <- collect(pj)
#        if (length(v) == 0) {
#          val <<- NULL
#        } else {
#          val <<- v[[1]]
#        }
#        evaluated <<- TRUE
#      }
#      val
#    },
#
#    done=function() {
#      .self$check.killed()
#      ret <- FALSE
#      if (evaluated) {
#        ret <- TRUE
#      } else {
#        v <- collect(pj, wait=FALSE)
#        if( !is.null(v) ) {
#          if (length(v) == 0) {
#            val <<- NULL
#          } else {
#            val <<- v[[1]]
#          }
#          evaluated <<- TRUE
#        }
#      }
#      ret
#    },
#
#    kill=function() {
#      .self$check.killed()
#      multicore::kill(pj)
#      killed <<- TRUE
#      TRUE
#    }
#  
#  )
#)
#assign("multicore.process", multicore.process, .GlobalEnv)

#.onLoad <- function(libname, pkgname) {
  # We need methods so that we can define the reference class generators.
#}
