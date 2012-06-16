
multicore.process <- setRefClass("multicore.process",
  contains="process.interface",
  fields = list(pj="ANY"),

  methods = list(

    initialize=function() {
      callSuper()
    },

    task=function(evalExpr, evalEnv=parent.frame()) {
      .self$check.killed()
      if (.evaluated)
        stop("This background object has already evaluated an expression")

      # The following helper function is almost the same as the parallel 
      # (and mcparallel) functions from the multicore library except that 
      # it has an extra field to specify the evironment.
      mcpar <- function (expr, env, name, mc.set.seed = FALSE, silent = FALSE) {
          f <- multicore::fork()
          if (inherits(f, "masterProcess")) {
              on.exit(multicore::exit(1, 
                structure("fatal error in wrapper code", class = "try-error")))
              if (isTRUE(mc.set.seed))
                  set.seed(Sys.getpid())
              if (isTRUE(silent))
                  multicore:::closeStdout()
              multicore::sendMaster(try(eval(expr, env), silent = TRUE))
              multicore::exit(0)
          }
          if (!missing(name) && !is.null(name))
              f$name <- as.character(name)[1]
          class(f) <- c("parallelJob", class(f))
          f
      }
      .env <<- evalEnv
      .expr <<- evalExpr
      .has.task <<- TRUE
      pj <<- mcpar(.expr, .env)
    },

    value=function() {
      .self$check.killed()
      if (!.evaluated) {
        v <- multicore::collect(pj)
        if (length(v) == 0) {
          .val <<- NULL
        } else {
          .val <<- v[[1]]
        }
        .evaluated <<- TRUE
      }
      .val
    },

    done=function() {
      .self$check.killed()
      ret <- FALSE
      if (.evaluated) {
        ret <- TRUE
      } else {
        v <- collect(pj, wait=FALSE)
        if( !is.null(v) ) {
          if (length(v) == 0) {
            .val <<- NULL
          } else {
            .val <<- v[[1]]
          }
          .evaluated <<- TRUE
        }
      }
      ret
    },

    kill=function() {
      .self$check.killed()
      multicore::kill(pj)
      .killed <<- TRUE
      TRUE
    }
  
  )
)

