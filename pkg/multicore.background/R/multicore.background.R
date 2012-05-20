# The following function is almost the same as the parallel (and mcparallel)
# functions from the multicore library except that it has an extra
# field to specify the evinronment.
mcpar <- function (expr, env, name, mc.set.seed = FALSE, silent = FALSE) {
    f <- multicore::fork()
    if (inherits(f, "masterProcess")) {
        on.exit(multicore::exit(1, structure("fatal error in wrapper code",
            class = "try-error")))
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

