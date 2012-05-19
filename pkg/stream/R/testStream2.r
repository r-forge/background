source("stream.r")
source("../../queue/queue.R")

pl <- create.pipeline()

pl <- add.component(pl, "B", 
  function(x) {
    cat("Hello from B.  Processing packet", x, "\n\n")
    x
  })

packet.generator <- function() {
  i <- 1
  function() {
    ret <- i
    cat("Hello from A.  Creating packet", as.character(i), "\n\n")
    i <<- i +1
    ret
  }
}
pl <- add.component(pl, "A", packet.generator())

pl <- add.component(pl, "C", 
  function(x) {
    cat("Hello from C.  Processing packet", x, "\n\n")
  })

pl <- add.connection(pl, "A", "B")
pl <- add.connection(pl, "B", "C")

run.seq(pl, 10)

