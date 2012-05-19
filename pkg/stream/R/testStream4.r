# One component going to two different sinks.
source("stream.r")

pl <- create.pipeline()

packet.generator <- function(str, vals) {
  i <- 1
  vals <- vals
  function() {
    ret <- vals[i]
    cat(str, as.character(vals[i]), "\n\n")
    i <<- i +1
    ret
  }
}

pl <- add.component(pl, "A", 
  packet.generator("Hello from A, creating packet",
  letters))

pl <- add.component(pl, "B",
  function(x) {
    cat("Hello from B. Processing packet", x, "\n\n")
    x
  })

pl <- add.component(pl, "C",
  function(x) {
    cat("Hello from C. Processing packet", x, "\n\n")
    x
  })

pl <- add.component(pl, "D",
  function(x) {
    cat("Hello from D. Processing packet", x, "\n\n")
    x
  })


pl <- add.connection(pl, "A", "C")
pl <- add.connection(pl, "A", "B")
pl <- add.connection(pl, "C", "D")
pl <- add.connection(pl, "B", "D")

run.seq(pl, 10)

