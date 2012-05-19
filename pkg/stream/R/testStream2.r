# Two components going to a single sink.
source("stream.r")

pl <- create.pipeline()

pl <- add.component(pl, "A", 
  function(x) {
    cat("Hello from A.  Processing packet", x, "\n\n")
  })

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

pl <- add.component(pl, "B", 
  packet.generator("Hello from B, creating packet",
  1:10))

pl <- add.component(pl, "C", 
  packet.generator("Hello from C, creating packet",
  letters))

pl <- add.connection(pl, "C", "A")
pl <- add.connection(pl, "B", "A")

run.seq(pl, 10)

