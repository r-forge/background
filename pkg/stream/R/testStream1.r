# Three components in a single pipe.
source("stream.r")

pl <- create.pipeline()

# First we'll create a filter component (both input and output queues).
pl <- add.component(pl, "B", 
  function(x) {
    cat("Hello from B.  Processing packet", x, "\n\n")
    x
  })

# Next well create a closure that will act as a source.
# All it does is send consecutive integers through the pipeline.
packet.generator <- function() {
  i <- 1
  function() {
    ret <- i
    cat("Hello from A.  Creating packet", as.character(i), "\n\n")
    i <<- i +1
    ret
  }
}

# Add the closure to the pipeline.
pl <- add.component(pl, "A", packet.generator())

# Now we'll add a sink to the pipeline.
pl <- add.component(pl, "C", 
  function(x) {
    cat("Hello from C.  Processing packet", x, "\n\n")
  })

# Connect the source (A) to the filter (B) and the 
# filter (B) to the sink (C)
pl <- add.connection(pl, "A", "B")
pl <- add.connection(pl, "B", "C")

# You can see the structure of the pipeline with 
#plot(pl)

# And we can run each of the components 10 times sequentially.
run.seq(pl, 10)


