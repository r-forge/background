
queue.interface <- setRefClass("queue.interface", 
  methods=list(
    push <- function( elem ) stop("The push method has not been implemented"),
    pop <- function() stop("The pop method has not been implemented")
  )
)

basic.queue <- setRefClass("basic.queue", 
  contains=c("queue.interface"),
  fields=list(.elems="list"),
  methods=list(
    push=function(elem) {
      .elems <<- append(.elems, elem)
      TRUE
    },
    pop=function() {
      ret <- NULL
      if (length(.elems) > 0) {
        ret <- .elems[[1]]
        .elems[1] <<- NULL
      }
      ret
    }
  ))

queue <- function(...) {
  if (is.null(options()$queue.backend)) {
    options(queue.backend="basic.queue")  
  }
  eval(parse(text=paste(options()$queue.backend, "$new(...)", sep='')))
}

push <- function( q, val ) {
  q$push(val)
}

pop <- function(q) {
  q$pop()
}

register.queue <- function(type) {
  # For right now I can only check to see if the backendName exists.
  # It would be nice to see if it inherits from process.interface.
  if (!exists(type)) {
    stop(paste("The reference class", type, "could not be found"))
  } else {
    options(queue.backend=type)
  }
  return(TRUE)
}

# Forget Redis queues... for now

#setClass("redis.queue.serialize", contains="serialize", 
#  representation=list(handle="character", host="character", port="character"))
#
#setMethod("unserialize", signature(p="redis.queue.serialize"), 
#  function(p) {
#    redis.queue$new(.handle=p@handle, .host=p@host, .port=p@port)
#  })
#
#redis.queue <- setRefClass("redis.queue", 
#  contains=c("queue.interface", "serialize.interface"),
#  fields=list(.handle="character", .host="character", .port="character"),
#  methods=list(
#    
#    initialize=function( handle=NULL, host="localhost", port="6379" ) {
#      require(rredis)
#      .host <<- host
#      .port <<- port
#      redisConnect(host=.host, port=.port)
#      if (is.null(handle)) {
#        if (!require(synchronicity, quietly=TRUE)) {
#          stop(paste("You must specify a queue handle or install the synchronicity",
#            "package"))
#        } else {
#          # Create a unique handle.
#          .handle <<- uuid()
#        }
#      } else {
#        .handle <<- handle
#      }
#      .self
#    },
#
#    push=function( elem ) {
#      redisLPush(.handle, elem)  
#    },
#
#    pop=function() {
#      redisRPop(.handle)
#    },
#
#    serialize=function() {
#      new("redis.queue.serialize", handle=.handle, host=.host, port=.port)
#    }
#  )
#)

