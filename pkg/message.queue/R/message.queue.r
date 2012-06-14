
message.queue <- setRefClass("message.queue", contains="queue.interface",
  fields=list(q="ANY"),
  methods=list(
    initialize=function(name, create=FALSE) {
      if (class(name) != "character") {
        stop("The name parameter must be of type character")
      }
      q <<- RawMessageQueue$new()
      if (create) {
        q$create(name)
      } else {
        q$attach(name)
      }
      callSuper()
    },
    push=function(elem) {
      if (class(elem) != "raw") {
        elem <- serialize(elem, NULL)
      }
      q$push(elem)
    },
    pop=function() {
      r <- q$pop()
      unserialize(r)
    }
  ))
