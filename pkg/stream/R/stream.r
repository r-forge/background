require(igraph)

# Processing pipeline is composed of computing components (vertices) and
# queues (arcs).

# A component has incoming queue connections, outgoing queue connections,
# and a function.  The parameter passed to the function is the 
# data received on the incoming queue.  The function returns data
# to the component and the component pushes this new data onto 
# the output queue.  A component with both incoming and outgoing 
# queues is called a filter.  It should be noted that a component 
# does not need both incoming and outgoing queues.  When a component
# has only outgoing queues it produces data for the rest of the pipeline
# and is called a source.  When a component has only incoming queues
# it is called a sink.

# Define the new pipeline type, derived from igraph, which is an S3 class
# type.

######################################################################
## The pipeline  class
######################################################################

add.class <- function(x, new.class) {
  class(x) <- unique(c(class(x), new.class))
  x
}

# Create a new pipeline.  For now, you'll create an empty pipeline
# and add things to it.
create.pipeline <- function() {
  add.class(graph.empty(), "stream")
}

# The S3 generic method registrations.
add.component <- function(pipeline, label, fun,
  in.components, out.components) {
  UseMethod("add.component", pipeline)
}

remove.component <- function(pipeline, label) {
  UseMethod("remove.component", pipeline)
}

add.connection <- function(pipeline, from, to) {
  UseMethod("add.connection", pipeline)
}

remove.connection <- function(pipeline, from, to) {
  UseMethod("remove.connection", pipeline)
}

# The pipeline specializations of the methods.

# Add a new component.
add.component.stream <- function(pipeline, label, fun, 
  in.components=vector(mode="character", length=0), 
  out.components=vector(mode="character", length=0)) {
  if (label %in% V(pipeline)$label) {
    stop(paste("A component with label", label,
      "already exists."))
  }
  pipeline <- add.vertices(pipeline, nv=1, label=label)
  V(pipeline)$fun[length(V(pipeline))] <- list(fun)
  add.class(pipeline, "stream")
}

delete.components.stream <- function(pipeline, labels) {
  add.class(delete.vertices( pipeline, match(labels, V(pipeline)$label) ), 
    "stream")
}

add.connection.stream <- function(pipeline, from, to) {
  add.class(add.edges(pipeline, 
    match(as.vector(t(cbind(from, to))), V(pipeline)$label)-1), 
    "stream")
}

delete.connection.stream <- function(pipeline, connections) {
  add.class(delete.edges(pipeline, connections), "stream")
}


# Run a pipeline sequentially.  Optionally set the number of
# iterations that will be run.
run.seq <- function(pipeline, max.it=NULL) {
  # Create the queues that connect the components.

  # Queues are defined by in-edges.  If there are many components
  # that have connections going to a single component, then 
  # a single queue is created and labelled with the in-edge vertex.

  # Get the labels for the vertices with in-degree greater than zero.
  qn <- V(pipeline)$label[which(degree(pipeline, mode="in") > 0)]
  
  qs <- Map( function(x) basic.queue$new(), qn )
  names(qs) <- qn

  # Since the graph is not a DAG, we can't do a topological sort
  # to push individual events through the pipeline.  For now
  # we are going to call component functions round-robin.

  # Precompute the in and out degrees
  in.degree <- degree(pipeline, mode="in")
  out.degree <- degree(pipeline, mode="out")

  # We'll keep track of the number of iterations for logging.
  it <- 1
  vs <- V(pipeline)
  es <- E(pipeline)
  while (is.null(max.it) || it < max.it) {
    for (i in 1:length(V(pipeline))) {
      out.packet <- NULL
      if (in.degree[i] == 0 && out.degree[i] > 0) {
        # It is a source.  Call the function and put the
        # result on the appropriate queue(s).
        out.packet <- vs$fun[[i]]()
        if (!is.null(out.packet)) {
          outQLabels <- vs$label[get.adjlist(pipeline, "out")[[i]]+1]
          for (label in outQLabels) {
            push(qs[[label]], out.packet)
          } 
        }
      } else if (in.degree[i] > 0 && out.degree[i] == 0) {
        # It is a sink.
        in.packet <- pop(qs[[vs$label[i]]])
        if (!is.null(in.packet)) {
          vs$fun[[i]](in.packet)
        }
      } else if (in.degree[i] > 0 && out.degree[i] > 0) {
        # It is a filter.
        in.packet <- pop(qs[[vs$label[i]]])
        if (!is.null(in.packet)) {
          out.packet <- vs$fun[[i]](in.packet)
        }
        if (!is.null(out.packet)) {
          outQLabels <- vs$label[get.adjlist(pipeline, "out")[[i]]+1]
          for (label in outQLabels) {
            push(qs[[label]], out.packet)
          } 
        }
      } else {
        # It has no in or out edges.
        vs$fun[[i]]()
      }
    }
    it <- it + 1
  }
}


