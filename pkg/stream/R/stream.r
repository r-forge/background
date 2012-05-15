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

# Define the new pipeline type, derived from igraph.

# Create a new pipeline
create.pipeline <- function() {
}

# Add a new component.
add.component <- function(name, in.components, out.components, fun) {
}

remove.component <- function(name) {
}

add.connections <- function(from, to) {
}

remove.connections <- function(from, to) {
}

run <- function(pipeline) {
}
