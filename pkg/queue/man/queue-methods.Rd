\name{push, pop}
\alias{create.queue}
\alias{push}
\alias{pop}
\title{Functions for queue operations}
\description{

}
\usage{
create.queue(type=NULL, ...)
push(q, val)
pop(q)
}
\arguments{
  \item{q}{The queue to push or pop values from}
  \item{val}{The value to push onto the queue}
  \item{..}{Other parameters}
}
\details{
By default the \code{create.queue} function creates a \code{basic.queue}
which can be used on a single \R process for sequential data
storage.  When distributed queues are used, as in the case with
the \code{redis.queue} package arguments such as the shared 
resource name must be specified via the \code{name} parameter.
}
\value{
\code{create.queue} returns the queue data structure.

\code{push} returns \code{TRUE} or \code{FALSE} depending on whether
or not the push is successful.

\code{pop} returns the data at the head of the queue.

}
\author{
Michael Kane
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{\dontrun{
# Background a simple R expression.
bg.comp <- bg({Sys.sleep(2); "Awake!\n"})

# If the calculation is not complete do something else.
if (!done(bg.comp)) {
  cat("Waiting for calculation\n")
}

# Output the result.
cat(fg(bg.comp))
}}
