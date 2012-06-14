\name{queue-package}
\alias{queue-package}
\docType{package}
\title{
Queue data structures
}
\description{
A queue is a sequential data structure allowing users
to push to the back of it to store data and pull from the front of it
to retrieve data.  The queue data structure provided in this package
is appropriate for use on a single-process R session.  However,
there are other packages, such as redis.queue, that allow data
structures to be passed between processes.
}
\details{
\tabular{ll}{
Package: \tab queue \cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2012-05-19\cr
License: \tab LGPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Michael J. Kane

Maintainer: Mike <kaneplusplus@gmail.com>
}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
%\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
# Create a queue.
q <- queue()

# Push the number one through 10 onto it.
for (i in 1:10) {
  push(q, i)
}

# Pop the values from the queue.
for (i in 1:10) {
  cat(i, " ")
}
cat("\n")
}

