\name{message.queue-package}
\alias{message.queue-package}
\docType{package}
\title{
Message passing between processes on a single machine.
}
\description{
This package implements message queues, based on the \code{queue.interface}
class defined in the \code{queue} package.
}
\details{
\tabular{ll}{
Package: \tab message.queue\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2012-06-11\cr
License: \tab LGPL-2\cr
}
}
\author{
Michael Kane

Maintainer: Mike <kaneplusplus@gmail.com>
}
\examples{\dontrun{
q <- queue("message.queue", "queue_resource")
push(q, 1:10)
print(pop(q, 1:10))
}}
