\name{background-package}
\alias{background-package}
\alias{background}
\docType{package}
\title{
Background calculations
}
\description{
Perform calculations concurently in the background thereby freeing the \R
console to perform other tasks.  The package currently supports a sequential
backend, where the calculation is evaluted when it is brought into the
foreground along with a multicore backend, where the calculation is started
immediately after it is backgrounded.  Other packages, such as 
multicore.background can be used for true process backgrounding.
}
\details{
\tabular{ll}{
Package: \tab background\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2011-04-07\cr
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

