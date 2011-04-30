\name{multicore.backend-package}
\alias{multicore.backend-package}
\alias{multicore.backend}
\docType{package}
\title{
Use multicore to background calculations
}
\description{
put description here
}
\details{
\tabular{ll}{
Package: \tab multicore.backend\cr
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

