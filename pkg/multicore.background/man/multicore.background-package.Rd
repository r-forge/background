\name{multicore.background-package}
\alias{multicore.background-package}
\docType{package}
\title{
A parallel bakend for the \pkg{background} package.
}
\description{
This package allows users to spawn ``backgrounded'' \R operations 
using the multicore package.
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
register.background("multicore.process")
bg.obj <- bg({Sys.sleep(1); "done"})
print(done(bg.obj))
fg(bg.obj)
}

