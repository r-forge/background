\name{bg, fg, done, kill, killed}
\alias{bg}
\alias{fg}
\alias{done}
\alias{kill}
\alias{killed}
\alias{backend}
\title{Functions for backgrounding calculations}
\description{
a description
}
\usage{
backend(backendName=NULL)
bg(expr, par.gen=options()$background.backend)
done(process)
fg(process)
kill(process)
killed(process)
}
\arguments{
  \item{backendName}{The name of the reference class backgrounding the calculations}
  \item{expr}{The expression to be calculated in the background}
  \item{par.gen}{An object inherited from \code{bg.interface} that 
    provides the concurrent backend}
  \item{process}{The backgrounded process returned from the \code{bg} function}
}
\details{
The \code{bg} function is used to background a calculation.  After 
a calculation is backgrounded, the 
\code{done} function allows a user to find out if the calculation
is complete.  The \code{fg} function allows a user to retrieve
the result of the calculation.  If the \code{fg} function is called
before the calculation is complete, then the \R session will block until
it is complete and then the result is returned.  The \code{kill} function
allows a user to kill a backgrounded calculation and the \code{killed} function
checks to see if a running calculation has been killed.
}
\value{
\code{bg} returns a handle to the backgrounded calculation which can then
be used as an argument to \code{done} or \code{fg}.

\code{done} returns \code{TRUE} or \code{FALSE} depending on whether
or not the backgrounded calculation is complete.

\code{fg} returns the result of the backgrounded calculation.

\code{kill} returns \code{TRUE}.

\code{killed} returns \code{TRUE} if the calculation has been killed, 
\code{FALSE} otherwise.
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
