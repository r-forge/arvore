\name{set.zoom.image.tree}
\alias{set.zoom.image.tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set.zoom.image.tree(imgHeight, imgWidth, scalarfac = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imgHeight}{ ~~Describe \code{imgHeight} here~~ }
  \item{imgWidth}{ ~~Describe \code{imgWidth} here~~ }
  \item{scalarfac}{ ~~Describe \code{scalarfac} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(imgHeight, imgWidth, scalarfac = 1) {
	imgHeight <- imgHeight * scalarfac
	imgWidth <- imgWidth * scalarfac
	assign("imgHeight", imgHeight, .EnvironmentArvoRe)
	assign("imgWidth", imgWidth, .EnvironmentArvoRe)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
