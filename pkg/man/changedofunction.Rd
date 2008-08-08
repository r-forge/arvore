\name{changedofunction}
\alias{changedofunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
changedofunction(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
  \item{.modeltypeArvore}{ ~~Describe \code{.modeltypeArvore} here~~ }
  \item{.EnvironmentArvore.Secure}{ ~~Describe \code{.EnvironmentArvore.Secure} here~~ }
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
function(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure) {
	TheTree.old <- TheTree
	.EnvironmentArvoRe.old <- .EnvironmentArvoRe
	.modeltypeArvore.old <- .modeltypeArvore
	
	assign("TheTree", get("TheTree", .EnvironmentArvore.Secure), .EnvironmentArvoRe)
	assign(".EnvironmentArvoRe", get(".EnvironmentArvoRe", .EnvironmentArvore.Secure), .EnvironmentArvoRe)
	assign(".modeltypeArvore", get(".modeltypeArvore", .EnvironmentArvore.Secure), .EnvironmentArvoRe)
	
	safedofunction(TheTree.old, .EnvironmentArvoRe.old, .modeltypeArvore.old)
	refreshF5()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
