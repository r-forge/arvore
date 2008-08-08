\name{settreevartype}
\alias{settreevartype}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
settreevartype(TheTree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
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
function(TheTree) {
	TheTree$Level <- as.numeric(TheTree$Level)
	TheTree$Node.N <- as.numeric(TheTree$Node.N)
	TheTree$Node.name <- as.character(TheTree$Node.name)
	TheTree$Father <- as.numeric(TheTree$Father)
	TheTree$Father.Name <- as.character(TheTree$Father.Name)
	TheTree$Prob <- as.numeric(TheTree$Prob)
	TheTree$Type <- as.character(TheTree$Type)
	TheTree$Note <- as.character(TheTree$Note)
	TheTree$Destiny <- as.character(TheTree$Destiny)
	TheTree$Payoff1 <- as.numeric(as.character(TheTree$Payoff1))
	TheTree$Payoff2 <- as.numeric(as.character(TheTree$Payoff2))
	assign("TheTree", TheTree, .EnvironmentArvoRe)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
