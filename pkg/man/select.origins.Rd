\name{select.origins}
\alias{select.origins}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
select.origins(TheTree, node.col, node.number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
  \item{node.col}{ ~~Describe \code{node.col} here~~ }
  \item{node.number}{ ~~Describe \code{node.number} here~~ }
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
function(TheTree, node.col, node.number) {
	require(abind)
	position <- intersect(which((TheTree$Level == node.col)),which(TheTree$Node.N == node.number))
	ans <- TheTree[position,]
	
	levelnodevalue <- node.col - 1
	nodenumbervalue <- ans$Father[1] #[position]
	
	while ( levelnodevalue > 0) {
		position <- intersect(which((TheTree$Level == levelnodevalue)),which(TheTree$Node.N == nodenumbervalue))
		subData <- TheTree[position,]
		ans <- abind(subData, ans, along=1)
		nodenumbervalue <- subData$Father[1]
		levelnodevalue <- levelnodevalue - 1
	}
	ans <- as.data.frame(ans)
	
	ans$Level <- as.numeric(ans$Level)
	ans$Node.N <- as.numeric(ans$Node.N)
	ans$Node.name <- as.character(ans$Node.name)
	ans$Father <- as.numeric(ans$Father)
	ans$Father.Name <- as.character(ans$Father.Name)
	ans$Prob <- as.numeric(ans$Prob)
	ans$Type <- as.character(ans$Type)
	ans$Note <- as.character(ans$Note)
	ans$Destiny <- as.character(ans$Destiny)
	ans$Payoff1 <- as.numeric(as.character(ans$Payoff1))
	ans$Payoff2 <- as.numeric(as.character(ans$Payoff2))

	return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
