\name{probability.check}
\alias{probability.check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
probability.check(k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{ ~~Describe \code{k} here~~ }
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
function(k) {
	names(k) <- c("Level", "Node.N", "Node.name", "Father", "Father.Name",
 					"Prob", "Type", "Note", "Destiny", "Payoff1", "Payoff2")
	Levels <- 2:max(k$Level)
	variables <- names(k)
	
	ans <- ""
	for (i in Levels) {
		Data <- subset(k, Level == i, select = variables)
		nodes <- as.numeric(names(table(Data$Father)))
		for (j in nodes) {
			Data2 <- subset(Data, Father == j, select = variables)
			psum <- sum(Data2$Prob)
			if (psum != 1) {
				nome.pai <- Data2$Father.Name[1]
				ans <- paste(ans, 
				"Há problema em [ NÍVEL = ", i-1, ", NODO = ", nome.pai, " ] \n", sep = "")
			}
		}
	}
	ans2 <- "1"
	if (nchar(ans) == 0) {
		ans <- "As probabilidades somam 1. Tudo ok!"
		ans2 <- "0"
	}
		
	return(c(ans,ans2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
