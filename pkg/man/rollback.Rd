\name{rollback}
\alias{rollback}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rollback(TheTree)
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
	Matrixset <- convert2matrix(TheTree)
	
	x <- Matrixset$x
	probMAT <- Matrixset$probMAT
	utilityMAT <- Matrixset$utilityMAT
	effectivenessMAT <- Matrixset$effectivenessMAT
	
	num.col <- dim(probMAT)[2]
	num.lin <- dim(probMAT)[1]
	
	ans.ce <- matrix(0, num.lin, num.col)
	ans.cost <- matrix(0, num.lin, num.col)
	ans.effectiveness <- matrix(0, num.lin, num.col)
	
	for (i in 1:(num.col)) {
		nodes <- as.numeric(names(table(x[,i])))
		for (j in nodes) {
			position <- which(x[,i] == j)
			sub.x <- x[position, i:num.col]
			lines.sub <- length(position)
			column.sub <- num.col - i + 1
			sub.x <- matrix(sub.x, lines.sub, column.sub)
			sub.prob <- probMAT[position, i:num.col]
			sub.prob <- matrix(sub.prob, lines.sub, column.sub)
			sub.util <- utilityMAT[position, i:num.col]
			sub.util <- matrix(sub.util, lines.sub, column.sub)
			sub.effectiveness <- effectivenessMAT[position, i:num.col]
			sub.effectiveness <- matrix(sub.effectiveness, lines.sub, column.sub)
			
			if (is.null(sub.prob)) {
				sub.prob[,1] <- 1
				sub.util[,1] <- 0
				sub.effectiveness[,1] <- 1
				val.expected.ce <- sum ( apply(sub.prob, 1, prod) * apply(sub.util/sub.effectiveness, 1, sum) )
				val.expected.cost <- sum ( apply(sub.prob, 1, prod) * apply(sub.util, 1, sum) )
				val.expected.effectiveness <- sum ( apply(sub.prob, 1, prod) * apply(sub.effectiveness, 1, sum) )
			} else {
				sub.prob[,1] <- 1
				val.expected <- sum ( apply(sub.prob,1,prod) * apply(sub.util/sub.effectiveness,1,sum) )
				val.expected.cost <- sum ( apply(sub.prob, 1, prod) * apply(sub.util, 1, sum) )
				val.expected.effectiveness <- sum ( apply(sub.prob, 1, prod) * apply(sub.effectiveness, 1, sum) )
			}
			ans.ce[position, i] <- val.expected
			ans.cost[position, i] <- val.expected.cost
			ans.effectiveness[position, i] <- val.expected.effectiveness
		}	
	}
	ans <- list("CE" = ans.ce, "Cost" = ans.cost, "Effectiveness" = ans.effectiveness)
	return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
