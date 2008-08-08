\name{cost.effectiveness.table}
\alias{cost.effectiveness.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cost.effectiveness.table(TheTree)
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
	y <- Matrixset$y
	probMAT <- Matrixset$probMAT
	utilityMAT <- Matrixset$utilityMAT
	effectivenessMAT <- Matrixset$effectivenessMAT
	typeMAT <- Matrixset$typeMAT
	
	rollbackLIST <- rollback(TheTree)
	
	num.col <- dim(x)[2]
	num.lin <- dim(x)[1]
	
	levelnode <- array(,0)
	paispos <- array(,0)
	nnode <- array(,0)
	namenode <- array(,0)
	probnode <- array(,0)
	utilitynode <- array(,0)
	effectivenessnode <- array(,0)
	typenode <- array(,0)
	paisnodos.n <- array(,0)
	paisnodos.name <- array(,0)
	paisnodos <- array(,0)
	expectedvalue.cost <- array(,0)
	expectedvalue.effectiveness <- array(,0)
	expectedvalue.ce <- array(,0)	
	
	for (i in 1:num.col) {
		max.node <- max(x[,i], na.rm = TRUE)
		pais <- 1:max.node	
		for (k in pais) {
			levelnode <- c(levelnode,i)
			nodepos <- which(x[,i] == k)[1]
			paispos <- c(paispos, nodepos)
			if (i == 1) {
				paisnodos.n <- c(paisnodos.n, 1)
				paisnodos.name <- c(paisnodos.name, " ")
			} else {
				paisnodos.n <- c(paisnodos.n, x[nodepos, i-1])
				paisnodos.name <- c(paisnodos.name, y[nodepos, i-1])
			}
			nnode <- c(nnode, k)
			namenode <- c(namenode, y[nodepos, i])
			probnode <- c(probnode, probMAT[nodepos, i])
			utilitynode <- c(utilitynode, utilityMAT[nodepos, i])
			effectivenessnode <- c(effectivenessnode, effectivenessMAT[nodepos, i])
			typenode <- c(typenode, typeMAT[nodepos, i])
			expectedvalue.cost <- c(expectedvalue.cost, rollbackLIST[["Cost"]][nodepos, i])
			expectedvalue.effectiveness <- c(expectedvalue.effectiveness, rollbackLIST[["Effectiveness"]][nodepos, i])
			expectedvalue.ce <- c(expectedvalue.ce, rollbackLIST[["CE"]][nodepos, i])
			
		}
	}
	
	tabela <- data.frame(Level = levelnode, Node.N = nnode, Node.name = namenode,
						 Mean.Cost = expectedvalue.cost,
						 Mean.Effectiveness = expectedvalue.effectiveness,
						 Mean.C.E.ratio = expectedvalue.ce
						 )
						 
	tabela <- subset(tabela, Level == 2)
	tabela <- as.data.frame(tabela)
	
	tabela$Level <- as.numeric(tabela$Level)
	tabela$Node.N <- as.numeric(tabela$Node.N)
	tabela$Node.name <- as.character(tabela$Node.name)
	tabela$Mean.Cost <- as.numeric(as.numeric(tabela$Mean.Cost))
	tabela$Mean.Effectiveness <- as.numeric(as.numeric(tabela$Mean.Effectiveness))
	tabela$Mean.C.E.ratio <- as.numeric(as.numeric(tabela$Mean.C.E.ratio))
		
	return(tabela)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
