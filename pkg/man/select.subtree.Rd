\name{select.subtree}
\alias{select.subtree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
select.subtree(TheTree, node.col, node.number, change.row.names = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
  \item{node.col}{ ~~Describe \code{node.col} here~~ }
  \item{node.number}{ ~~Describe \code{node.number} here~~ }
  \item{change.row.names}{ ~~Describe \code{change.row.names} here~~ }
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
function(TheTree, node.col, node.number, change.row.names = FALSE) {
	require(abind)
	levelmax <- max(TheTree$Level)
	variables <- names(TheTree)
	
	ans <- subset(TheTree, Level == node.col, select = variables)
	ans <- subset(ans, Node.N == node.number, select = variables)
# 	print(ans)
	
	if (node.col != levelmax) {
		i <- (node.col+1)
		pais <- node.number
		while (i != 0) {
			
			Datatmp <- subset(TheTree, Level == i, select = variables)
			novos.pais <- array(,0)
			for (j in pais) {
				DatatmpP <- subset(Datatmp, Father == j, select = variables)
				if (dim(DatatmpP)[1] != 0) {
					ans <- abind(ans, DatatmpP, along=1)
# 					print(ans)
					novos.pais <- c(novos.pais, DatatmpP$Node.N)
				}
			}
			pais <- novos.pais
			
			if (i == levelmax) {
				i <- 0
			} else {
				i <- i + 1
			}
			if( length(pais) == 0) i <- 0
		}
	}
	
	ans <- as.data.frame(ans)
	
	ans$Level <- as.numeric(ans$Level)
	ans$Node.N <- as.numeric(as.character(ans$Node.N))
	ans$Node.name <- as.character(ans$Node.name)
	ans$Father <- as.numeric(as.character(ans$Father))
	ans$Father.Name <- as.character(ans$Father.Name)
	ans$Prob <- as.numeric(as.character(ans$Prob))
	ans$Type <- as.character(ans$Type)
	ans$Note <- as.character(ans$Note)
	ans$Destiny <- as.character(ans$Destiny)
	ans$Payoff1 <- as.numeric(as.character(ans$Payoff1))
	ans$Payoff2 <- as.numeric(as.character(ans$Payoff2))
# 	
# 	# Ajusta a numeração dos nodos
# 	levelmax <- max(ans$Level)
# 	for (i in 1:levelmax) {
# 		positions <- which(ans$Level == i)
# 		n.node <- as.numeric(names(table(ans$Node.N[positions])))
# 		size.n.node <- length(n.node)
# 		for (j in 1:size.n.node) {
# 			positions.node.replace <- which(ans$Node.N == n.node[j])
# 			positions.node.replace <- intersect(positions, positions.node.replace)
# 			ans$Node.N[positions.node.replace] <- j
# 			if (i != levelmax) {
# 				positions.next.level <- which(ans$Level == (i+1))
# 				positions.node.as.father <- which(ans$Father == n.node[j])
# 				positions.node.as.father <- intersect(positions.next.level, positions.node.as.father)
# 				ans$Father[positions] <- j
# 			}
# 		}
# 	}
	ans <- ans[ order(ans$Level,ans$Node.N),]
 	if (change.row.names) rownames(ans) <- NULL
	return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
