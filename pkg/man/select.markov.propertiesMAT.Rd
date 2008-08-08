\name{select.markov.propertiesMAT}
\alias{select.markov.propertiesMAT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
select.markov.propertiesMAT(TheTree, SubTree, markov.propertiesMAT)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
  \item{SubTree}{ ~~Describe \code{SubTree} here~~ }
  \item{markov.propertiesMAT}{ ~~Describe \code{markov.propertiesMAT} here~~ }
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
function(TheTree, SubTree, markov.propertiesMAT) {
	require(abind)
	
	selected.lines <- rownames(SubTree)
	check.tree <- TheTree[selected.lines,]
	wanted.level <- check.tree$Level[1]+1
	check.tree <- check.tree[check.tree$Level == wanted.level,]
	ans <- data.frame(	"Level" = array(,0),
						"Node.N" = array(,0),
						"Node.name" = array(,0),
						"Father" = array(,0),
						"Father.Name" = array(,0),
						"Initial.cost" = array(,0), 
						"Incremental.cost" = array(,0), 
						"Final.cost" = array(,0),
						"Initial.effectiveness" = array(,0), 
						"Incremental.effectiveness" = array(,0), 
						"Final.effectiveness" = array(,0))
	for (i in 1:length(check.tree$Node.N) ) {
		balde <- subset(markov.propertiesMAT, Node.N == check.tree$Node.N[i])
		n.lin.balde <- dim(balde)[1]
		if (n.lin.balde > 0) {
			ans <- abind(ans, balde, along = 1)
		} else {
			balde <- data.frame(	"Level" = check.tree$Level[i],
									"Node.N" = check.tree$Node.N[i],
									"Node.name" = check.tree$Node.name[i],
									"Father" = check.tree$Father[i],
									"Father.Name" = check.tree$Father.Name[i],
									"Initial.cost" = 0, 
									"Incremental.cost" = check.tree$Payoff1[i], 
									"Final.cost" = 0,
									"Initial.effectiveness" = 0, 
									"Incremental.effectiveness" = check.tree$Payoff2[i], 
									"Final.effectiveness" = 0)
			ans <- abind(ans, balde, along = 1)
		}
	}
	ans <- as.data.frame(ans)
	
	wanted.level.sub <- SubTree$Level[1]+1
	subSubTree <- subset(SubTree, Level == wanted.level.sub)
	ans$Level <- subSubTree$Level
	ans$Node.N <- subSubTree$Node.N
	ans$Father <- subSubTree$Father
	ans$Father.Name <- subSubTree$Father.Name
	rownames(ans) <- rownames(subSubTree)
	
	ans$Level <- as.numeric(as.character(ans$Level))
	ans$Node.N <- as.numeric(as.character(ans$Node.N))
	ans$Node.name <- (as.character(ans$Node.name))
	ans$Father <- as.numeric(as.character(ans$Father))
	ans$Father.Name <- (as.character(ans$Father.Name))
	ans$Initial.cost <- as.numeric(as.character(ans$Initial.cost))
	ans$Incremental.cost <- as.numeric(as.character(ans$Incremental.cost))
	ans$Final.cost <- as.numeric(as.character(ans$Final.cost))
	ans$Initial.effectiveness <- as.numeric(as.character(ans$Initial.effectiveness))
	ans$Incremental.effectiveness <- as.numeric(as.character(ans$Incremental.effectiveness))
	ans$Final.effectiveness <- as.numeric(as.character(ans$Final.effectiveness))
	
	return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
