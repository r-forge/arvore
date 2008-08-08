\name{remove.node}
\alias{remove.node}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
remove.node(TheTree, node.col, node.number)
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
	removelines <- select.subtree(TheTree, node.col, node.number, change.row.names = FALSE)
	removelines <- rownames(removelines)
	
	num.lin <- dim(TheTree)[1]
	
	whoiwant <- as.numeric(setdiff(as.character(1:num.lin), removelines))
	
	ans <- TheTree[whoiwant,]
	
	ans <- as.data.frame(ans)
	
	ans$Level <- as.numeric(as.character(ans$Level))
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
	
	ans <- ans[ order(ans$Level,ans$Father, ans$Node.N),]
	
	position <- which(ans$Level == 2)
	
if( ( length(position) > 1 ) && ( dim(ans)[1] > 2 )) {
	#- Correção para o primeiro do nível ---------------------------------------------------------------
	.stopit <- FALSE
	i <- 1
	nans <- dim(ans)[1]
	while ( !.stopit ) {
		i <- i + 1
		GTtflag <- 	( as.numeric(ans$Node.N[i]) != 1 ) &&
					( as.numeric(ans$Level[i]) > as.numeric(ans$Level[i-1]) )
			if (GTtflag) {
				old.value <- ans$Node.N[i]
				ans$Node.N[i] <- 1
				usedlevel <- ans$Level[i] + 1
				position <- intersect(which(ans$Level == usedlevel),which(ans$Father == old.value))
				if ( length(position) > 0) {
					ans$Father[position] <- ans$Node.N[i]
					ans$Father.Name[position] <- ans$Node.name[i]
				}
				ans <- ans[ order(ans$Level,ans$Father, ans$Node.N),]
				i <- 1
			} else {
				if (i >= nans) .stopit <- TRUE
			}
		}

	#- Correção para numeracao dos nodos -------------------------------------------------------------
	.stopit <- FALSE
	i <- 1
	nans <- dim(ans)[1]
	while ( !.stopit ) {
		i <- i + 1
		GTtflag <- 	( as.numeric(ans$Node.N[i]) > as.numeric(ans$Node.N[i-1])+1 ) &&
					( as.numeric(ans$Level[i]) == as.numeric(ans$Level[i-1]) )
			if (GTtflag) {
				old.value <- ans$Node.N[i]
				ans$Node.N[i] <- ans$Node.N[i-1] + 1
				usedlevel <- ans$Level[i-1] + 1
				position <- intersect(which(ans$Level == usedlevel),which(ans$Father == old.value))
				if ( length(position) > 0) {
					ans$Father[position] <- old.value
					ans$Father.Name[position] <- ans$Node.name[i-1]
				}
				ans <- ans[ order(ans$Level,ans$Father, ans$Node.N),]
				i <- 1
			} else {
				if (i >= nans) .stopit <- TRUE
			}
		}
	#--------------------------------------------------------------------------------------------------
  }		
	rownames(ans) <- NULL
	return(ans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
