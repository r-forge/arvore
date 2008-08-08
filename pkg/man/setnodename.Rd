\name{setnodename}
\alias{setnodename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setnodename(TheTree, column, node.number, nodename, .EnvironmentArvoRe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
  \item{column}{ ~~Describe \code{column} here~~ }
  \item{node.number}{ ~~Describe \code{node.number} here~~ }
  \item{nodename}{ ~~Describe \code{nodename} here~~ }
  \item{.EnvironmentArvoRe}{ ~~Describe \code{.EnvironmentArvoRe} here~~ }
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
function(TheTree, column, node.number, nodename, .EnvironmentArvoRe) {
	if (!is.numeric(node.number)) node.number <- as.numeric(node.number)
	if (!is.numeric(column)) column <- as.numeric(column)
	
	position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
	if (length(position) >= 1) {
# 		old.father.name <- TheTree$Node.name[position]
		TheTree$Node.name[position] <- nodename

		position <- intersect(which((TheTree$Level == (column+1) )),which(TheTree$Father == node.number))

		if (length(position) >= 1) {
			TheTree$Father.Name[position] <- nodename
		}

	assign("TheTree", TheTree, envir = .EnvironmentArvoRe)
	assign(".workstatus", "unsaved", .EnvironmentArvoRe)
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
