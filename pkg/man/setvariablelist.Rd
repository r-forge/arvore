\name{setvariablelist}
\alias{setvariablelist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
setvariablelist(variableMAT, newvariableline = " ", variable.name = " ", action = "edit")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variableMAT}{ ~~Describe \code{variableMAT} here~~ }
  \item{newvariableline}{ ~~Describe \code{newvariableline} here~~ }
  \item{variable.name}{ ~~Describe \code{variable.name} here~~ }
  \item{action}{ ~~Describe \code{action} here~~ }
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
function(variableMAT, newvariableline = " ", variable.name = " ", action = "edit") {
	if (action == "delete") {
		variables <- names(variableMAT)
		ans <- subset(variableMAT, Name != variable.name, select = variables)
	}
	if (action == "add") {
		require(abind)
		ans <- abind(variableMAT, newvariableline, along=1)
	}
	if (action == "edit") {
		variables <- names(variableMAT)
		ans <- subset(variableMAT, Name != variable.name, select = variables)
		
		require(abind)
		ans <- abind(ans, newvariableline, along=1)
	}

	ans <- as.data.frame(ans)
	ans$Name <- as.character(ans$Name)
	ans$Fix.Value <- as.numeric(as.character(ans$Fix.Value))
	ans$Min.Value <- as.numeric(as.character(ans$Min.Value))
	ans$Max.Value <- as.numeric(as.character(ans$Max.Value))
	ans$Notes <- as.character(ans$Notes)
	assign("variableMAT", ans, envir = .EnvironmentArvoRe)
	assign(".workstatus", "unsaved", .EnvironmentArvoRe)
	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
