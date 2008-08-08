\name{save.as.file.arv}
\alias{save.as.file.arv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
save.as.file.arv(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function(...) {
	fileName<-tclvalue(tkgetSaveFile(filetypes="{{ArvoRe Files} {.arv}} {{All files} *}"))
	if (!nchar(fileName))
		tkfocus(tt)
	else {
		ans <- substr(fileName,nchar(fileName)-3,nchar(fileName))
		if ( ans != ".arv" ) fileName <- paste(fileName, ".arv", sep="")
		save(TheTree, .EnvironmentArvoRe, .modeltypeArvore, markov.propertiesMAT, file = fileName, ascii = TRUE)
		assign(".workstatus", "saved", .EnvironmentArvoRe)
		assign(".opennedfile", fileName, .EnvironmentArvoRe)
		.Windowtitle <- paste("ÁrvoRe - Janela Principal", " - [", .opennedfile, "]", sep = "")
		tkwm.title(tt, .Windowtitle)
		tkfocus(tt)
	}	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
