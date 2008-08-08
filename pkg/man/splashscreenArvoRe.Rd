\name{splashscreenArvoRe}
\alias{splashscreenArvoRe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
splashscreenArvoRe()
}
%- maybe also 'usage' for other objects documented here.
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
function() {
	splashArvoRe <- tktoplevel()
	Width <- 640
	Height <- 480
	tkwm.title(splashArvoRe, paste("ÁrvoRe - ", .arvore.version, sep=""))
	for (i in 1:length(.libPaths())) {
		icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Arvore.png",sep=""))
		if (file.exists(icon.but)) {
			icn <- tkimage.create("photo", file=icon.but)
			new.but <- tkbutton(splashArvoRe, image=icn, width=Width, height=Height, 
							command=function() tkdestroy(splashArvoRe))
	 		tkgrid(new.but)
		} 
	}
	posiciona.janela.tela(splashArvoRe)
	tcl("tkwait","window",splashArvoRe)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
