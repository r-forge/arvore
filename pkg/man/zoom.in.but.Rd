\name{zoom.in.but}
\alias{zoom.in.but}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
zoom.in.but(imgHeight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imgHeight}{ ~~Describe \code{imgHeight} here~~ }
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
function(imgHeight) {
	if (imgHeight < 8000) {
		imgHeight <- round(imgHeight * 1.1, digits = 0)
		imgWidth <- round((4/3) * imgHeight, digits = 0)
	} else {
		msg <- paste("Este é um tamanho de imagem consideravelmente grande. Deseja realmente ampliar?")
		ans <- tkmessageBox(message = msg,  icon = "question", type = "yesnocancel", default = "no")
		ans <- as.character(tclvalue(ans))
			if ( ans == "yes" ) {
				imgHeight <- round(imgHeight * 1.1, digits = 0)
				imgWidth <- round((4/3) * imgHeight, digits = 0)
			}
		tkfocus(tt)	
	}
	set.zoom.image.tree(imgHeight, imgWidth)
	refreshF5()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
