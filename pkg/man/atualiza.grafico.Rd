\name{atualiza.grafico}
\alias{atualiza.grafico}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
atualiza.grafico(...)
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
	.Filename <- paste(tempdir(),"\\", "arvore.png", sep="")
	if (!file.exists(.Filename)) file.remove(.Filename)
	
	png(file=.Filename, width = imgWidth, height = imgHeight, bg = "white", restoreConsole = FALSE)
		plot.tree(TheTree, line.type = .treeangle, show.probability = .probabilityconf, 
					show.payoffs = .payoffsconf, show.notes = .notesconf, 
					node.name.font.size = .node.name.font.size, payoffs.font.size = .payoffs.font.size, 
					notes.font.size = .notes.font.size)
	dev.off()

	image1 <- tclVar()
	tcl("image","create","photo",image1,file=.Filename)
	tkcreate(Canvas, "image", imgWidth/2, imgHeight/2, image = image1, anchor = "center")
	tkconfigure(Canvas, scrollregion = c(0,0,imgWidth,imgHeight))
	
	file.remove(.Filename)
	tkwm.deiconify(tt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
