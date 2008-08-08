\name{posiciona.janela.tela}
\alias{posiciona.janela.tela}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
posiciona.janela.tela(janela.nova)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{janela.nova}{ ~~Describe \code{janela.nova} here~~ }
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
function(janela.nova) {
	MAX.height <- as.integer( tclvalue( tkwinfo("screenheight", janela.nova) ) )
	MAX.width <- as.integer( tclvalue( tkwinfo("screenwidth", janela.nova) ) )
	
	wm.height <- as.integer( tclvalue( tkwinfo("height", janela.nova) ) )
	wm.width <- as.integer( tclvalue( tkwinfo("width", janela.nova) ) )

	new.wm.y <- round( MAX.height/2 - wm.height/2 )
	new.wm.x <- round( MAX.width/2 - wm.width/2 )
	
	limite.sup.x <- round( MAX.width - wm.width )
	limite.inf.x <- 0
	limite.sup.y <- round( MAX.height - wm.height )
	limite.inf.y <- 0
	
	# Limitantes para o tamanho da tela. Quem tem tela virtural... #$\%#$\%
	if (new.wm.x > limite.sup.x) new.wm.x <- limite.sup.x
	if (new.wm.x < limite.inf.x) new.wm.x <- limite.inf.x
	if (new.wm.y > limite.sup.y) new.wm.y <- limite.sup.y
	if (new.wm.y < limite.inf.y) new.wm.y <- limite.sup.y
	
	posicao <- paste(wm.width, "x", wm.height, "+", new.wm.x,"+", new.wm.y, sep="")
	tkwm.geometry(janela.nova,posicao)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
