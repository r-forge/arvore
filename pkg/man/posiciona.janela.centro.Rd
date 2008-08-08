\name{posiciona.janela.centro}
\alias{posiciona.janela.centro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
posiciona.janela.centro(janela.principal, janela.nova)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{janela.principal}{ ~~Describe \code{janela.principal} here~~ }
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
function(janela.principal, janela.nova) {
	MAX.height <- as.integer( tclvalue( tkwinfo("screenheight", janela.principal) ) )
	MAX.width <- as.integer( tclvalue( tkwinfo("screenwidth", janela.principal) ) )

	wm.main.height <- as.integer( tclvalue( tkwinfo("height", janela.principal) ) )
	wm.main.width <- as.integer( tclvalue( tkwinfo("width", janela.principal) ) )

	wm.child.height <- as.integer( tclvalue( tkwinfo("height", janela.nova) ) )
	wm.child.width <- as.integer( tclvalue( tkwinfo("width", janela.nova) ) )

	wm.x <- as.integer( tclvalue( tkwinfo("x", janela.principal) ) )
	wm.y <- as.integer( tclvalue( tkwinfo("y", janela.principal) ) )
	
	new.wm.x <- wm.x + wm.main.width/2 - wm.child.width/2
	new.wm.y <- wm.y + wm.main.height/2 - wm.child.height/2
	
	new.wm.x <- round(new.wm.x)
	new.wm.y <- round(new.wm.y)
	
	limite.sup.x <- round( MAX.width - wm.child.width )
	limite.inf.x <- round( wm.child.width )
	limite.sup.y <- round( MAX.height - wm.child.height )
	limite.sup.y <- round( wm.child.height )

	# Limitantes para o tamanho da tela. Quem tem tela virtural... #$\%#$\%
	if (new.wm.x > limite.sup.x) new.wm.x <- limite.sup.x
	if (new.wm.x < limite.inf.x) new.wm.x <- limite.inf.x
	if (new.wm.y > limite.sup.y) new.wm.y <- limite.sup.y
	if (new.wm.y > limite.sup.y) new.wm.y <- limite.sup.y
	
	posicao <- paste(wm.child.width, "x", wm.child.height, "+", new.wm.x,"+", new.wm.y, sep="")
	tkwm.geometry(janela.nova,posicao)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
