\name{dimensoes.janela}
\alias{dimensoes.janela}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dimensoes.janela(janela, height, width)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{janela}{ ~~Describe \code{janela} here~~ }
  \item{height}{ ~~Describe \code{height} here~~ }
  \item{width}{ ~~Describe \code{width} here~~ }
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
function(janela, height, width) {
	MAX.height <- as.integer( tclvalue( tkwinfo("screenheight", janela) ) )
	MAX.width <- as.integer( tclvalue( tkwinfo("screenwidth", janela) ) )

	wm.x <- as.integer( tclvalue( tkwinfo("x", janela) ) )
	wm.y <- as.integer( tclvalue( tkwinfo("y", janela) ) )
	
	if( height > MAX.height ) height <- MAX.height
	if( width > MAX.width ) width <- MAX.width
	
	limite.sup.x <- round( MAX.width - width )
	limite.inf.x <- round( width )
	limite.sup.y <- round( MAX.height - height )
	limite.sup.y <- round( height )

	# Limitantes para o tamanho da tela. Quem tem tela virtural... #$\%#$\%
	if (wm.x > limite.sup.x) wm.x <- limite.sup.x
	if (wm.x < limite.inf.x) wm.x <- limite.inf.x
	if (wm.y > limite.sup.y) wm.y <- limite.sup.y
	if (wm.y > limite.sup.y) wm.y <- limite.sup.y
	
	posicao <- paste(width, "x", height, "+", wm.x,"+", wm.y, sep="")
	tkwm.geometry(janela,posicao)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
