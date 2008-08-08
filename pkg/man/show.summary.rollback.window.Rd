\name{show.summary.rollback.window}
\alias{show.summary.rollback.window}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
show.summary.rollback.window(...)
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
	k <- summary.rollback.table(TheTree)
	
	names(k) <- c("Nível", "Nodo N", "Nome do nodo", 
					"Custo Esperado", "Efetividade Esperada", "Razão C-E Esperada", 
					"Nome Nodo Pai", "Probabilidade",
					"Custo", "Efetividade", "Tipo")
					
	displayInTable(as.matrix(k), title="Valores Esperados (Roll-back)",
					height=10,width=8,nrow=dim(k)[1],ncol=dim(k)[2], 
					titlerows = FALSE, titlecols = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
