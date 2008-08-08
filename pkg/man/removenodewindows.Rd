\name{removenodewindows}
\alias{removenodewindows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
removenodewindows(...)
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
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		node.number <- as.numeric(nodeSec[3])
		node.col <- as.numeric(nodeSec[2])
		if (node.col > 1) {
			position <- intersect(which((TheTree$Level == node.col)),which(TheTree$Node.N == node.number))
			Removenamevar <- TheTree$Node.name[position]
			
			msg <- paste("Deseja realmente excluir o nodo '", Removenamevar, "'?", sep = "")
			ans <- tkmessageBox(message=msg, icon="question",type="yesnocancel",default="no")
			ans <- as.character(tclvalue(ans))
			if (ans == "yes") {
				NewTheTree <- remove.node(TheTree, node.col, node.number)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				setremovenode(NewTheTree, .EnvironmentArvoRe)
				refreshF5()
				tkfocus(tt)
			}
		} else {
			msg <- paste("Não é possível remover o nodo raiz.")
			tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
			tkfocus(tt)
		}
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
