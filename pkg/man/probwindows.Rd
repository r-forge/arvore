\name{probwindows}
\alias{probwindows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
probwindows()
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
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da �rvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "�rvoRe - AVISO")
		tkfocus(tt)
	} else {
		probWindow <- tktoplevel()
		title <- "�rvoRe - Probabilidade Nodo"
		tkwm.title(probWindow,title)
		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		Probvar <- tclVar(TheTree$Prob[position])

		entry.Value  <- tkentry(probWindow,width="20",textvariable=Probvar)
		tkgrid(tklabel(probWindow,text="Probabilidade"))
		tkgrid(entry.Value)
		OnOK <- function()
		{
			ProbVal <- as.numeric(tclvalue(Probvar))
			if ( (is.numeric(ProbVal)) && (!is.na(ProbVal)) && (ProbVal <= 1) && (ProbVal >= 0) ) {
				tkdestroy(probWindow)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				setprob(TheTree, nodeSec[2], nodeSec[3], ProbVal, .EnvironmentArvoRe)
				tkfocus(tt)
			} else {
				msg <- paste("Este n�o � um valor de probabilidade v�lido '",ProbVal, "'")
				tkmessageBox(message=msg)
				tkfocus(probWindow)
			}
		}
		OK.but <-tkbutton(probWindow,text="    OK    ",command=OnOK)
		tkbind(entry.Value, "<Return>",OnOK)
		
		OnCancel <- function()
		{
			tkdestroy(probWindow)
			tkfocus(tt)
		}
		
		Cancel.but <-tkbutton(probWindow,text=" Cancelar ",command=OnCancel)
		tkbind(probWindow, "<Escape>",OnCancel)

		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)

		posiciona.janela.no.mouse(probWindow, 200, 100)
		tkfocus(probWindow)
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
