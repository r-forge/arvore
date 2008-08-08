\name{nodenamewindows}
\alias{nodenamewindows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
nodenamewindows()
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
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		nodenameWindow <- tktoplevel()
		title <- "ÁrvoRe - Nome Nodo"
		tkwm.title(nodenameWindow,title)

		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		Namevar <- tclVar(TheTree$Node.name[position])
		
		entry.Value  <- tkentry(nodenameWindow,width="20",textvariable=Namevar)
		tkgrid(tklabel(nodenameWindow,text="Nome do Nodo"), sticky = "n")
		tkgrid(entry.Value, sticky = "n")
		OnOK <- function()
		{
			NameVal <- as.character(tclvalue(Namevar))
			if ( (is.character(NameVal)) && (!is.na(NameVal)) && (nchar(NameVal) > 0) ) {
				tkdestroy(nodenameWindow)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
 				setnodename(TheTree, nodeSec[2], nodeSec[3], NameVal, .EnvironmentArvoRe)
 				refreshF5()
				tkfocus(tt)
			} else {
				msg <- paste("Este não é um nome de nodo válido '",NameVal, "'")
				tkmessageBox(message=msg)
				tkfocus(nodenameWindow)
			}
		}
		OK.but <-tkbutton(nodenameWindow,text="   OK   ",command=OnOK)
		tkbind(entry.Value, "<Return>",OnOK)
		
		OnCancel <- function()
		{
			tkdestroy(nodenameWindow)
			tkfocus(tt)
		}
		
		Cancel.but <-tkbutton(nodenameWindow, text=" Cancel ", command=OnCancel)
		tkbind(nodenameWindow, "<Escape>",OnCancel)

		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)

		tkfocus(nodenameWindow)
		posiciona.janela.no.mouse(nodenameWindow, 200, 100)
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
