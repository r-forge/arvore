\name{addnodewindows}
\alias{addnodewindows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addnodewindows()
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
		node.number <- as.numeric(nodeSec[3])
		node.col <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == node.col)),which(TheTree$Node.N == node.number))
		node.type <- TheTree$Type[position]
		if (node.type == "T") {
			msg <- paste(" O nodo selecionado é de tipo 'Terminal'.\n Altere o tipo do nodo e tente novamente.")
			tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
			tkfocus(tt)
		} else {
			# A janela Tk
			addnodeWindow <- tktoplevel()
			title <- "ÁrvoRe - Novo Nodo"
			tkwm.title(addnodeWindow,title)
			
			NomeVar <- tclVar("New Node")
			NomeEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=NomeVar)
			tkgrid(tklabel(addnodeWindow,text="Nome do nodo"))
			tkgrid(NomeEntryWidget)
			
			ProbabilidadeVar <- tclVar("0.0")
			ProbabilityEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=ProbabilidadeVar)
			tkgrid(tklabel(addnodeWindow,text="Probabilidade"))
			tkgrid(ProbabilityEntryWidget)
			
			UtilidadeVar <- tclVar("0.0")
			UtilityEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=UtilidadeVar)
			tkgrid(tklabel(addnodeWindow,text="Custo / Payoff"))
			tkgrid(UtilityEntryWidget)
	
			EffectivenessVar <- tclVar("0.0")
			EffectivenessEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=EffectivenessVar)
			tkgrid(tklabel(addnodeWindow,text="Efetividade / Payoff"))
			tkgrid(EffectivenessEntryWidget)
		
			NotasVar <- tclVar(" ")
			NotesEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=NotasVar)
			tkgrid(tklabel(addnodeWindow,text="Notas"))
			tkgrid(NotesEntryWidget)		
			
			tkfocus(addnodeWindow)
	
			OnOK <- function()
			{
				NameVal <- tclvalue(NomeVar)
				ProbabilidadeVal <- as.numeric( tclvalue(ProbabilidadeVar) )
				UtilidadeVal <- as.numeric( tclvalue(UtilidadeVar) )
				EffectivenessVal <- as.numeric( tclvalue(EffectivenessVar) )
				NotasVal <- tclvalue(NotasVar)
				
				if ( (ProbabilidadeVal < 0) || (ProbabilidadeVal > 1) ) {
					msg <- paste("Este não é um valor de probabilidade válido '",ProbVal, "'")
					tkmessageBox(message=msg)
					tkfocus(addnodeWindow)
				} else {
					NewTree <- add.node(TheTree,
									node.col = node.col,
									node.number = node.number, 
									node.name = NameVal, 
									node.prob = ProbabilidadeVal, 
									node.type = "C", 
									node.notes = NotasVal,
									node.destiny = " ",
									node.utility = UtilidadeVal,
									node.effectiveness = EffectivenessVal)
					safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
					setaddnode(NewTree, .EnvironmentArvoRe)
					refreshF5()
					tkdestroy(addnodeWindow)
					tkfocus(tt)
				}
					
			}
			OK.but <-tkbutton(addnodeWindow,text="   OK   ",command=OnOK)
			tkbind(NomeEntryWidget, "<Return>",OnOK)
			tkbind(ProbabilidadeVar, "<Return>",OnOK)
			tkbind(UtilityEntryWidget, "<Return>",OnOK)
			tkbind(EffectivenessEntryWidget, "<Return>",OnOK)
			tkbind(NotasVar, "<Return>",OnOK)
	
			OnCancel <- function()
			{
				tkdestroy(addnodeWindow)
				tkfocus(tt)
			}
			
			Cancel.but <-tkbutton(addnodeWindow,text=" Cancel ",command=OnCancel)
			tkbind(addnodeWindow, "<Escape>",OnCancel)
	
			tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
	
			posiciona.janela.no.mouse(addnodeWindow, 250, 230)
	 		}
 		}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
