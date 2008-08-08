\name{acewindow}
\alias{acewindow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
acewindow(TheTree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
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
function(TheTree) {
	require(abind)
	
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione o nodo de tipo 'Decisão' da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))

		if (( column != 1) && (node.number != 1)) {
			msg <- paste("A tabela apresentada a seguir exibe resultados apenas para o nodo raiz.")
			tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
			tkfocus(tt)
		}
		
		CEtableWindow <- tktoplevel()
		title <- "ÁrvoRe - Análise de Custo-Efetividade"
		tkwm.title(CEtableWindow,title)
		
		# Cria o primeiro frame
		FrameOverAll <- tkframe(CEtableWindow, borderwidth = 0, relief = "groove")
		Frame1 <- tkframe(FrameOverAll, borderwidth = 2, relief = "groove")
		Frame2 <- tkframe(FrameOverAll, borderwidth = 0, relief = "sunken")
		
		# Cria o label
		textlabellista <- "Selecione o procedimento padrão para ACE. \n Ele será a base de comparação para os demais."
		rotulolista <- tklabel(Frame1, text = textlabellista)
		tkgrid(rotulolista, columnspan = 2)
		
		# Cria uma barra de rolagem
		scr <- tkscrollbar(Frame1, repeatinterval=5, command=function(...)tkyview(tl,...))
		
		Data.CEA <- cost.effectiveness.table(TheTree)
		# Cria os elementos da lista
		elementos <- Data.CEA$Node.name
		
		# Determina a altura da listbox
		heightlistbox <- length(elementos)
		larguratexto <- max(nchar(elementos)) + 4
		# Cria uma listbox					
		tl <- tklistbox(Frame1, height = 5, width = larguratexto, selectmode = "single",
						yscrollcommand = function(...)tkset(scr,...), background="white")
		
		# Adiciona os elementos à listbox
		for (i in (1:heightlistbox)) {
		    tkinsert(tl, "end", elementos[i])
		}
		
		# Monta a listbox e a barra de rolagem
		tkgrid(tl, scr, sticky="nse")
		
# 		tkgrid(tklabel(Frame1, text = " "))
		
		# Ajusta a barra de rolagem
		tkgrid.configure(scr, rowspan = 5, sticky="nsw")
		
		# Define o "Elemento 2" como padrão da listbox.
		# Para a listbox o índice começa em zero
		tkselection.set(tl, 0)
		
		# Monta os frames
		tkgrid(Frame1, Frame2, sticky = "nwe", padx = 5, pady = 5)
		tkgrid(FrameOverAll, sticky = "nswe", columnspan = 2)
		
		OnOK <- function() {
			respostaListbox <- Data.CEA$Node.N[as.numeric(tkcurselection(tl))+1]
			
			Data.alternative <- Data.CEA[Data.CEA$Node.N != respostaListbox,]
			Data.standart <- Data.CEA[Data.CEA$Node.N == respostaListbox, ]
			
			ans <- data.frame( 	Strategy = Data.standart$Node.name,
								Cost = Data.standart$Mean.Cost, 
								Incr.Cost = NA, 
								Effectiveness = Data.standart$Mean.Effectiveness, 
								Incr.Eff. = NA, 
								CE.ratio = Data.standart$Mean.Cost / Data.standart$Mean.Effectiveness, 
								ICER = NA
								)
			
			for (i in 1:dim(Data.alternative)[1]) {
				ans.line <- data.frame( Strategy = Data.alternative$Node.name[i],
								Cost = Data.alternative$Mean.Cost[i], 
								Incr.Cost = Data.alternative$Mean.Cost[i] - Data.standart$Mean.Cost, 
								Effectiveness = Data.alternative$Mean.Effectiveness[i], 
								Incr.Eff. = Data.alternative$Mean.Effectiveness[i] - Data.standart$Mean.Effectiveness, 
								CE.ratio = Data.alternative$Mean.Cost[i] / Data.alternative$Mean.Effectiveness[i], 
								ICER = ((Data.alternative$Mean.Cost[i] - Data.standart$Mean.Cost) /
											(Data.alternative$Mean.Effectiveness[i] - Data.standart$Mean.Effectiveness))
								)
				ans <- abind(ans, ans.line, along = 1)
				
			}
			ans <- as.data.frame(ans)
			
			displayInTable(as.matrix(ans), title="Análise de Custo-Efetividade",
						height=10,width=8,nrow=dim(ans)[1],ncol=dim(ans)[2], 
						titlerows = FALSE, titlecols = TRUE, returntt = FALSE)
	    }
			
	    OnCancel <- function() {
			tkdestroy(CEtableWindow)
	# 		tkwm.deiconify(tt)
			tkfocus(tt)
	    }
	    
	  	.Width.but <- 10
		.Height.but <- 1
			
		OK.but <-tkbutton(CEtableWindow,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <-tkbutton(CEtableWindow,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCancel)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		tkbind(CEtableWindow, "<Return>",OnOK)
		tkbind(CEtableWindow, "<Escape>",OnOK)
		
		posiciona.janela.no.mouse(CEtableWindow, 300, 180)
		
		tkfocus(CEtableWindow)
		}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
