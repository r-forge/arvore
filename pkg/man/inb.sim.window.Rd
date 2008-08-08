\name{inb.sim.window}
\alias{inb.sim.window}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inb.sim.window(Alltreatmentstable)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Alltreatmentstable}{ ~~Describe \code{Alltreatmentstable} here~~ }
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
function(Alltreatmentstable) {
	require(abind)
	
		INBsimtableWindow <- tktoplevel()
		title <- "ÁrvoRe - INB"
		tkwm.title(INBsimtableWindow,title)
		
		# Cria o primeiro frame
		FrameOverAll <- tkframe(INBsimtableWindow, borderwidth = 0, relief = "groove")
		Frame1 <- tkframe(FrameOverAll, borderwidth = 2, relief = "groove")
		Frame2 <- tkframe(FrameOverAll, borderwidth = 0, relief = "sunken")
		
		# Cria o label
		textlabellista <- "Selecione o procedimento padrão para ACE. \n Ele será a base de comparação para os demais."
		rotulolista <- tklabel(Frame1, text = textlabellista)
		tkgrid(rotulolista, columnspan = 2)
		
		# Cria uma barra de rolagem
		scr <- tkscrollbar(Frame1, repeatinterval=5, command=function(...)tkyview(tl,...))
		
		Data.CEA <- Alltreatmentstable
		Data.CEA.Cost <- subset(Data.CEA, Data == "Cost")
		Data.CEA.Effectiveness <- subset(Data.CEA, Data == "Effectiveness")
		Data.CEA.CE <- subset(Data.CEA, Data == "C/E")
		n.treat <- 1:length(Data.CEA.Cost$Treatment)
		
		Data.CEA.Cost <- data.frame(NT = n.treat, Data.CEA.Cost)
		Data.CEA.Effectiveness <- data.frame(NT = n.treat, Data.CEA.Effectiveness)
		Data.CEA.CE <- data.frame(NT = n.treat, Data.CEA.CE)
		
# 		print(Data.CEA.Cost)
# 		print(Data.CEA.Effectiveness)
# 		print(Data.CEA.CE)

		# Cria os elementos da lista
		elementos <- Data.CEA.Cost$Treatment
		
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
		
		# The WTP
		WTPvar <- tclVar(0.1)

		WTPValue  <- tkentry(Frame1,width="20",textvariable=WTPvar)
		tkgrid(tklabel(Frame1,text="Valor do willingness-to-pay (WTP)"),
				columnspan = 2, sticky = "n")
		tkgrid(WTPValue, columnspan = 2, sticky = "n")
		tkgrid(tklabel(Frame1,text="                                             "),
				columnspan = 2, sticky = "n")
						
		# Monta os frames
		tkgrid(Frame1, sticky = "nwe", padx = 5, pady = 5)
		tkgrid(Frame2, sticky = "s", padx = 5, pady = 5)
		tkgrid(FrameOverAll, sticky = "nswe", columnspan = 2)
		
		OnOK <- function() {
			respostaListbox <- n.treat[as.numeric(tkcurselection(tl))+1]
			WTPVal <- as.integer(tclvalue(WTPvar))
			
			Data.alternative.Cost <- subset(Data.CEA.Cost, NT != respostaListbox)			
			Data.standart.Cost <- subset(Data.CEA.Cost, NT == respostaListbox)
			Data.alternative.Effectiveness <- subset(Data.CEA.Effectiveness, NT != respostaListbox)			
			Data.standart.Effectiveness <- subset(Data.CEA.Effectiveness, NT == respostaListbox)
			Data.alternative.CE <- subset(Data.CEA.CE, NT != respostaListbox)			
			Data.standart.CE <- subset(Data.CEA.CE, NT == respostaListbox)
			
			ans <- data.frame( 	Strategy = Data.standart.Cost$Treatment[1],
								Cost = Data.standart.Cost$Mean[1], 
								Incr.Cost = NA, 
								Effectiveness = Data.standart.Effectiveness$Mean[1], 
								Incr.Eff. = NA, 
								CE.ratio = Data.standart.Cost$Mean[1] / Data.standart.Effectiveness$Mean[1], 
								INB = NA,
								Var.INB = NA,
								Sd.INB = NA,
								LL_IC95_INB = NA,
								UL_IC95_INB = NA
								)
			
			for (i in 1:dim(Data.alternative.Cost)[1]) {
				
				inb <- (Data.alternative.Effectiveness$Mean[i] - Data.standart.Effectiveness$Mean[1]) *
						WTPVal - (Data.alternative.Cost$Mean[i] - Data.standart.Cost$Mean[1])
				var.inb <- ( 	WTPVal^2
							) * Data.alternative.Effectiveness$Variance[i] +
							Data.alternative.Cost$Variance[i] -
							2 * WTPVal * ( 00000 )
				alfa <- 0.05 # the significance
				
				ans.line <- data.frame( Strategy = Data.alternative.Cost$Treatment[i],
								Cost = Data.alternative.Cost$Mean[i], 
								Incr.Cost = Data.alternative.Cost$Mean[i] - Data.standart.Cost$Mean[1], 
								Effectiveness = Data.alternative.Effectiveness$Mean[i], 
								Incr.Eff. = Data.alternative.Effectiveness$Mean[i] - Data.standart.Effectiveness$Mean[1], 
								CE.ratio = Data.alternative.Cost$Mean[i] / Data.alternative.Effectiveness$Mean[i], 
								INB = inb,
								Var.INB = var.inb,
								Sd.INB = var.inb^0.5,
								LL_IC95_INB = inb - qnorm(1 - alfa/2) * var.inb^0.5,
								UL_IC95_INB = inb + qnorm(1 - alfa/2) * var.inb^0.5
								)
				ans <- abind(ans, ans.line, along = 1)
				
			}
			ans <- as.data.frame(ans)
			
# 			print(ans)
			
			displayInTable(as.matrix(ans), title="ICER - Análise de Custo-Efetividade",
						height=10,width=8,nrow=dim(ans)[1],ncol=dim(ans)[2], 
						titlerows = FALSE, titlecols = TRUE, returntt = FALSE)
	    }
			
	    OnCancel <- function() {
			tkdestroy(INBsimtableWindow)
	# 		tkwm.deiconify(tt)
			tkfocus(tt)
	    }
	    
	  	.Width.but <- 10
		.Height.but <- 1
			
		OK.but <-tkbutton(Frame2,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <-tkbutton(Frame2,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCancel)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		tkbind(INBsimtableWindow, "<Return>",OnOK)
		tkbind(INBsimtableWindow, "<Escape>",OnOK)
		
		posiciona.janela.no.mouse(INBsimtableWindow, 250, 230)
		
		tkfocus(INBsimtableWindow)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
