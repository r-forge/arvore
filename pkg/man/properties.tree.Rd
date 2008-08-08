\name{properties.tree}
\alias{properties.tree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
properties.tree(...)
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
		propertiesWindow <- tktoplevel()
		title <- "ÁrvoRe - Propriedades"
		tkwm.title(propertiesWindow,title)
		
		tclRequire("BWidget")
		
	    frameOverall <- tkframe(propertiesWindow)
	    frameLeft <- tkframe(frameOverall)
	    frameRight <- tkframe(frameOverall)
	    titleframe <- "Método de Cálculo"
	    frameUpper <- tkwidget(frameLeft, "labelframe", borderwidth = 2, relief = "groove", text = titleframe)
	    titleframe <- "Simulação 1-st order"
	    frameSimUpper <- tkwidget(frameLeft, "labelframe", borderwidth = 2, relief = "groove", text = titleframe)
	    titleframe <- "Formato Numérico"
	    frameNumeric <- tkwidget(frameLeft, "labelframe", borderwidth = 2, relief = "groove", text = titleframe)
	    titleframe <- "Formato da árvore"
	    frameTreePlot <- tkwidget(frameLeft, "labelframe", borderwidth = 2, relief = "groove", text = titleframe)
	    titleframe <- "Exibir na árvore"
	    frameTreePlotElements <- tkwidget(frameRight, "labelframe", borderwidth = 2, relief = "groove", text = titleframe)
	    frameLower <- tkframe(frameOverall, borderwidth=2)
	    titleframe <- "Fontes da árvore"
	    frameFontPlot <- tkwidget(frameRight, "labelframe", borderwidth = 2, relief = "groove", text = titleframe)
	    
	    ### Method settings ###
	    metodos <- c("Simple Decision (simple payoff)", "Cost-Effectiveness")
	    method.arvore <- c("SD", "CE")
	    
	    methodBox <- tkwidget(frameUpper, "ComboBox", editable=FALSE, values=metodos, width = 30)
	    labelmethodBox <- tklabel(frameUpper,text="Método")
	    tkgrid(labelmethodBox, methodBox, sticky = "nw", padx = 5, pady = 5)
	    
	    if (.modeltypeArvore == "SD") {
		    selected.method <- "@0"
	    } else {
		    if (.modeltypeArvore == "CE") selected.method <- "@1"
	    }
	    tcl(methodBox, "setvalue", selected.method)
	    
	    ### Numeric format settings ###
		numericSpinBox <- tkwidget(frameNumeric, "SpinBox", editable=FALSE, range = c(0,10,1), width = 3)
		labeldigits <- tklabel(frameNumeric,text="Número de casas decimais")
		tkgrid(labeldigits, numericSpinBox, sticky = "nw", padx = 5, pady = 5)
		tcl(numericSpinBox, "setvalue", paste("@", .digits,sep = ""))
		
	    ### Simulation settings ###
		absorventstatecb <- tkcheckbutton(frameSimUpper)
		absorventstatecbValue <- tclVar(.absorventstateconf)
		tkconfigure(absorventstatecb, variable = absorventstatecbValue)
		tkgrid(absorventstatecb, tklabel(frameSimUpper,text = "Interpretar estado absorvente como morte"))
	    
		### Tree Plot ###
# 		("squared", "normal")
		tkgrid(tklabel(frameTreePlot,text="Ângulos das linhas das ramificação"), row = 0, column = 0, columnspan = 2, sticky = "w")
		
		rb1 <- tkradiobutton(frameTreePlot)
		tpValue <- tclVar(.treeangle)
		tkconfigure(rb1, variable = tpValue, value = "squared")
		tkgrid(rb1, row = 3, column = 0, sticky = "w")
		tkgrid(tklabel( frameTreePlot,text="Retos"), row = 3, column = 1, sticky = "w")
		
		rb2 <- tkradiobutton(frameTreePlot)
		tkconfigure(rb2, variable = tpValue, value = "normal")
		tkgrid(rb2, row = 4, column = 0, sticky = "w")
		tkgrid(tklabel(frameTreePlot,text="Normais"), row = 4, column = 1, sticky = "w")
		
		### Tree Plot Elements ###
# 		tkgrid(tklabel(frameTreePlotElements,text="Exibir na árvore"), row = 0, column = 0, columnspan = 2)
		
		notescb <- tkcheckbutton(frameTreePlotElements)
		notescbValue <- tclVar(.notesconf)
		tkconfigure(notescb, variable = notescbValue)
		tkgrid(notescb, tklabel(frameTreePlotElements,text="Comentários"))
		
		probabilitycb <- tkcheckbutton(frameTreePlotElements)
		probabilitycbValue <- tclVar(.probabilityconf)
		tkconfigure(probabilitycb, variable = probabilitycbValue)
		tkgrid(probabilitycb, tklabel(frameTreePlotElements,text="Probabilidades"))

		payoffscb <- tkcheckbutton(frameTreePlotElements)
		payoffscbValue <- tclVar(.payoffsconf)
		tkconfigure(payoffscb, variable = payoffscbValue)
		tkgrid(payoffscb, tklabel(frameTreePlotElements,text="Payoffs"))
		
		tkgrid(frameUpper, sticky="nwe")
		tkgrid(frameNumeric, sticky="nwe")
		tkgrid(frameSimUpper, sticky="nwe")
		tkgrid(frameTreePlot, sticky="nwe")
		tkgrid(frameTreePlotElements, sticky="nwe")
		tkgrid(frameFontPlot, sticky="nwe")
		
		### Tree Plot Font ###
		font.nameSpinBox <- tkwidget(frameFontPlot, "SpinBox", editable=FALSE, range = c(0,72,1), width = 3)
		labeldigits <- tklabel(frameFontPlot,text="Nome do nodo")
		tkgrid(labeldigits, font.nameSpinBox, sticky = "nw", padx = 5, pady = 5)
		tcl(font.nameSpinBox, "setvalue", paste("@", .node.name.font.size, sep = ""))

		font.payoffsSpinBox <- tkwidget(frameFontPlot, "SpinBox", editable=FALSE, range = c(0,72,1), width = 3)
		labeldigits <- tklabel(frameFontPlot,text="Payoffs (custo e efetividade)")
		tkgrid(labeldigits, font.payoffsSpinBox, sticky = "nw", padx = 5, pady = 5)
		tcl(font.payoffsSpinBox, "setvalue", paste("@", .payoffs.font.size, sep = ""))
		
		font.notesSpinBox <- tkwidget(frameFontPlot, "SpinBox", editable=FALSE, range = c(0,72,1), width = 3)
		labeldigits <- tklabel(frameFontPlot,text="Comentários do nodo")
		tkgrid(labeldigits, font.notesSpinBox, sticky = "nw", padx = 5, pady = 5)
		tcl(font.notesSpinBox, "setvalue", paste("@", .notes.font.size, sep = ""))
		
		# Configurações para o tamanho dos botões.
    	.Width.but <- 10
		.Height.but <- 1

		OnDefault.font <- function () {
			tcl(font.nameSpinBox, "setvalue", paste("@", 12, sep = ""))
			tcl(font.payoffsSpinBox, "setvalue", paste("@", 6, sep = ""))
			tcl(font.notesSpinBox, "setvalue", paste("@", 6, sep = ""))
		}
		
		OnRestore.font <- function () {
			tcl(font.nameSpinBox, "setvalue", paste("@", .node.name.font.size, sep = ""))
			tcl(font.payoffsSpinBox, "setvalue", paste("@", .payoffs.font.size, sep = ""))
			tcl(font.notesSpinBox, "setvalue", paste("@", .notes.font.size, sep = ""))
		}
		
		Restore.font <-tkbutton(frameFontPlot,text="Restaurar", width=.Width.but, height=.Height.but, command=OnRestore.font)
# 		tkgrid(Default.font, sticky = "sw", padx = 5, pady = 5)
		
		Default.font <-tkbutton(frameFontPlot,text="Padrão", width=.Width.but, height=.Height.but, command=OnDefault.font)
		tkgrid(Restore.font, Default.font, sticky = "sw", padx = 5, pady = 5)
		
		OnOK <- function()
		{
			methodChoice <- method.arvore[as.numeric(tclvalue(tcl(methodBox,"getvalue")))+1]
			assign(".modeltypeArvore", methodChoice, .EnvironmentArvoRe)
			
			.digits <- as.numeric(tclvalue(tcl(numericSpinBox,"getvalue")))
			if ((is.numeric(.digits) )&&(!is.na(.digits))) assign(".digits", .digits, .EnvironmentArvoRe)
			
			.treeangle <- tclvalue(tpValue)
			assign(".treeangle", .treeangle, .EnvironmentArvoRe)
			
			.absorventstateconf <- as.numeric(as.character(tclvalue(absorventstatecbValue)))
			assign(".absorventstateconf", .absorventstateconf, .EnvironmentArvoRe)
			
			.notesconf <- as.numeric(as.character(tclvalue(notescbValue)))
			assign(".notesconf", .notesconf, .EnvironmentArvoRe)
			
			.probabilityconf <- as.numeric(as.character(tclvalue(probabilitycbValue)))
			assign(".probabilityconf", .probabilityconf, .EnvironmentArvoRe)
			
			.payoffsconf <- as.numeric(as.character(tclvalue(payoffscbValue)))
			assign(".payoffsconf", .payoffsconf, .EnvironmentArvoRe)
			
			.node.name.font.size <- as.numeric(tclvalue(tcl(font.nameSpinBox,"getvalue")))
			if ((is.numeric(.node.name.font.size) )&&(!is.na(.node.name.font.size))) assign(".node.name.font.size", .node.name.font.size, .EnvironmentArvoRe)
			
			.payoffs.font.size <- as.numeric(tclvalue(tcl(font.payoffsSpinBox,"getvalue")))
			if ((is.numeric(.payoffs.font.size) )&&(!is.na(.payoffs.font.size))) assign(".payoffs.font.size", .payoffs.font.size, .EnvironmentArvoRe)

			.notes.font.size <- as.numeric(tclvalue(tcl(font.notesSpinBox,"getvalue")))
			if ((is.numeric(.notes.font.size) )&&(!is.na(.notes.font.size))) assign(".notes.font.size", .notes.font.size, .EnvironmentArvoRe)
			
			tkdestroy(propertiesWindow)
			refreshF5()
			tkfocus(tt)
		}
    	OnCancel <- function() {
	    	tkdestroy(propertiesWindow)
	    	tkfocus(tt)
    	}
    			
		OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <-tkbutton(frameLower,text="Cancel", width=.Width.but, height=.Height.but, command=OnCancel)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		tkbind(propertiesWindow, "<Return>",OnOK)
		tkbind(propertiesWindow, "<Escape>",OnCancel)

	    tkgrid(frameLeft, frameRight, ipadx = 6, sticky="nwe")
	    tkgrid(frameLower, sticky="nwe", columnspan = 2)
	    tkgrid(frameOverall)

	    tkfocus(propertiesWindow)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
