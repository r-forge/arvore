\name{summary.simulation.window}
\alias{summary.simulation.window}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.simulation.window(Simlist, tempo1 = Sys.time(), tempo2 = Sys.time(), CicloVal, tipo.nodo = " ", digits = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Simlist}{ ~~Describe \code{Simlist} here~~ }
  \item{tempo1}{ ~~Describe \code{tempo1} here~~ }
  \item{tempo2}{ ~~Describe \code{tempo2} here~~ }
  \item{CicloVal}{ ~~Describe \code{CicloVal} here~~ }
  \item{tipo.nodo}{ ~~Describe \code{tipo.nodo} here~~ }
  \item{digits}{ ~~Describe \code{digits} here~~ }
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
function(Simlist, tempo1 = Sys.time(), tempo2 = Sys.time(), CicloVal, tipo.nodo = " ", digits = 3) {
	require(abind)
	require(gplots)
	
	treatments.sim <- names(Simlist)
	
	windheight <- 300
	windwidth <- 750
	
	summarysimulationWindow <- tktoplevel()
	title <- "ÁrvoRe - Simulação Monte Carlo"
	tkwm.title(summarysimulationWindow,title)
	
	frameOverall <- tkwidget(summarysimulationWindow, "labelframe", borderwidth = 0, relief = "groove")
	frameResume <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove")
	framePanelButton <- tkwidget(frameResume, "labelframe", borderwidth = 0, relief = "groove")
	framebutton <- tkwidget(summarysimulationWindow, "labelframe", borderwidth = 0, relief = "groove")
	
	pBar <- tkwidget(frameResume, "NoteBook", height = windheight, width = windwidth)
	
	tkpack(frameOverall, expand = 1, fill = "both") #, side = "left")
	tkpack(frameResume, expand = 1, fill = "both", side = "top", anchor = "ne")
	tkpack(framebutton, expand = 1, fill = "x", side = "bottom")
	tkpack(pBar, expand = 1, fill = "both", side = "left")
	tkpack(framePanelButton, fill = "both", side = "right") # , anchor = "ne"

	PageNoteBook <- tcl(pBar, "insert", "end", "Page0", "-text", "Nodos")
	
	timecounter <- 1
	
	Alltreatmentstable <- data.frame(Treatment = array(,0), Data = array(,0), Mean = array(,0), 
							Variance = array(,0), Sd = array(,0), Median = array(,0), 
							Min = array(,0),Max = array(,0), 
							Quartil1 = array(,0), Quartil2 = array(,0), Time = array(,0))
							
	for (i in treatments.sim) {
		
		tempo <- tempo2[timecounter] - tempo1[timecounter]
		
		timecounter <- timecounter + 1
		
		# Cria uma página para este tratamento -------------------------------------------------
		position <- which( treatments.sim == i)
		
		pagetclname <- paste("Page",position, sep = "")
		pagelabel <- i
		
		PageNoteBook <- tcl(pBar, "insert", "end", pagetclname, "-text", pagelabel)
		object.page.name <- paste("PageNoteBook", position, sep = "")
		assign(object.page.name, PageNoteBook)
		
		PageNoteBook.Window <- .Tk.newwin(PageNoteBook)
		object.page.window.name <- paste("PageNoteBook.Window", position, sep = "")
		assign(object.page.window.name, PageNoteBook.Window)
		
		frameWindow <- tkwidget(PageNoteBook.Window, "labelframe", borderwidth = 2, relief = "groove", text = "Relatório")
		# -------------------------------------------------
		
		frameUpper <- tkframe(frameWindow, relief="groove", borderwidth = 0)
		frameUpperLeft <- tkwidget(frameUpper, "labelframe", borderwidth = 2, relief = "groove", text = "Custo")
		frameUpperRight <- tkwidget(frameUpper, "labelframe", borderwidth = 2, relief = "groove", text = "Efetividade")
		frameLower <- tkframe(frameWindow, relief="groove", borderwidth=2)
		
		# The node root name
		node.root.name <- paste("Nodo : ", i, sep = "")
		node.root.name.label <- tklabel(frameUpper, text = node.root.name)
		tkgrid(node.root.name.label, sticky = "nw", columnspan = 1)
		
		# The time of simulation
		time.text <- paste("Tempo decorrido (segundos) : ", format(round(tempo, digits = digits), nsmall = digits), sep = "")
		time.sim <- tklabel(frameUpper, text = time.text)
		tkgrid(time.sim, sticky = "nw", columnspan = 1)
		
							
		# A Efetividade -------------------------------------------------
		Mktable <- Simlist[[i]]
		Data <- Mktable$Effectiveness
		# Remover esta linha se sumarizar saídas de funções de simulação
		Data <- apply(Data,2,sum, na.rm = TRUE)
		ntreat <- length(Data)
		statisticsData <- summary(Data)
		
		meanData <- mean(Data, na.rm = TRUE)
		varData <- ( 1 / (ntreat*(ntreat-1)) ) * sum( (Data - meanData)^2)
		sdData <- sqrt(varData)
		medianData <- statisticsData[3]
		minData <- statisticsData[1]
		maxData <- statisticsData[6]
		quartil1 <- statisticsData[2]
		quartil3 <- statisticsData[5]
		
		EvarData <- varData
				
		# Guarda as informações importantes
		line.data.summary <- data.frame(Treatment = pagelabel, Data = "Effectiveness", Mean = meanData, 
							Variance = varData, Sd = sdData, Median = medianData, 
							Min = minData, Max = maxData, 
							Quartil1 = quartil1, Quartil2 = quartil3, Time = tempo)
		Alltreatmentstable <- abind(Alltreatmentstable, line.data.summary, along=1)
		
		lableminsize <- tklabel(frameUpperRight,text = paste(rep("_",50),collapse="",sep=""))
		lableminsize2 <- tklabel(frameUpperRight,text = paste(rep("_",50),collapse="",sep=""))
# 		label0 <- tklabel(frameUpperRight,text= "Tempo decorrido (segundos)")
# 		label1 <- tklabel(frameUpperRight,text= format(tempo, nsmall = digits) )
		label2 <- tklabel(frameUpperRight,text= "Valor Médio")
		label3 <- tklabel(frameUpperRight,text= format(round(meanData, digits = digits), nsmall = digits) )
		label4 <- tklabel(frameUpperRight,text= "Variância")
		label5 <- tklabel(frameUpperRight,text= format(round(varData, digits = digits), nsmall = digits) )
		label6 <- tklabel(frameUpperRight,text= "Desvio Padrão")
		label7 <- tklabel(frameUpperRight,text= format(round(sdData, digits = digits), nsmall = digits) )
		label8 <- tklabel(frameUpperRight,text= "Mediana")
		label9 <- tklabel(frameUpperRight,text= format(round(medianData, digits = digits), nsmall = digits) )
		label10 <- tklabel(frameUpperRight,text= "Mínimo")
		label11 <- tklabel(frameUpperRight,text= format(round(minData, digits = digits), nsmall = digits) )
		label12 <- tklabel(frameUpperRight,text= "Máximo")
		label13 <- tklabel(frameUpperRight,text= format(round(maxData, digits = digits), nsmall = digits) )
		label14 <- tklabel(frameUpperRight,text= "1st. Quartil")
		label15 <- tklabel(frameUpperRight,text= format(round(quartil1, digits = digits), nsmall = digits) )
		label16 <- tklabel(frameUpperRight,text= "3rd. Quartil")
		label17 <- tklabel(frameUpperRight,text= format(round(quartil3, digits = digits), nsmall = digits) )
	
		tkgrid(lableminsize, row = 1, column = 0, columnspan = 2)
# 		tkgrid(label0,  row = 2,  column = 0,sticky="w")
# 		tkgrid(label1,  row = 2,  column = 1,sticky="e")
		tkgrid(label2,  row = 3,  column = 0,sticky="w")
		tkgrid(label3,  row = 3,  column = 1,sticky="e")
		tkgrid(label4,  row = 4,  column = 0,sticky="w")
		tkgrid(label5,  row = 4,  column = 1,sticky="e")
		tkgrid(label6,  row = 5,  column = 0,sticky="w")
		tkgrid(label7,  row = 5,  column = 1,sticky="e")
		tkgrid(label8,  row = 6,  column = 0,sticky="w")
		tkgrid(label9,  row = 6,  column = 1,sticky="e")
		tkgrid(label10, row = 7,  column = 0,sticky="w")
		tkgrid(label11, row = 7,  column = 1,sticky="e")
		tkgrid(label12, row = 8,  column = 0,sticky="w")
		tkgrid(label13, row = 8,  column = 1,sticky="e")
		tkgrid(label14, row = 9,  column = 0,sticky="w")
		tkgrid(label15, row = 9,  column = 1,sticky="e")
		tkgrid(label16, row = 10, column = 0,sticky="w")
		tkgrid(label17, row = 10, column = 1,sticky="e")
		tkgrid(lableminsize2, row = 11, column = 0, columnspan = 2)
		
		# O Custo -------------------------------------------------
		Data <- Mktable$Cost
		# Remover esta linha se sumarizar saídas de funções de simulação
		Data <- apply(Data,2,sum, na.rm = TRUE)
		ntreat <- length(Data)
		statisticsData <- summary(Data)
		
		meanData <- mean(Data, na.rm = TRUE)
		varData <- ( 1 / (ntreat*(ntreat-1)) ) * sum( (Data - meanData)^2)
		sdData <- sqrt(varData)
		medianData <- statisticsData[3]
		minData <- statisticsData[1]
		maxData <- statisticsData[6]
		quartil1 <- statisticsData[2]
		quartil3 <- statisticsData[5]
		
		CvarData <- varData
		
		# Guarda as informações importantes
		line.data.summary <- data.frame(Treatment = pagelabel, Data = "Cost", Mean = meanData, 
							Variance = varData, Sd = sdData, Median = medianData, 
							Min = minData, Max = maxData, 
							Quartil1 = quartil1, Quartil2 = quartil3, Time = tempo)
		Alltreatmentstable <- abind(Alltreatmentstable, line.data.summary, along=1)
		
		lableminsize <- tklabel(frameUpperLeft,text = paste(rep("_",50),collapse="",sep=""))
		lableminsize2 <- tklabel(frameUpperLeft,text = paste(rep("_",50),collapse="",sep=""))
# 		label0 <- tklabel(frameUpperLeft,text= "Tempo decorrido (segundos)")
# 		label1 <- tklabel(frameUpperLeft,text= format(tempo, nsmall = digits) )
		label2 <- tklabel(frameUpperLeft,text= "Valor Médio")
		label3 <- tklabel(frameUpperLeft,text= format(round(meanData, digits = digits), nsmall = digits) )
		label4 <- tklabel(frameUpperLeft,text= "Variância")
		label5 <- tklabel(frameUpperLeft,text= format(round(varData, digits = digits), nsmall = digits) )
		label6 <- tklabel(frameUpperLeft,text= "Desvio Padrão")
		label7 <- tklabel(frameUpperLeft,text= format(round(sdData, digits = digits), nsmall = digits) )
		label8 <- tklabel(frameUpperLeft,text= "Mediana")
		label9 <- tklabel(frameUpperLeft,text= format(round(medianData, digits = digits), nsmall = digits) )
		label10 <- tklabel(frameUpperLeft,text= "Mínimo")
		label11 <- tklabel(frameUpperLeft,text= format(round(minData, digits = digits), nsmall = digits) )
		label12 <- tklabel(frameUpperLeft,text= "Máximo")
		label13 <- tklabel(frameUpperLeft,text= format(round(maxData, digits = digits), nsmall = digits) )
		label14 <- tklabel(frameUpperLeft,text= "1st. Quartil")
		label15 <- tklabel(frameUpperLeft,text= format(round(quartil1, digits = digits), nsmall = digits) )
		label16 <- tklabel(frameUpperLeft,text= "3rd. Quartil")
		label17 <- tklabel(frameUpperLeft,text= format(round(quartil3, digits = digits), nsmall = digits) )
	
		tkgrid(lableminsize, row = 1, column = 0, columnspan = 2)
# 		tkgrid(label0,  row = 2,  column = 0,sticky="w")
# 		tkgrid(label1,  row = 2,  column = 1,sticky="e")
		tkgrid(label2,  row = 3,  column = 0,sticky="w")
		tkgrid(label3,  row = 3,  column = 1,sticky="e")
		tkgrid(label4,  row = 4,  column = 0,sticky="w")
		tkgrid(label5,  row = 4,  column = 1,sticky="e")
		tkgrid(label6,  row = 5,  column = 0,sticky="w")
		tkgrid(label7,  row = 5,  column = 1,sticky="e")
		tkgrid(label8,  row = 6,  column = 0,sticky="w")
		tkgrid(label9,  row = 6,  column = 1,sticky="e")
		tkgrid(label10, row = 7,  column = 0,sticky="w")
		tkgrid(label11, row = 7,  column = 1,sticky="e")
		tkgrid(label12, row = 8,  column = 0,sticky="w")
		tkgrid(label13, row = 8,  column = 1,sticky="e")
		tkgrid(label14, row = 9,  column = 0,sticky="w")
		tkgrid(label15, row = 9,  column = 1,sticky="e")
		tkgrid(label16, row = 10, column = 0,sticky="w")
		tkgrid(label17, row = 10, column = 1,sticky="e")
		tkgrid(lableminsize2, row = 11, column = 0, columnspan = 2)
		
# 		eval( parse(text = markov.termination) )
# 		eval( parse(text = markov.termination) )

		tkgrid(frameUpperLeft, frameUpperRight, sticky="ns")
		tkgrid(frameUpper,sticky="ns")
		tkgrid(frameLower,sticky="ns")
		
		tkpack(frameWindow, expand = 1, fill = "both")
		tkgrid(PageNoteBook.Window)
		
		# The CE -----------------------------------------------------------------------
		# Remover esta linha se sumarizar saídas de funções de simulação
		Data <- apply(Mktable$Cost,2,sum) / apply(Mktable$Effectiveness,2,sum, na.rm = TRUE)
		
		statisticsData <- summary(Data)
		
		meanData <- statisticsData[4]
		varData <- var(Data, na.rm = TRUE, use = "complete.obs")
		sdData <- sqrt(varData)
		medianData <- statisticsData[3]
		minData <- statisticsData[1]
		maxData <- statisticsData[6]
		quartil1 <- statisticsData[2]
		quartil3 <- statisticsData[5]
		
		# Guarda as informações importantes
		line.data.summary <- data.frame(Treatment = pagelabel, Data = "C/E", Mean = meanData, 
							Variance = varData, Sd = sdData, Median = medianData, 
							Min = minData, Max = maxData, 
							Quartil1 = quartil1, Quartil2 = quartil3, Time = tempo)
		Alltreatmentstable <- abind(Alltreatmentstable, line.data.summary, along=1)
		
		
	}
	
	rm(Data, statisticsData, Mktable)
	
	# Ajusta o Alltreatmentstable
	rownames(Alltreatmentstable) <- NULL
	Alltreatmentstable <- as.data.frame(Alltreatmentstable)
	Alltreatmentstable$Treatment <- as.character(Alltreatmentstable$Treatment)
	Alltreatmentstable$Data <- as.character(Alltreatmentstable$Data)
	Alltreatmentstable$Mean <- as.numeric(as.character(Alltreatmentstable$Mean))
	Alltreatmentstable$Variance <- as.numeric(as.character(Alltreatmentstable$Variance))
	Alltreatmentstable$Sd <- as.numeric(as.character(Alltreatmentstable$Sd))
	Alltreatmentstable$Median <- as.numeric(as.character(Alltreatmentstable$Median))
	Alltreatmentstable$Min <- as.numeric(as.character(Alltreatmentstable$Min))
	Alltreatmentstable$Max <- as.numeric(as.character(Alltreatmentstable$Max))
	Alltreatmentstable$Quartil1 <- as.numeric(as.character(Alltreatmentstable$Quartil1))
	Alltreatmentstable$Quartil2 <- as.numeric(as.character(Alltreatmentstable$Quartil2))
	Alltreatmentstable$Time <- as.numeric(as.character(Alltreatmentstable$Time))
	Alltreatmentstable <- Alltreatmentstable[ order(Alltreatmentstable$Data),]
# 	print(Alltreatmentstable)
	assign("Alltreatmentstable", Alltreatmentstable, env = .GlobalEnv)
	
	# The data to plot			
	AllTreatCost <- Alltreatmentstable[Alltreatmentstable$Data == "Cost",]
	AllTreatEffectiveness <- Alltreatmentstable[Alltreatmentstable$Data == "Effectiveness",]
	AllTreatCE <- Alltreatmentstable[Alltreatmentstable$Data == "C/E",]
	
	# Initial colors to treatments points
	treatments.colors.plot <- 1:length(AllTreatCost$Treatment)
	# The treatments names
	treatments.label.plot <- AllTreatCost$Treatment
	
	n.treat <- c(0,length(treatments.sim):1,0,length(treatments.sim))
	for (i in n.treat) {
		pagetclname <- paste("Page",i, sep="")
		tcl(pBar,"raise",pagetclname)
	}
	
	tcl(pBar,"itemconfigure", "Page0", "-state", "disabled") # Set Page0 page to disabled.
	
	OnOK <- function()
	{
		tkdestroy(summarysimulationWindow)
		tkwm.deiconify(tt)
		tkfocus(tt)
	}
	
	OnGraph <- function() {
		selectedpage.number <- tclvalue(tcl(pBar,"raise"))	# Retorna a página selecionada
		selectedpage.number <- as.numeric(substr(selectedpage.number,5,nchar(selectedpage.number)))
		selected.treatment <- treatments.sim[selectedpage.number]
		Mktable <- Simlist[[selected.treatment]]
		
		Cost <- apply(Mktable$Cost,2,sum)
		Effectiveness <- apply(Mktable$Effectiveness,2,sum)
	
# OnGraph ----------------------------------------------------------------------------------------------------------------		tkfocus(summarysimulationWindow)
		graphsimulationWindow <- tktoplevel()
		title.window <- "ÁrvoRe - MC Simulação - Gráficos"
		tkwm.title(graphsimulationWindow, title.window)
		
		frameOverall <- tkwidget(graphsimulationWindow, "labelframe", borderwidth = 0, relief = "groove")
		frameResume <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove", text = "Tipos de Gráficos")
		frameDistribution <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove", 
										text = "Distribuição")
		frameOtherGraphs <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove", 
										text = "Custo-Efetividade")
		frameLower <- tkframe(frameOverall, relief="groove", borderwidth = 0)
			
		OnShowIt <- function(type = "Other", SurvivalData = Mktable$Survival,...) {
			
			aGraphWindow <- tktoplevel()
			title.window <- "ÁrvoRe - MC Simulação - Graphics"
			tkwm.title(aGraphWindow, title.window)
			
			frametext <- "Gráfico"
			frameOverall <- tkwidget(aGraphWindow, "labelframe", borderwidth = 2, relief = "groove", 
									labelanchor = "n", text = frametext)
			frameButton <- tkwidget(aGraphWindow, "labelframe", borderwidth = 0, relief = "groove")
			
			tkgrid(frameOverall, sticky = "nwe")
			tkgrid(frameButton, sticky = "swe")
			
			# Image setings.
			g.imgHeight <- 600/2
			g.imgWidth <- 800/2
			
			# Canvas window configurations
			C.Height <- min(c(g.imgHeight, 768))
			C.Width <- min(c(g.imgWidth, 1024))
			Borderwidth <- 2
			
			# scrollbar objects
			fHscroll <- tkscrollbar(frameOverall, orient="horiz", command = function(...)tkxview(fCanvas,...) )
			fVscroll <- tkscrollbar(frameOverall, command = function(...)tkyview(fCanvas,...) )
			fCanvas <- tkcanvas(frameOverall, relief = "sunken", borderwidth = Borderwidth, 
								width = C.Width, height = C.Height,
								xscrollcommand = function(...)tkset(fHscroll,...), 
								yscrollcommand = function(...)tkset(fVscroll,...)
								)
								
			# Pack the scroll bars.
			tkpack(fHscroll, side = "bottom", fill = "x")
			tkpack(fVscroll, side = "right", fill = "y")
			# Pack the canvas
			tkpack(fCanvas, anchor = "center", side = "right", fill = "both", expand = 1)
					
			# Image file name setings.
			.Filename <- paste(tempdir(),"\\", "grafico.arvore.png", sep="")
			
			# What plot?
			plot.it.to.image <- function(.Filename, img.type = "png", img.quality = 90,
											img.width = 600, img.height = 600, SurvivalData = Mktable$Survival, 
											...) {
# 				print(.Filename)
# 				print(type)
# 				print(img.type)
				
				if( type == "Distrib.cost") {
					if (img.type == "png") {
						png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
							Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
							xlabel <- "Cost"
							hist(Cost, main = Graphtitle, xlab = xlabel)
						dev.off()
					} else {
						if (img.type == "jpg") {
							jpeg(filename = .Filename, width = img.width, height = img.height,
							     units = "px", pointsize = 12, quality = img.quality, bg = "white",
							     res = NA, restoreConsole = FALSE)								
					     		Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
								xlabel <- "Cost"
								hist(Cost, main = Graphtitle, xlab = xlabel)
							dev.off()
						} else {
							bmp(filename = .Filename, width = img.width, height = img.height,
						    	units = "px", pointsize = 12, bg = "white", res = NA,
						    	restoreConsole = FALSE)
					     		Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
								xlabel <- "Cost"
								hist(Cost, main = Graphtitle, xlab = xlabel)
							dev.off()
						}
					}
				}
				
				if( type == "Distrib.effectiveness") {
					if (img.type == "png") {
						png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
						Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
						xlabel <- "Efetividade"
						hist(Effectiveness, main = Graphtitle, xlab = xlabel)
						dev.off()
					} else {
						if (img.type == "jpg") {
							jpeg(filename = .Filename, width = img.width, height = img.height,
							     units = "px", pointsize = 12, quality = img.quality, bg = "white",
							     res = NA, restoreConsole = FALSE)								
								Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
								xlabel <- "Efetividade"
								hist(Effectiveness, main = Graphtitle, xlab = xlabel)
							dev.off()
						} else {
							bmp(filename = .Filename, width = img.width, height = img.height,
						    	units = "px", pointsize = 12, bg = "white", res = NA,
						    	restoreConsole = FALSE)
								Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
								xlabel <- "Efetividade"
								hist(Effectiveness, main = Graphtitle, xlab = xlabel)
							dev.off()
						}
					}
				}
				
				if( type == "CE.scatterplot") {
					if (img.type == "png") {
						png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
							Graphtitle <- "CE Scatterplot"
							xlabel <- "Efetividade"
							ylabel <- "Custo"
							plot(Effectiveness,Cost, col = "red", pch = "*", main = Graphtitle,
									xlab = xlabel, ylab = ylabel)
						dev.off()
					} else {
						if (img.type == "jpg") {
							jpeg(filename = .Filename, width = img.width, height = img.height,
							     units = "px", pointsize = 12, quality = img.quality, bg = "white",
							     res = NA, restoreConsole = FALSE)								
								Graphtitle <- "CE Scatterplot"
								xlabel <- "Efetividade"
								ylabel <- "Custo"
								plot(Effectiveness,Cost, col = "red", pch = "*", main = Graphtitle,
										xlab = xlabel, ylab = ylabel)
							dev.off()
						} else {
							bmp(filename = .Filename, width = img.width, height = img.height,
						    	units = "px", pointsize = 12, bg = "white", res = NA,
						    	restoreConsole = FALSE)
								Graphtitle <- "CE Scatterplot"
								xlabel <- "Efetividade"
								ylabel <- "Custo"
								plot(Effectiveness,Cost, col = "red", pch = "*", main = Graphtitle,
										xlab = xlabel, ylab = ylabel)
							dev.off()
						}
					}
				}
				
				if( type == "Distrib.CER") {
					if (img.type == "png") {
						png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
							Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
							xlabel <- "Razão Custo-Efetividade ($)"
							hist(Cost/Effectiveness, main = Graphtitle, xlab = xlabel)
						dev.off()
					} else {
						if (img.type == "jpg") {
							jpeg(filename = .Filename, width = img.width, height = img.height,
							     units = "px", pointsize = 12, quality = img.quality, bg = "white",
							     res = NA, restoreConsole = FALSE)								
								Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
								xlabel <- "Razão Custo-Efetividade ($)"
								hist(Cost/Effectiveness, main = Graphtitle, xlab = xlabel)
							dev.off()
						} else {
							bmp(filename = .Filename, width = img.width, height = img.height,
						    	units = "px", pointsize = 12, bg = "white", res = NA,
						    	restoreConsole = FALSE)
								Graphtitle <- paste("Histograma de ", selected.treatment, sep = "")
								xlabel <- "Razão Custo-Efetividade ($)"
								hist(Cost/Effectiveness, main = Graphtitle, xlab = xlabel)
							dev.off()
						}
					}
				}
				
				if( type == "Survival.Curve") {
					if (img.type == "png") {
						png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
							Graphtitle <- paste("Número de Sobreviventes \n", selected.treatment, sep = "")
							xlabel <- "Ciclos"
# 							hist(SurvivalData, main = Graphtitle, xlab = xlabel)
							barplot(SurvivalData, main = Graphtitle, col = "red", space = c(0,0),
									xlab = xlabel)
						dev.off()
# 						print(SurvivalData)
					} else {
						if (img.type == "jpg") {
							jpeg(filename = .Filename, width = img.width, height = img.height,
							     units = "px", pointsize = 12, quality = img.quality, bg = "white",
							     res = NA, restoreConsole = FALSE)								
								Graphtitle <- paste("Número de Sobreviventes \n", selected.treatment, sep = "")
								xlabel <- "Ciclos"
	# 							hist(Cost/Effectiveness, main = Graphtitle, xlab = xlabel)
								barplot(SurvivalData, main = Graphtitle, col = "red", space = c(0,0),
										xlab = xlabel)
							dev.off()
						} else {
							bmp(filename = .Filename, width = img.width, height = img.height,
						    	units = "px", pointsize = 12, bg = "white", res = NA,
						    	restoreConsole = FALSE)
								Graphtitle <- paste("Número de Sobreviventes \n", selected.treatment, sep = "")
								xlabel <- "Ciclos"
	# 							hist(Cost/Effectiveness, main = Graphtitle, xlab = xlabel)
								barplot(SurvivalData, main = Graphtitle, col = "red", space = c(0,0),
										xlab = xlabel)
							dev.off()
						}
					}
				}
				
			
			
				
			}
			
			# Default img type
			img.type <- "png"
			plot.it.to.image(.Filename = .Filename, type = type, img.type = img.type,
								img.width = g.imgWidth, img.height = g.imgHeight)
			
			image1 <- tclVar()
			tcl("image","create","photo",image1,file=.Filename)
			tkcreate(fCanvas, "image", g.imgWidth/2, g.imgHeight/2, image = image1, anchor = "center")
			tkconfigure(fCanvas, scrollregion = c(0,0,g.imgWidth,g.imgHeight))
			
			
			OnOK <- function() {
				file.remove(.Filename)
				tkdestroy(aGraphWindow)
				tkwm.deiconify(graphsimulationWindow)
				tkfocus(graphsimulationWindow)
			}
			
			OnExportGraphic <- function(...) {
				exportImgGraphWindow <- tktoplevel()
				title <- "ÁrvoRe - Exportar Imagem"
				tkwm.title(exportImgGraphWindow,title)
				
				frameOverall <- tkframe(exportImgGraphWindow)
				frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=0)
				frameUpperLeft <- tkframe(frameUpper, relief="groove", borderwidth=2)
				frameUpperRigth <- tkframe(frameUpper, relief="groove", borderwidth=2)
				frameLower <- tkframe(frameOverall, relief="groove", borderwidth=0)
				
				tkgrid( tklabel(frameUpper,text="Formato de imagem"),sticky="n", columnspan = 2)
				
				rbValue <- tclVar("jpg")
				QualityValue <- tclVar("90")
				
				rb1 <- tkradiobutton(frameUpper)
				tkconfigure(rb1,variable=rbValue,value="bmp")
				tkgrid(	tklabel(frameUpperLeft,text="Bitmap .bmp "),rb1, sticky = "ne")
				
				rb2 <- tkradiobutton(frameUpper)
				tkconfigure(rb2,variable=rbValue,value="jpg")
				tkgrid(	tklabel(frameUpperLeft,text="Jpeg .jpg "),rb2, sticky = "ne")
				
				rb3 <- tkradiobutton(frameUpper)
				tkconfigure(rb3,variable=rbValue,value="png")
				tkgrid(	tklabel(frameUpperLeft,text="Portable network graphics .png "),rb3, sticky = "ne")
				
				SliderValueLabel <- tklabel(frameUpperRigth, text = as.character(tclvalue(QualityValue)) )
				sliderlabel <- tklabel(frameUpperRigth, text = "Valor da qualidade de imagem : ")
				sliderlabel2 <- tklabel(frameUpperRigth,text = "\%")
				tkgrid(sliderlabel, SliderValueLabel, sliderlabel2)
				tkconfigure(SliderValueLabel, textvariable = QualityValue)
				sliderImg <- tkscale(frameUpperRigth, from = 100, to = 1,
				                   showvalue = F, variable = QualityValue,
				                   resolution = 1, orient = "horizontal")
				tkgrid(sliderImg,sticky="ew")
			
				tkgrid(frameUpperLeft, frameUpperRigth,sticky="ns")
				tkgrid(frameUpper,sticky="ns")
				tkgrid(frameLower,sticky="ns")
				
				Onformat <- function() {
					ansVar <- as.character(tclvalue(rbValue))
					if (ansVar != "jpg") {
						tkconfigure(SliderValueLabel, state = "disabled")
						tkconfigure(sliderlabel, state = "disabled")
						tkconfigure(sliderlabel2, state = "disabled")
						tkconfigure(SliderValueLabel, state = "disabled")
						tkconfigure(sliderImg, state = "disabled")
					} else {
						tkconfigure(SliderValueLabel, state = "normal")
						tkconfigure(sliderlabel, state = "normal")
						tkconfigure(sliderlabel2, state = "normal")
						tkconfigure(SliderValueLabel, state = "normal")
						tkconfigure(sliderImg, state = "normal")
					}
				}
				
				OnOK <- function(...)
				{
					ImgFormatselected <- as.character(tclvalue(rbValue))
					ImgQualityselected <- as.numeric(as.character(tclvalue(QualityValue)))
					if (ImgFormatselected == "png") {
						.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Portable network graphics Image Files} {.png}} {{All files} *}"))
						if (!nchar(.Filename))
							tkfocus(aGraphWindow)
						else {
							ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
							if ( ans != ".png" ) .Filename <- paste(.Filename, ".png", sep="")
									
							if (!file.exists(.Filename)) file.remove(.Filename)
							
							plot.it.to.image(.Filename = .Filename, type = type, img.type = ImgFormatselected, img.width = 600, img.height = 600)
						}
					} else {
						if (ImgFormatselected == "jpg") {
							.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Jpeg Image Files} {.jpg}} {{All files} *}"))
							if (!nchar(.Filename))
								tkfocus(aGraphWindow)
							else {
								ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
								if ( ans != ".jpg" ) .Filename <- paste(.Filename, ".jpg", sep="")
										
								if (!file.exists(.Filename)) file.remove(.Filename)
								
								plot.it.to.image(.Filename = .Filename, type = type, img.type = ImgFormatselected, img.width = 600, img.height = 600,
													img.quality = ImgQualityselected)
							}
						} else {
							.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Bitmap Image Files} {.bmp}} {{All files} *}"))
							if (!nchar(.Filename))
								tkfocus(aGraphWindow)
							else {
								ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
								if ( ans != ".bmp" ) .Filename <- paste(.Filename, ".bmp", sep="")
										
								if (!file.exists(.Filename)) file.remove(.Filename)
								
								plot.it.to.image(.Filename = .Filename, type = type, img.type = ImgFormatselected, img.width = 600, img.height = 600)
							}
						}
					}
					tkdestroy(exportImgGraphWindow)
					tkwm.deiconify(aGraphWindow)
					tkfocus(aGraphWindow)
				}
				
				OnCancel <- function()
				{
					tkdestroy(exportImgGraphWindow)
					tkwm.deiconify(aGraphWindow)
					tkfocus(aGraphWindow)
				}
				
				.Width.but <- 10
				.Height.but <- 1
				
				OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
				tkbind(exportImgGraphWindow, "<Return>",OnOK)
				Cancel.but <-tkbutton(frameLower,text="Cancel", width=.Width.but, height=.Height.but, command=OnCancel)
				tkbind(exportImgGraphWindow, "<Escape>",OnCancel)
				tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5, columnspan = 2, sticky = "s")
				
				tkbind(rb1, "<Enter>",Onformat)
				tkbind(rb2, "<Enter>",Onformat)
				tkbind(rb3, "<Enter>",Onformat)
				tkbind(rb1, "<Leave>",Onformat)
				tkbind(rb2, "<Leave>",Onformat)
				tkbind(rb3, "<Leave>",Onformat)
				
				tkgrid(frameOverall)
				tkfocus(exportImgGraphWindow)
# 				posiciona.janela.no.mouse(exportImgGraphWindow)
			}
			
		    .Width.but <- 10
			.Height.but <- 1
			
			OK.but <- tkbutton(frameButton,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
			Export.but <- tkbutton(frameButton,text="Exportar...", width=.Width.but, height=.Height.but, command=OnExportGraphic)
	
			tkgrid(OK.but, Export.but, sticky = "s", padx = 5, pady = 5)
# 			tkconfigure(Export.but, state = "disabled")
	
			tkbind(aGraphWindow, "<Return>", OnOK)
			tkbind(aGraphWindow, "<Escape>", OnCancel)
	
			tkwm.deiconify(aGraphWindow)
			tkfocus(aGraphWindow)
			
		}
		
		OnOK <- function() {
			tkdestroy(graphsimulationWindow)
			tkfocus(summarysimulationWindow)
		}
		
		OnCancel <- function() {
			tkdestroy(graphsimulationWindow)
			tkfocus(summarysimulationWindow)
		}
		
		OnDistrib.cost <- function() {
			OnShowIt(type = "Distrib.cost")		
		}
		
		OnDistrib.effectiveness <- function() {
			OnShowIt(type = "Distrib.effectiveness")
		}
		
		OnDistrib.CER <- function() {
			OnShowIt(type = "Distrib.CER")
		}
	
		OnDistrib.incrementals <- function() {
			OnShowIt(type = "Distrib.incrementals")
		}
		
		OnCE <- function() {
			CEGraphWindow <- tktoplevel()
			title.window <- "ÁrvoRe - MC Simulação - Graphics"
			tkwm.title(CEGraphWindow, title.window)
			
			frametext <- "Gráfico"
			frameOverall <- tkwidget(CEGraphWindow, "labelframe", borderwidth = 2, relief = "groove", 
									labelanchor = "n", text = frametext)
			frameButton <- tkwidget(CEGraphWindow, "labelframe", borderwidth = 0, relief = "groove")
			
			tkgrid(frameOverall, sticky = "nwe")
			tkgrid(frameButton, sticky = "swe")
			
			# Image setings.
			g.imgHeight <- 600/2
			g.imgWidth <- 800/2
			
			# Canvas window configurations
			C.Height <- min(c(g.imgHeight, 768))
			C.Width <- min(c(g.imgWidth, 1024))
			Borderwidth <- 2
			
			# scrollbar objects
			fHscroll <- tkscrollbar(frameOverall, orient="horiz", command = function(...)tkxview(fCanvas,...) )
			fVscroll <- tkscrollbar(frameOverall, command = function(...)tkyview(fCanvas,...) )
			fCanvas <- tkcanvas(frameOverall, relief = "sunken", borderwidth = Borderwidth, 
								width = C.Width, height = C.Height,
								xscrollcommand = function(...)tkset(fHscroll,...), 
								yscrollcommand = function(...)tkset(fVscroll,...)
								)
								
			# Pack the scroll bars.
			tkpack(fHscroll, side = "bottom", fill = "x")
			tkpack(fVscroll, side = "right", fill = "y")
			# Pack the canvas
			tkpack(fCanvas, anchor = "center", side = "right", fill = "both", expand = 1)
					
			# Image file name setings.
			.Filename <- paste(tempdir(),"\\", "grafico.arvore.png", sep="")
			
			# The data to plot			
			AllTreatCost <- Alltreatmentstable[Alltreatmentstable$Data == "Cost",]
			AllTreatEffectiveness <- Alltreatmentstable[Alltreatmentstable$Data == "Effectiveness",]
			# Initial colors to treatments points
			treatments.colors.plot <- 1:length(AllTreatCost$Treatment)
			# The treatments names
			treatments.label.plot <- AllTreatCost$Treatment
			
			# What plot?
			plot.it.to.image <- function(AllTreatEffectiveness, AllTreatCost, treatments.colors.plot,
											treatments.label.plot,
											.Filename, img.type = "png", img.quality = 90, 
											img.width = 600, img.height = 600, ...) {
				
				if (img.type == "png") {
					png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
						Graphtitle <- "Plano Custo-Efetividade"
						xlabel <- "Efetividade"
						ylabel <- "Custo"
						plot(AllTreatEffectiveness$Mean, AllTreatCost$Mean, 
								col = treatments.colors.plot, pch = "*", main = Graphtitle,
								xlab = xlabel, ylab = ylabel)
						smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
						             legend = c(treatments.label.plot), #legend parameters
							     fill=c(treatments.colors.plot),                        #legend parameters
							     bg = "gray")								
					dev.off()
				} else {
					if (img.type == "jpg") {
						jpeg(filename = .Filename, width = img.width, height = img.height,
						     units = "px", pointsize = 12, quality = img.quality, bg = "white",
						     res = NA, restoreConsole = FALSE)								
							Graphtitle <- "Plano Custo-Efetividade"
							xlabel <- "Efetividade"
							ylabel <- "Custo"
							plot(AllTreatEffectiveness$Mean, AllTreatCost$Mean, 
									col = treatments.colors.plot, pch = "*", main = Graphtitle,
									xlab = xlabel, ylab = ylabel)
									
							smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
							             legend = c(treatments.label.plot), #legend parameters
								     fill=c(treatments.colors.plot),                        #legend parameters
								     bg = "gray")								
									
						dev.off()
					} else {
						bmp(filename = .Filename, width = img.width, height = img.height,
					    	units = "px", pointsize = 12, bg = "white", res = NA,
					    	restoreConsole = FALSE)
							Graphtitle <- "Plano Custo-Efetividade"
							xlabel <- "Efetividade"
							ylabel <- "Custo"
							plot(AllTreatEffectiveness$Mean, AllTreatCost$Mean, 
									col = treatments.colors.plot, pch = "*", main = Graphtitle,
									xlab = xlabel, ylab = ylabel)
									
							smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
							             legend = c(treatments.label.plot), #legend parameters
								     fill=c(treatments.colors.plot),                        #legend parameters
								     bg = "gray")								
									
						dev.off()
							}
				}
			}
			
			# Default img type
			img.type <- "png"
			plot.it.to.image(AllTreatEffectiveness, AllTreatCost, treatments.colors.plot, treatments.label.plot, 
								.Filename = .Filename, type = type, img.type = img.type,
								img.width = g.imgWidth, img.height = g.imgHeight)
			
			image1 <- tclVar()
			tcl("image","create","photo",image1,file=.Filename)
			tkcreate(fCanvas, "image", g.imgWidth/2, g.imgHeight/2, image = image1, anchor = "center")
			tkconfigure(fCanvas, scrollregion = c(0,0,g.imgWidth,g.imgHeight))
			
			
			OnOK <- function() {
				file.remove(.Filename)
				tkdestroy(CEGraphWindow)
				tkwm.deiconify(graphsimulationWindow)
				tkfocus(graphsimulationWindow)
			}
			
			OnExportGraphic <- function(...) {
				exportImgGraphWindow <- tktoplevel()
				title <- "ÁrvoRe - Exportar Imagem"
				tkwm.title(exportImgGraphWindow,title)
				
				frameOverall <- tkframe(exportImgGraphWindow)
				frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=0)
				frameUpperLeft <- tkframe(frameUpper, relief="groove", borderwidth=2)
				frameUpperRigth <- tkframe(frameUpper, relief="groove", borderwidth=2)
				frameLower <- tkframe(frameOverall, relief="groove", borderwidth=0)
				
				tkgrid( tklabel(frameUpper,text="Formato de imagem"),sticky="n", columnspan = 2)
				
				rbValue <- tclVar("jpg")
				QualityValue <- tclVar("90")
				
				rb1 <- tkradiobutton(frameUpper)
				tkconfigure(rb1,variable=rbValue,value="bmp")
				tkgrid(	tklabel(frameUpperLeft,text="Bitmap .bmp "),rb1, sticky = "ne")
				
				rb2 <- tkradiobutton(frameUpper)
				tkconfigure(rb2,variable=rbValue,value="jpg")
				tkgrid(	tklabel(frameUpperLeft,text="Jpeg .jpg "),rb2, sticky = "ne")
				
				rb3 <- tkradiobutton(frameUpper)
				tkconfigure(rb3,variable=rbValue,value="png")
				tkgrid(	tklabel(frameUpperLeft,text="Portable network graphics .png "),rb3, sticky = "ne")
				
				SliderValueLabel <- tklabel(frameUpperRigth, text = as.character(tclvalue(QualityValue)) )
				sliderlabel <- tklabel(frameUpperRigth, text = "Valor da qualidade de imagem : ")
				sliderlabel2 <- tklabel(frameUpperRigth,text = "\%")
				tkgrid(sliderlabel, SliderValueLabel, sliderlabel2)
				tkconfigure(SliderValueLabel, textvariable = QualityValue)
				sliderImg <- tkscale(frameUpperRigth, from = 100, to = 1,
				                   showvalue = F, variable = QualityValue,
				                   resolution = 1, orient = "horizontal")
				tkgrid(sliderImg,sticky="ew")
			
				tkgrid(frameUpperLeft, frameUpperRigth,sticky="ns")
				tkgrid(frameUpper,sticky="ns")
				tkgrid(frameLower,sticky="ns")
				
				Onformat <- function() {
					ansVar <- as.character(tclvalue(rbValue))
					if (ansVar != "jpg") {
						tkconfigure(SliderValueLabel, state = "disabled")
						tkconfigure(sliderlabel, state = "disabled")
						tkconfigure(sliderlabel2, state = "disabled")
						tkconfigure(SliderValueLabel, state = "disabled")
						tkconfigure(sliderImg, state = "disabled")
					} else {
						tkconfigure(SliderValueLabel, state = "normal")
						tkconfigure(sliderlabel, state = "normal")
						tkconfigure(sliderlabel2, state = "normal")
						tkconfigure(SliderValueLabel, state = "normal")
						tkconfigure(sliderImg, state = "normal")
					}
				}
				
				OnOK <- function(...)
				{
					ImgFormatselected <- as.character(tclvalue(rbValue))
					ImgQualityselected <- as.numeric(as.character(tclvalue(QualityValue)))
					if (ImgFormatselected == "png") {
						.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Portable network graphics Image Files} {.png}} {{All files} *}"))
						if (!nchar(.Filename))
							tkfocus(CEGraphWindow)
						else {
							ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
							if ( ans != ".png" ) .Filename <- paste(.Filename, ".png", sep="")
									
							if (!file.exists(.Filename)) file.remove(.Filename)
							
							plot.it.to.image(AllTreatEffectiveness, AllTreatCost, treatments.colors.plot, treatments.label.plot,
												.Filename = .Filename, type = type, img.type = ImgFormatselected)
						}
					} else {
						if (ImgFormatselected == "jpg") {
							.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Jpeg Image Files} {.jpg}} {{All files} *}"))
							if (!nchar(.Filename))
								tkfocus(CEGraphWindow)
							else {
								ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
								if ( ans != ".jpg" ) .Filename <- paste(.Filename, ".jpg", sep="")
										
								if (!file.exists(.Filename)) file.remove(.Filename)
								
								plot.it.to.image(AllTreatEffectiveness, AllTreatCost, treatments.colors.plot, treatments.label.plot,
													.Filename = .Filename, type = type, img.type = ImgFormatselected,
													img.quality = ImgQualityselected)
							}
						} else {
							.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Bitmap Image Files} {.bmp}} {{All files} *}"))
							if (!nchar(.Filename))
								tkfocus(CEGraphWindow)
							else {
								ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
								if ( ans != ".bmp" ) .Filename <- paste(.Filename, ".bmp", sep="")
										
								if (!file.exists(.Filename)) file.remove(.Filename)
								
								plot.it.to.image(AllTreatEffectiveness, AllTreatCost, treatments.colors.plot, treatments.label.plot,
													.Filename = .Filename, type = type, img.type = ImgFormatselected)
							}
						}
					}
					tkdestroy(exportImgGraphWindow)
					tkwm.deiconify(CEGraphWindow)
					tkfocus(CEGraphWindow)
				}
				
				OnCancel <- function()
				{
					tkdestroy(exportImgGraphWindow)
					tkwm.deiconify(CEGraphWindow)
					tkfocus(CEGraphWindow)
				}
				
				.Width.but <- 10
				.Height.but <- 1
				
				OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
				tkbind(exportImgGraphWindow, "<Return>",OnOK)
				Cancel.but <-tkbutton(frameLower,text="Cancel", width=.Width.but, height=.Height.but, command=OnCancel)
				tkbind(exportImgGraphWindow, "<Escape>",OnCancel)
				tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5, columnspan = 2, sticky = "s")
				
				tkbind(rb1, "<Enter>",Onformat)
				tkbind(rb2, "<Enter>",Onformat)
				tkbind(rb3, "<Enter>",Onformat)
				tkbind(rb1, "<Leave>",Onformat)
				tkbind(rb2, "<Leave>",Onformat)
				tkbind(rb3, "<Leave>",Onformat)
				
				tkgrid(frameOverall)
				tkfocus(exportImgGraphWindow)
# 				posiciona.janela.no.mouse(exportImgGraphWindow)
			}
			
		    .Width.but <- 10
			.Height.but <- 1
			
			OK.but <- tkbutton(frameButton,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
			Export.but <- tkbutton(frameButton,text="Exportar...", width=.Width.but, height=.Height.but, command=OnExportGraphic)
	
			tkgrid(OK.but, Export.but, sticky = "s", padx = 5, pady = 5)
# 			tkconfigure(Export.but, state = "disabled")
	
			tkbind(CEGraphWindow, "<Return>", OnOK)
			tkbind(CEGraphWindow, "<Escape>", OnCancel)
	
			tkwm.deiconify(CEGraphWindow)
			tkfocus(CEGraphWindow)
			
		}
		
		OnCE.scatterplot <- function() {
			OnShowIt(type = "CE.scatterplot")
		}
		
		OnAccept.Curve <- function(Alltreatmentstable) {
			aceptability.sim.window(Alltreatmentstable)
		}
		
		OnSurvival.Curve <- function() {
			SurvivalData <- Mktable$Survival
			OnShowIt(type = "Survival.Curve", SurvivalData = SurvivalData)
		}
		
		
		# Button label
		label.but1 <- "Custo"
		label.but2 <- "Efetividade"
		label.but3 <- "Razão Custo-Efetividade"
		label.but4 <- "Incrementals"
		label.but5 <- "Custo-Efetividade"
		label.but6 <- "Scatterplot C-E"
		label.but7 <- "Curva de aceitabilidade"
		label.but8 <- "Curva de sobrevivência"
			
	    .Width.but <- max( c( nchar(label.but1), nchar(label.but2), nchar(label.but3), nchar(label.but4), 
	    					nchar(label.but5), nchar(label.but6), nchar(label.but7)) )
		.Height.but <- 1
		
		# The buttons
		Distrib.cost.but <- tkbutton(frameDistribution, text = label.but1, 
			width=.Width.but, height=.Height.but, command = OnDistrib.cost)
		Distrib.effectiveness.but <- tkbutton(frameDistribution,text = label.but2, 
			width=.Width.but, height=.Height.but, command = OnDistrib.effectiveness)
		Distrib.CER.but <- tkbutton(frameDistribution,text = label.but3, 
			width =.Width.but, height=.Height.but, command = OnDistrib.CER)
		Distrib.incrementals.but <- tkbutton(frameDistribution, text = label.but4, 
			width=.Width.but, height=.Height.but, command = OnDistrib.incrementals)
		CE.but <- tkbutton(frameOtherGraphs, text = label.but5, 
			width=.Width.but, height=.Height.but, command = OnCE)
		CE.scatterplot.but <- tkbutton(frameOtherGraphs,text=label.but6, 
			width=.Width.but, height=.Height.but, command = OnCE.scatterplot)
		Accept.Curve.but <- tkbutton(frameOtherGraphs,text=label.but7, 
			width=.Width.but, height=.Height.but, command = function() OnAccept.Curve(Alltreatmentstable))
		Survival.Curve.but <- tkbutton(frameOtherGraphs,text=label.but8, 
			width=.Width.but, height=.Height.but, command = OnSurvival.Curve)
		
		tkgrid(Distrib.cost.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(Distrib.effectiveness.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(Distrib.CER.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(Distrib.incrementals.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(CE.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(CE.scatterplot.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(Accept.Curve.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(Survival.Curve.but, sticky = "s", padx = 5, pady = 5)
		
		OK.but <- tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <- tkbutton(frameLower,text="Cancel", width=.Width.but, height=.Height.but, command=OnCancel)
			
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		
		tkgrid(frameDistribution,sticky="nwe")
		tkgrid(frameOtherGraphs,sticky="nwe")
		tkgrid(frameResume,sticky="nwe")
		tkgrid(frameLower, sticky = "s")
		tkgrid(frameOverall)
	
		tkbind(graphsimulationWindow, "<Return>", OnOK)
		tkbind(graphsimulationWindow, "<Escape>", OnCancel)
			
		tkfocus(graphsimulationWindow)
# OnGraph ----------------------------------------------------------------------------------------------------------------		tkfocus(summarysimulationWindow)

	}
	
	OnText <- function() {
		StatsData <- Alltreatmentstable[ order(Alltreatmentstable$Treatment, Alltreatmentstable$Data),]
		assign("StatsData", StatsData, .EnvironmentArvoRe)
		
		Costdata <- subset(StatsData, Data == "Cost")
		Effectivenessdata <- subset(StatsData, Data == "Effectiveness")
		CEdata <- subset(StatsData, Data == "C/E")
		
		statsSWindow <- tktoplevel()
		title.window <- "ÁrvoRe - MC Simulação - Estatísticas"
		tkwm.title(statsSWindow, title.window)
		
		frameOverall <- tkwidget(statsSWindow, "labelframe", borderwidth = 2, relief = "groove")
		frameButtons <- tkframe(statsSWindow, relief="groove", borderwidth = 0)
		
		OnNM <- function() {
			WTPVal <- as.integer(tclvalue(WTPvar))
			
			selected.treatment <- treatments.sim[1]
			Mktable <- Simlist[[selected.treatment]]
			# The NMB -----------------------------------------------------------------------
			# Remover esta linha se sumarizar saídas de funções de simulação
			DataCost <- apply(Mktable$Cost,2,sum)
			DataEffectiveness <- apply(Mktable$Effectiveness,2,sum)
			Data <- DataEffectiveness * WTPVal - DataCost
			NMBtable <- data.frame( Cost = DataCost, 
									Effectiveness = DataEffectiveness, 
									NMB = Data)
			namesvariables <- c(".Cost", ".Effectiveness", ".NMB")
			names(NMBtable) <- paste(selected.treatment,namesvariables,sep="")
								
			if (length(treatments.sim) > 1) {					
				for (i in 2:length(treatments.sim) ) {
					selected.treatment <- treatments.sim[i]
					Mktable <- Simlist[[selected.treatment]]
					
					# The NMB -----------------------------------------------------------------------
					# Remover esta linha se sumarizar saídas de funções de simulação
					DataCost <- apply(Mktable$Cost,2,sum)
					DataEffectiveness <- apply(Mktable$Effectiveness,2,sum)
					Data <- DataEffectiveness * WTPVal - DataCost
					
					newNMBtable <- data.frame( Cost = DataCost, 
											Effectiveness = DataEffectiveness, 
											NMB = Data)
					names(newNMBtable) <- paste(selected.treatment,namesvariables,sep="")
					# Guarda as informações importantes
					NMBtable <- abind(NMBtable, newNMBtable, along=2)
					
				}
			}
			Trial <- 1:length(DataCost)
			NMBtable <- abind(Trial, NMBtable, along=2)
			names(NMBtable) <- c("Trial", names(NMBtable))
			
			tituloNMB <- "Estatísticas - Net Monetary Benefits"
			NMBtable <- as.matrix(NMBtable)
			
			displayInTable(NMBtable, title = tituloNMB, height=min(10,dim(NMBtable)[1]), width= min(10,dim(NMBtable)[2]),
							nrow=dim(NMBtable)[1],ncol=dim(NMBtable)[2],
							titlerows = FALSE, titlecols = TRUE, editable = FALSE,
							returntt = FALSE)
		}
		
		OnNH <- function() {
			WTPVal <- as.integer(tclvalue(WTPvar))
			
			selected.treatment <- treatments.sim[1]
			Mktable <- Simlist[[selected.treatment]]
			# The NMB -----------------------------------------------------------------------
			# Remover esta linha se sumarizar saídas de funções de simulação
			DataCost <- apply(Mktable$Cost,2,sum)
			DataEffectiveness <- apply(Mktable$Effectiveness,2,sum)
			Data <- DataEffectiveness * WTPVal - DataCost
			
			NMBtable <- data.frame( Cost = DataCost, 
									Effectiveness = DataEffectiveness, 
									NMB = Data)
			namesvariables <- c(".Cost", ".Effectiveness", ".NMB")
			names(NMBtable) <- paste(selected.treatment,namesvariables,sep="")
								
			if (length(treatments.sim) > 1) {					
				for (i in 2:length(treatments.sim) ) {
					selected.treatment <- treatments.sim[i]
					Mktable <- Simlist[[selected.treatment]]
					
					# The NMB -----------------------------------------------------------------------
					# Remover esta linha se sumarizar saídas de funções de simulação
					DataCost <- apply(Mktable$Cost,2,sum)
					DataEffectiveness <- apply(Mktable$Effectiveness,2,sum)
					Data <- DataEffectiveness * WTPVal - DataCost
					
					newNMBtable <- data.frame( Cost = DataCost, 
											Effectiveness = DataEffectiveness, 
											NMB = Data)
					names(newNMBtable) <- paste(selected.treatment,namesvariables,sep="")
					# Guarda as informações importantes
					NMBtable <- abind(NMBtable, newNMBtable, along=2)
					
				}
			}
			Trial <- 1:length(DataCost)
			NMBtable <- abind(Trial, NMBtable, along=2)
			names(NMBtable) <- c("Trial", names(NMBtable))
			
			tituloNMB <- "Estatísticas - Net Health Benefits"
			NMBtable <- as.matrix(NMBtable)
			
			displayInTable(NMBtable, title = tituloNMB, height=min(10,dim(NMBtable)[1]), width= min(10,dim(NMBtable)[2]),
							nrow=dim(NMBtable)[1],ncol=dim(NMBtable)[2],
							titlerows = FALSE, titlecols = TRUE, editable = FALSE,
							returntt = FALSE)
		}

		OnCE <- function() {
			
			selected.treatment <- treatments.sim[1]
			Mktable <- Simlist[[selected.treatment]]
			# The CE -----------------------------------------------------------------------
			# Remover esta linha se sumarizar saídas de funções de simulação
			DataCost <- apply(Mktable$Cost,2,sum)
			DataEffectiveness <- apply(Mktable$Effectiveness,2,sum)
			
			CEtable <- data.frame( Cost = DataCost, 
									Effectiveness = DataEffectiveness, 
									CE = DataCost / DataEffectiveness)
			namesvariables <- c(".Cost", ".Effectiveness", ".CE")
			names(CEtable) <- paste(selected.treatment,namesvariables,sep="")
								
			if (length(treatments.sim) > 1) {					
				for (i in 2:length(treatments.sim) ) {
					selected.treatment <- treatments.sim[i]
					Mktable <- Simlist[[selected.treatment]]
					
					# The CE -----------------------------------------------------------------------
					# Remover esta linha se sumarizar saídas de funções de simulação
					DataCost <- apply(Mktable$Cost,2,sum)
					DataEffectiveness <- apply(Mktable$Effectiveness,2,sum)
					
					newCEtable <- data.frame( Cost = DataCost, 
											Effectiveness = DataEffectiveness, 
											CE = DataCost / DataEffectiveness)
					names(newCEtable) <- paste(selected.treatment,namesvariables,sep="")
					# Guarda as informações importantes
					CEtable <- abind(CEtable, newCEtable, along=2)
					
				}
			}
			Trial <- 1:length(DataCost)
			CEtable <- abind(Trial, CEtable, along=2)
			names(CEtable) <- c("Trial", names(CEtable))
			
			tituloCE <- "Estatísticas - Análise de Custo-Efetividade"
			CEtable <- as.matrix(CEtable)
			
			displayInTable(CEtable, title = tituloCE, height=min(10,dim(CEtable)[1]), width= min(10,dim(CEtable)[2]),
							nrow=dim(CEtable)[1],ncol=dim(CEtable)[2],
							titlerows = FALSE, titlecols = TRUE, editable = FALSE,
							returntt = FALSE)
		}
				
		.Width.but <- 18
		.Height.but <- 1
		
		NM.but <-tkbutton(frameOverall,text="Net monetary benefit", width=.Width.but, height=.Height.but, command=OnNM)
		NH.but <-tkbutton(frameOverall,text="Net health benefit", width=.Width.but, height=.Height.but, command=OnNH)
		CE.but <-tkbutton(frameOverall,text="Custo-Efetividade", width=.Width.but, height=.Height.but, command=OnCE)
		
		tkgrid(NM.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(NH.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(CE.but, sticky = "s", padx = 5, pady = 5)
		
		WTPvar <- tclVar(0.1)

		WTPValue  <- tkentry(frameOverall,width="20",textvariable=WTPvar)
		tkgrid(tklabel(frameOverall,text="Valor do willingness-to-pay (WTP)"),
				row = 4, column = 0, columnspan = 2, sticky = "n")
		tkgrid(WTPValue, row = 5, column = 0, columnspan = 2, sticky = "n")
		tkgrid(tklabel(frameOverall,text="                                             "),
				row = 6, column = 0, columnspan = 2, sticky = "n")
		
		
		tkgrid( frameOverall, sticky = "n", columnspan = 2, padx = 5, pady = 5)
		tkgrid( frameButtons, sticky = "s")
		
		OnOK <- function() {
			tkdestroy(statsSWindow)
			tkfocus(summarysimulationWindow)
		}
		
		tkbind(statsSWindow, "<Return>",OnOK)
		tkbind(statsSWindow, "<Escape>",OnOK)
	
		OK.but <-tkbutton(frameButtons,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
# 		Cancel.but <-tkbutton(framebutton,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		
		tkgrid(OK.but, sticky = "s", columnspan = 2, padx = 5, pady = 5)
		
	}
	
	OnExport <- function() {
		filetypeWindow <- tktoplevel()
		title <- "ÁrvoRe - Exportar"
		tkwm.title(filetypeWindow,title)

	    frameOverall <- tkframe(filetypeWindow)
	    frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=2)
	    frameLower <- tkframe(frameOverall, borderwidth=2)

		tkgrid(tklabel(frameUpper,text="Selecione o tipo de arquivo:"))
		filetypes <- c("CSV (separado por vírgulas)","TXT (texto separado por tabulações)","Todos arquivos")
		fileextensions <- c(".csv", ".txt", " ")
		
		widthcombo <- max( nchar(filetypes) )
		
		comboBox <- tkwidget(frameUpper,"ComboBox", width = widthcombo, editable = FALSE, values = filetypes)
		tkgrid(comboBox)
	
		OnOK <- function() {
    		filetypeChoice <- filetypes[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
    		fileextChoice <- fileextensions[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
    		tkdestroy(filetypeWindow)
    		filetypes <- paste("{{ ", filetypeChoice, "}", " {", fileextChoice, "}}", sep = "")
    		fileName <- tclvalue(tkgetSaveFile(filetypes=filetypes))
    		
			if (!nchar(fileName))
				tkfocus(summarysimulationWindow)
			else {
				selectedpage.number <- tclvalue(tcl(pBar,"raise"))	# Retorna a página selecionada
				selectedpage.number <- as.numeric(substr(selectedpage.number,5,nchar(selectedpage.number)))
				selected.treatment <- treatments.sim[selectedpage.number]
				Mktable <- Simlist[[selected.treatment]]
				
				if (tipo.nodo[selectedpage.number] == "C") {
					ResumeSim <- data.frame(Cost = apply(Mktable$Cost,2,sum), 
											Effectiveness = apply(Mktable$Effectiveness,2,sum))
					ResumeSim <- data.frame(Trial = 0:(dim(ResumeSim)[1] - 1), ResumeSim)
					
					ans <- substr(fileName,nchar(fileName)-3,nchar(fileName))
					if ( fileextChoice == ".csv" ) {
						if (ans == ".csv") {
							write.csv2(ResumeSim, file = fileName, row.names = FALSE)
						} else {
							fileName <- paste(fileName, ".csv", sep = "")
							write.csv2(ResumeSim, file = fileName, row.names = FALSE)
						}
					}
					if ( fileextChoice == ".txt" ) {
						if (ans == ".txt") {
							write.table(ResumeSim, file = fileName, sep = "\t")
						} else {
							fileName <- paste(fileName, ".txt", sep = "")
							write.table(ResumeSim, file = fileName, sep = "\t")
						}
					}
					if ( fileextChoice == " " ) {
						if (ans == ".txt") {
							write.table(ResumeSim, file = fileName, sep = "\t")
						} else {
							fileName <- paste(fileName, ".txt", sep = "")
							write.table(ResumeSim, file = fileName, sep = "\t")
						}
					}	
				} else {
					if (tipo.nodo[selectedpage.number] == "M") {
						
						# Summary Coort
						ResumeSim <- data.frame(Cost = apply(Mktable$Cost,2,sum), 
												Effectiveness = apply(Mktable$Effectiveness,2,sum))
						ResumeSim <- data.frame(Individual = 1:(dim(ResumeSim)[1]), ResumeSim)
						
						ans <- substr(fileName,nchar(fileName)-3,nchar(fileName))
						if ( fileextChoice == ".csv" ) {
							if (ans == ".csv") {
								write.csv2(ResumeSim, file = fileName, row.names = FALSE)
							} else {
								fileName <- paste(fileName, ".csv", sep = "")
								write.csv2(ResumeSim, file = fileName, row.names = FALSE)
							}
						}
						if ( fileextChoice == ".txt" ) {
							if (ans == ".txt") {
								write.table(ResumeSim, file = fileName, sep = "\t")
							} else {
								fileName <- paste(fileName, ".txt", sep = "")
								write.table(ResumeSim, file = fileName, sep = "\t")
							}
						}
						if ( fileextChoice == " " ) {
							if (ans == ".txt") {
								write.table(ResumeSim, file = fileName, sep = "\t")
							} else {
								fileName <- paste(fileName, ".txt", sep = "")
								write.table(ResumeSim, file = fileName, sep = "\t")
							}
						}	
						
						# Full detail						
						Cycle <- 0:(dim(Mktable$Path)[1] - 1)
						ResumeSim.Cost <- data.frame( Cycle, Mktable$Cost )
						ResumeSim.Effectiveness <- data.frame( Cycle, Mktable$Effectiveness )
						ResumeSim.Path <- data.frame( Cycle, Mktable$Path )
						
# 						print(fileName)
						
						ans <- substr(fileName,nchar(fileName)-3,nchar(fileName))
						if ( substr(fileName,nchar(fileName)-3,nchar(fileName)-3) == "." ) {
							ans.root.file.name <- substr(fileName,1,nchar(fileName)-4)
						} else {
							ans.root.file.name <- fileName
						}
						
						if ( fileextChoice == ".csv" ) {
							if (ans == ".csv") {
# 								print("Estou salvando")
								fileName <- paste(ans.root.file.name," Cost", ans, sep = "")
								write.csv2(ResumeSim.Cost, file = fileName, row.names = FALSE)
								fileName <- paste(ans.root.file.name," Effectiveness", ans, sep = "")
								write.csv2(ResumeSim.Effectiveness, file = fileName, row.names = FALSE)
								fileName <- paste(ans.root.file.name," Path", ans, sep = "")
								write.csv2(ResumeSim.Path, file = fileName, row.names = FALSE)
							} else {
# 								print("Estou salvando")
								fileName <- paste(ans.root.file.name, " Cost", ".csv", sep = "")
								write.csv2(ResumeSim.Cost, file = fileName, row.names = FALSE)
								fileName <- paste(ans.root.file.name, " Effectiveness", ".csv", sep = "")
								write.csv2(ResumeSim.Effectiveness, file = fileName, row.names = FALSE)
								fileName <- paste(ans.root.file.name, " Path", ".csv", sep = "")
								write.csv2(ResumeSim.Path, file = fileName, row.names = FALSE)	
							}
						}
						if ( fileextChoice == ".txt" ) {
							if (ans == ".txt") {
								write.table(ResumeSim, file = fileName, sep = "\t")
							} else {
								fileName <- paste(fileName, ".txt", sep = "")
								write.table(ResumeSim, file = fileName, sep = "\t")
							}
						}
						if ( fileextChoice == " " ) {
							if (ans == ".txt") {
								write.table(ResumeSim, file = fileName, sep = "\t")
							} else {
								fileName <- paste(fileName, ".txt", sep = "")
								write.table(ResumeSim, file = fileName, sep = "\t")
							}
						}						
						
			 		} else {
					cat("Aviso: não é possível exportar resultados para nodo Terminal")	
					}
				}
								
				tkfocus(summarysimulationWindow)
			}	

    	}
    	
    	OnCancel <- function() {
	    	tkdestroy(filetypeWindow)
	    	tkfocus(summarysimulationWindow)
    	}
    	
    	.Width.but <- 10
		.Height.but <- 1
		
		OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <-tkbutton(frameLower,text="Cancel", width=.Width.but, height=.Height.but, command=OnCancel)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		
		tkgrid(frameUpper,sticky="nwe")
		tkgrid(frameLower,sticky="nwe")
		tkgrid(frameOverall)

		tkbind(filetypeWindow, "<Return>",OnOK)
		tkbind(filetypeWindow, "<Escape>",OnOK)
		
		tkfocus(filetypeWindow)
	}
	
	OnStatsRep <- function() {
		
		
		StatsData <- Alltreatmentstable[ order(Alltreatmentstable$Treatment, Alltreatmentstable$Data),]
		assign("StatsData", StatsData, .EnvironmentArvoRe)
		
		Costdata <- subset(StatsData, Data == "Cost")
		Effectivenessdata <- subset(StatsData, Data == "Effectiveness")
		CEdata <- subset(StatsData, Data == "C/E")
		
		
# 		print(StatsData)
		
		statsSWindow <- tktoplevel()
		title.window <- "ÁrvoRe - MC Simulação - Estatísticas"
		tkwm.title(statsSWindow, title.window)
		
		frameOverall <- tkwidget(statsSWindow, "labelframe", borderwidth = 2, relief = "groove")
		frameButtons <- tkframe(statsSWindow, relief="groove", borderwidth = 0)
		
		OnNM <- function() {
			WTPVal <- as.integer(tclvalue(WTPvar))
			
			NMBtable <- data.frame(Treatment = array(,0), Mean = array(,0), 
								Variance = array(,0), Sd = array(,0), Median = array(,0), 
								Min = array(,0), Max = array(,0), 
								Quartil1 = array(,0), Quartil2 = array(,0))
								
			for (i in 1:length(treatments.sim) ) {
				selected.treatment <- treatments.sim[i]
				Mktable <- Simlist[[selected.treatment]]
				
				# The NMB -----------------------------------------------------------------------
				# Remover esta linha se sumarizar saídas de funções de simulação
				Data <- apply(Mktable$Effectiveness,2,sum) * WTPVal - apply(Mktable$Cost,2,sum)
				
				statisticsData <- summary(Data)
				
				meanData <- statisticsData[4]
				varData <- var(Data, na.rm = TRUE, use = "complete.obs")
				sdData <- sqrt(varData)
				medianData <- statisticsData[3]
				minData <- statisticsData[1]
				maxData <- statisticsData[6]
				quartil1 <- statisticsData[2]
				quartil3 <- statisticsData[5]
				
				# Guarda as informações importantes
				line.data.summary <- data.frame(Treatment = selected.treatment, Mean = meanData, 
									Variance = varData, Sd = sdData, Median = medianData, 
									Min = minData, Max = maxData, 
									Quartil1 = quartil1, Quartil2 = quartil3)
				NMBtable <- abind(NMBtable, line.data.summary, along=1)
			}
			
			tituloNMB <- "Estatísticas - Net Monetary Benefits"
			NMBtable <- as.matrix(NMBtable)
			
			displayInTable(NMBtable, title = tituloNMB, height=min(10,dim(NMBtable)[1]), width= min(10,dim(NMBtable)[2]),
							nrow=dim(NMBtable)[1],ncol=dim(NMBtable)[2],
							titlerows = FALSE, titlecols = TRUE, editable = FALSE,
							returntt = FALSE)
		}
		
		OnNH <- function() {
			WTPVal <- as.integer(tclvalue(WTPvar))
			
			NMBtable <- data.frame(Treatment = array(,0), Mean = array(,0), 
								Variance = array(,0), Sd = array(,0), Median = array(,0), 
								Min = array(,0), Max = array(,0), 
								Quartil1 = array(,0), Quartil2 = array(,0))
								
			for (i in 1:length(treatments.sim) ) {
				selected.treatment <- treatments.sim[i]
				Mktable <- Simlist[[selected.treatment]]
				
				# The NHB -----------------------------------------------------------------------
				# Remover esta linha se sumarizar saídas de funções de simulação
				Data <- apply(Mktable$Effectiveness,2,sum) * WTPVal - apply(Mktable$Cost,2,sum)
				
				statisticsData <- summary(Data)
				
				meanData <- statisticsData[4]
				varData <- var(Data, na.rm = TRUE, use = "complete.obs")
				sdData <- sqrt(varData)
				medianData <- statisticsData[3]
				minData <- statisticsData[1]
				maxData <- statisticsData[6]
				quartil1 <- statisticsData[2]
				quartil3 <- statisticsData[5]
				
				# Guarda as informações importantes
				line.data.summary <- data.frame(Treatment = selected.treatment, Mean = meanData, 
									Variance = varData, Sd = sdData, Median = medianData, 
									Min = minData, Max = maxData, 
									Quartil1 = quartil1, Quartil2 = quartil3)
				NMBtable <- abind(NMBtable, line.data.summary, along=1)
			}
			
			tituloNMB <- "Estatísticas - Net Monetary Benefits"
			NMBtable <- as.matrix(NMBtable)
			
			displayInTable(NMBtable, title = tituloNMB, height=min(10,dim(NMBtable)[1]), width= min(10,dim(NMBtable)[2]),
							nrow=dim(NMBtable)[1],ncol=dim(NMBtable)[2],
							titlerows = FALSE, titlecols = TRUE, editable = FALSE,
							returntt = FALSE)
			
		}

		OnCE <- function() {
# 			ResumeData <- as.data.frame( t(StatsData[,2:dim(StatsData)[2]]) )
# 			names(ResumeData) <- StatsData[,1]
# 			ResumeData <- as.matrix(ResumeData)

			tituloCE <- "Estatísticas - Análise de Custo-Efetividade"
			StatsData <- as.matrix(StatsData)
			
			displayInTable(StatsData, title = tituloCE, height=min(10,dim(StatsData)[1]), width= min(10,dim(StatsData)[2]),
							nrow=dim(StatsData)[1],ncol=dim(StatsData)[2],
							titlerows = FALSE, titlecols = TRUE, editable = FALSE,
							returntt = FALSE)
		}
				
		OnICER <- function(Alltreatmentstable) {
			icer.sim.window(Alltreatmentstable)
		}
		
		OnINB <- function(Alltreatmentstable) {
			inb.sim.window(Alltreatmentstable)
		}
		
		.Width.but <- 40
		.Height.but <- 1
		
		NM.but <-tkbutton(frameOverall,text="Net monetary benefit", width=.Width.but, height=.Height.but, command=OnNM)
		NH.but <-tkbutton(frameOverall,text="Net health benefit", width=.Width.but, height=.Height.but, command=OnNH)
		CE.but <-tkbutton(frameOverall,text="Custo-Efetividade", width=.Width.but, height=.Height.but, command=OnCE)
		ICER.but <-tkbutton(frameOverall,text="Razão adicional de C-E (ICER)", width=.Width.but, height=.Height.but, 
					command= function() OnICER(StatsData))
		INB.but <-tkbutton(frameOverall,text="Incremento da rede de benfícios (INB)", width=.Width.but, 
					height=.Height.but, command= function() OnINB(StatsData))
		
		
		tkgrid(NM.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(NH.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(CE.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(ICER.but, sticky = "s", padx = 5, pady = 5)
		tkgrid(INB.but, sticky = "s", padx = 5, pady = 5)
		
		WTPvar <- tclVar(0.1)

		WTPValue  <- tkentry(frameOverall,width="20",textvariable=WTPvar)
		tkgrid(tklabel(frameOverall,text="Valor do willingness-to-pay (WTP)"),
				columnspan = 2, sticky = "n")
		tkgrid(WTPValue, columnspan = 2, sticky = "n")
		tkgrid(tklabel(frameOverall,text="                                             "),
				columnspan = 2, sticky = "n")
		
		
		tkgrid( frameOverall, sticky = "n", columnspan = 2, padx = 5, pady = 5)
		tkgrid( frameButtons, sticky = "s")
		
		OnOK <- function() {
			tkdestroy(statsSWindow)
			tkfocus(summarysimulationWindow)
		}
		
		tkbind(statsSWindow, "<Return>",OnOK)
		tkbind(statsSWindow, "<Escape>",OnOK)
	
		OK.but <-tkbutton(frameButtons,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
# 		Cancel.but <-tkbutton(framebutton,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		
		tkgrid(OK.but, sticky = "s", columnspan = 2, padx = 5, pady = 5)
		
	}
	
	.Width.but <- 18
	.Height.but <- 1
	
	OK.but <-tkbutton(framebutton,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
	StatsRep.but <-tkbutton(framePanelButton,text="Estatísticas", width=.Width.but, height=.Height.but,command=OnStatsRep)
	Graph.but <-tkbutton(framePanelButton,text="Gráficos", width=.Width.but, height=.Height.but,command=OnGraph)
	TextRep.but <-tkbutton(framePanelButton,text="Relatório Texto", width=.Width.but, height=.Height.but,command=OnText)
	Export.but <-tkbutton(framePanelButton,text="Exportar Relatório", width=.Width.but, height=.Height.but,command=OnExport)
	
	tkbind(summarysimulationWindow, "<Return>",OnOK)
	tkbind(summarysimulationWindow, "<Escape>",OnOK)

			
	tkgrid(StatsRep.but, sticky = "s", padx = 5, pady = 5)
	tkgrid(Graph.but, sticky = "s", padx = 5, pady = 5)
	tkgrid(TextRep.but, sticky = "s", padx = 5, pady = 5)
	tkgrid(Export.but, sticky = "s", padx = 5, pady = 5)

	tkgrid(OK.but, sticky = "s", padx = 5, pady = 5)

# 	posiciona.janela.centro(tt, summarysimulationWindow)
					
	tkfocus(summarysimulationWindow)
	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
