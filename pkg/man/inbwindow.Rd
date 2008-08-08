\name{inbwindow}
\alias{inbwindow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inbwindow(TheTree)
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
	
	plotINBtableWindow <- tktoplevel()
	title <- "ÁrvoRe - INB"
	tkwm.title(plotINBtableWindow,title)
				
	# What plot?
	frameOverall <- tkwidget(plotINBtableWindow, "labelframe", borderwidth = 0, relief = "groove", 
							labelanchor = "n")
	frametext <- "Gráfico"
	framePlot <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove", 
							labelanchor = "n", text = frametext)
	frametext <- "Propriedades"
	frameProp <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove", 
							labelanchor = "n", text = frametext)
	frameButton <- tkwidget(plotINBtableWindow, "labelframe", borderwidth = 0, relief = "groove")

	# The data to plot	
	Data.CEA <- cost.effectiveness.table(TheTree)		
	AllTreatCost <- Data.CEA$Mean.Cost
	AllTreatEffectiveness <- Data.CEA$Mean.Effectiveness
	AllTreatCE <- Data.CEA$Mean.Cost / Data.CEA$Mean.Effectiveness
	
	# Initial WTP
	WTParray <- seq(0, 10000, round( (10000 - 0 ) / 10) )
	
	# Initial colors to treatments points
	treatments.colors.plot <- 1:length(Data.CEA$Node.name)
	# The treatments names
	treatments.label.plot <- Data.CEA$Node.name
		
	# Default img type
	img.type <- "png"
	img.quality <- 90

	# The frame Properties 
	LIvar <- tclVar(0)
	LSvar <- tclVar(10000)
	NPvar <- tclVar(10)
	
	label0 <- tklabel(frameProp,text = "Intervalo para o WTP (threshold)")
	tkgrid(label0, columnspan = 2, stick = "n")
	
	entry.ValueLI  <- tkentry(frameProp,width="20",textvariable=LIvar)
	label1 <- tklabel(frameProp,text="Limite inferior")
	tkgrid(label1, entry.ValueLI, sticky = "n")
	
	entry.ValueLS  <- tkentry(frameProp,width="20",textvariable=LSvar)
	label2 <- tklabel(frameProp,text="Limite superior")
	tkgrid(label2, entry.ValueLS, sticky = "n")
	
	entry.ValueNP  <- tkentry(frameProp,width="20",textvariable=NPvar)
	label3 <- tklabel(frameProp,text="Intervalos")
	tkgrid(label3, entry.ValueNP, sticky = "n")
	
	# Cria o label
	textlabellista <- "\nSelecione o procedimento padrão para ACE. \n Ele será a base de comparação para os demais.\n"
	rotulolista <- tklabel(frameProp, text = textlabellista)
	tkgrid(rotulolista, columnspan = 2)
	
	# Cria uma barra de rolagem
	scr <- tkscrollbar(frameProp, repeatinterval=5, command=function(...)tkyview(tl,...))
	
	# Cria os elementos da lista
	elementos <- Data.CEA$Node.name
	
	# Determina a altura da listbox
	heightlistbox <- length(elementos)
	larguratexto <- max(nchar(elementos)) + 4
	# Cria uma listbox					
	tl <- tklistbox(frameProp, height = 5, width = larguratexto, selectmode = "single",
					yscrollcommand = function(...)tkset(scr,...), background="white")
	
	# Adiciona os elementos à listbox
	for (i in (1:heightlistbox)) {
	    tkinsert(tl, "end", elementos[i])
	}
	
	# Monta a listbox e a barra de rolagem
	tkgrid(tl, scr, sticky="nse")
	
# 	tkgrid(tklabel(Frame1, text = " "))
	
	# Ajusta a barra de rolagem
	tkgrid.configure(scr, rowspan = 5, sticky="nsw")
		
	# Define o "Elemento 2" como padrão da listbox.
	# Para a listbox o índice começa em zero
	tkselection.set(tl, 0)
		
	
	# ---------------------------------------------------------------------------------------------------
	tkgrid(framePlot, frameProp, sticky = "n")
	tkgrid(frameOverall, sticky = "nwe")
	
	# Image setings.
	g.imgHeight <- 600/2
	g.imgWidth <- 800/2
		
	# Canvas window configurations
	C.Height <- min(c(g.imgHeight, 768))
	C.Width <- min(c(g.imgWidth, 1024))
	Borderwidth <- 2
		
	# scrollbar objects
	fHscroll <- tkscrollbar(framePlot, orient="horiz", command = function(...)tkxview(fCanvas,...) )
	fVscroll <- tkscrollbar(framePlot, command = function(...)tkyview(fCanvas,...) )
	fCanvas <- tkcanvas(framePlot, relief = "sunken", borderwidth = Borderwidth, 
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
	.Filename <- paste(tempdir(),"\\", "grafico.arvoreCE.png", sep="")
		
		
	plot.it.to.image <- function(wtp, cedata, treatments.colors.plot,
										treatments.label.plot,
										.Filename, img.type = "png", img.quality = 90,
										img.width = 400, img.height = 400, ...) {
		
		if (img.type == "png") {
			png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
				Graphtitle <- "Incremental Net Benefit"
				xlabel <- "Willingness-to-pay"
				ylabel <- "INB"
				
				inb <- cedata$Incr.Eff[1] * wtp - cedata$Incr.Cost[1]
				for (i in 2:dim(cedata)[1]) {
					balde.inb <- cedata$Incr.Eff[i] * wtp - cedata$Incr.Cost[i]
					inb <- rbind(inb, balde.inb)
				}
				rownames(inb) <- cedata$Strategy
# 				print(wtp)
# 				print(inb)
				
				xlim1 <- min(wtp)
				xlim2 <- max(wtp)
				ylim1 <- min(inb)
				ylim2 <- max(inb)
				
				plot(wtp, inb[1,], 
						col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
						xlab = xlabel, ylab = ylabel, xlim = c(xlim1,xlim2), ylim = c(ylim1,ylim2))
				lines(wtp, inb[1,], col = treatments.colors.plot[1])
				for (i in 2:dim(cedata)[1]) {
					lines(wtp, inb[i,], col = treatments.colors.plot[i])
					points(wtp, inb[i,], col = treatments.colors.plot[i], pch = "*")
				}
			dev.off()
		} else {
			if (img.type == "jpg") {
				jpeg(filename = .Filename, width = img.width, height = img.height,
				     units = "px", pointsize = 12, quality = img.quality, bg = "white",
				     res = NA, restoreConsole = FALSE)								
						Graphtitle <- "Incremental Net Benefit"
						xlabel <- "Willingness-to-pay"
						ylabel <- "INB"
						
						inb <- cedata$Incr.Eff[1] * wtp - cedata$Incr.Cost[1]
						for (i in 2:dim(cedata)[1]) {
							balde.inb <- cedata$Incr.Eff[i] * wtp - cedata$Incr.Cost[i]
							inb <- rbind(inb, balde.inb)
						}
						rownames(inb) <- cedata$Strategy
# 						print(wtp)
# 						print(inb)
						
						xlim1 <- min(wtp)
						xlim2 <- max(wtp)
						ylim1 <- min(inb)
						ylim2 <- max(inb)
						
						plot(wtp, inb[1,], 
								col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
								xlab = xlabel, ylab = ylabel, xlim = c(xlim1,xlim2), ylim = c(ylim1,ylim2))
						lines(wtp, inb[1,], col = treatments.colors.plot[1])
						for (i in 2:dim(cedata)[1]) {
							lines(wtp, inb[i,], col = treatments.colors.plot[i])
							points(wtp, inb[i,], col = treatments.colors.plot[i], pch = "*")
						}
				dev.off()
			} else {
				bmp(filename = .Filename, width = img.width, height = img.height,
			    	units = "px", pointsize = 12, bg = "white", res = NA,
			    	restoreConsole = FALSE)
						Graphtitle <- "Incremental Net Benefit"
						xlabel <- "Willingness-to-pay"
						ylabel <- "INB"
						
						inb <- cedata$Incr.Eff[1] * wtp - cedata$Incr.Cost[1]
						for (i in 2:dim(cedata)[1]) {
							balde.inb <- cedata$Incr.Eff[i] * wtp - cedata$Incr.Cost[i]
							inb <- rbind(inb, balde.inb)
						}
						rownames(inb) <- cedata$Strategy
# 						print(wtp)
# 						print(inb)
						
						xlim1 <- min(wtp)
						xlim2 <- max(wtp)
						ylim1 <- min(inb)
						ylim2 <- max(inb)
						
						plot(wtp, inb[1,], 
								col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
								xlab = xlabel, ylab = ylabel, xlim = c(xlim1,xlim2), ylim = c(ylim1,ylim2))
						lines(wtp, inb[1,], col = treatments.colors.plot[1])
						for (i in 2:dim(cedata)[1]) {
							lines(wtp, inb[i,], col = treatments.colors.plot[i])
							points(wtp, inb[i,], col = treatments.colors.plot[i], pch = "*")
						}
				dev.off()
			}
		}
	}
		
	# The CEDATA
			respostaListbox <- Data.CEA$Node.N[as.numeric(tkcurselection(tl))+1]
			Data.alternative <- Data.CEA[Data.CEA$Node.N != respostaListbox,]
			Data.standart <- Data.CEA[Data.CEA$Node.N == respostaListbox, ]
			
			ans <- data.frame( 	Strategy = Data.standart$Node.name,
								Cost = Data.standart$Mean.Cost, 
								Incr.Cost = 0, 
								Effectiveness = Data.standart$Mean.Effectiveness, 
								Incr.Eff = 0, 
								CE.ratio = Data.standart$Mean.Cost / Data.standart$Mean.Effectiveness, 
								ICER = NA
								)
			
			for (i in 1:dim(Data.alternative)[1]) {
				ans.line <- data.frame( Strategy = Data.alternative$Node.name[i],
								Cost = Data.alternative$Mean.Cost[i], 
								Incr.Cost = Data.alternative$Mean.Cost[i] - Data.standart$Mean.Cost, 
								Effectiveness = Data.alternative$Mean.Effectiveness[i], 
								Incr.Eff = Data.alternative$Mean.Effectiveness[i] - Data.standart$Mean.Effectiveness, 
								CE.ratio = Data.alternative$Mean.Cost[i] / Data.alternative$Mean.Effectiveness[i], 
								ICER = ((Data.alternative$Mean.Cost[i] - Data.standart$Mean.Cost) /
											(Data.alternative$Mean.Effectiveness[i] - Data.standart$Mean.Effectiveness))
								)
				ans <- abind(ans, ans.line, along = 1)
				
			}
			ans <- as.data.frame(ans)
# 			print(ans)
			
			ans$Incr.Cost <- as.numeric(as.character(ans$Incr.Cost))
			ans$Incr.Eff <- as.numeric(as.character(ans$Incr.Eff))

	# end CEDATA
		
	plot.it.to.image(WTParray, ans, treatments.colors.plot, treatments.label.plot, 
								.Filename = .Filename, img.type = img.type,
								img.width = g.imgWidth, img.height = g.imgHeight)
			
	image1 <- tclVar()
	tcl("image","create","photo",image1,file=.Filename)
	tkcreate(fCanvas, "image", g.imgWidth/2, g.imgHeight/2, image = image1, anchor = "center")
	tkconfigure(fCanvas, scrollregion = c(0,0,g.imgWidth,g.imgHeight))
						
	OnExportGraphic <- function(...) {
		exportImgGraphWindow <- tktoplevel()
		title <- "ÁrvoRe - Exportar Imagem"
		tkwm.title(exportImgGraphWindow,title)
		
		framePlot <- tkframe(exportImgGraphWindow)
		frameUpper <- tkframe(framePlot, relief="groove", borderwidth=0)
		frameUpperLeft <- tkframe(frameUpper, relief="groove", borderwidth=2)
		frameUpperRigth <- tkframe(frameUpper, relief="groove", borderwidth=2)
		frameLower <- tkframe(framePlot, relief="groove", borderwidth=0)
		
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
		
	    ### Image size settings ###
		numericSpinBox <- tkwidget(frameUpperRigth, "SpinBox", editable=TRUE, range = c(100,10000,1), width = 5)
		labeldigits <- tklabel(frameUpperRigth,text="Altura da imagem")
		tkgrid(labeldigits, numericSpinBox, sticky = "nw", padx = 5, pady = 5)
		tcl(numericSpinBox, "setvalue", paste("@", g.imgHeight,sep = ""))
		
		numericSpinBox2 <- tkwidget(frameUpperRigth, "SpinBox", editable=TRUE, range = c(100,10000,1), width = 5)
		labeldigits <- tklabel(frameUpperRigth,text="Largura da imagem")
		tkgrid(labeldigits, numericSpinBox2, sticky = "nw", padx = 5, pady = 5)
		tcl(numericSpinBox2, "setvalue", paste("@", g.imgWidth,sep = ""))
		
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
			img.height <- as.numeric(tclvalue(tcl(numericSpinBox,"getvalue")))
			if ((is.numeric(img.height) )&&(!is.na(img.height))) g.imgHeight <- img.height
			
			img.width <- as.numeric(tclvalue(tcl(numericSpinBox2,"getvalue")))
			if ((is.numeric(img.width) )&&(!is.na(img.width))) g.imgWidth <- img.width
			
			ImgFormatselected <- as.character(tclvalue(rbValue))
			ImgQualityselected <- as.numeric(as.character(tclvalue(QualityValue)))
			if (ImgFormatselected == "png") {
				.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Portable network graphics Image Files} {.png}} {{All files} *}"))
				if (!nchar(.Filename))
					tkfocus(plotINBtableWindow)
				else {
					ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
					if ( ans != ".png" ) .Filename <- paste(.Filename, ".png", sep="")
							
					if (!file.exists(.Filename)) file.remove(.Filename)
					
					plot.it.to.image(wtp, cedata, treatments.colors.plot, treatments.label.plot,
										.Filename = .Filename, img.type = ImgFormatselected,
										img.width = g.imgWidth, img.height = g.imgHeight)
				}
			} else {
				if (ImgFormatselected == "jpg") {
					.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Jpeg Image Files} {.jpg}} {{All files} *}"))
					if (!nchar(.Filename))
						tkfocus(plotINBtableWindow)
					else {
						ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
						if ( ans != ".jpg" ) .Filename <- paste(.Filename, ".jpg", sep="")
										
						if (!file.exists(.Filename)) file.remove(.Filename)
								
						plot.it.to.image(wtp, cedata, treatments.colors.plot, treatments.label.plot,
											.Filename = .Filename, img.type = ImgFormatselected,
											img.quality = ImgQualityselected,
											img.width = g.imgWidth, img.height = g.imgHeight)
					}
				} else {
					.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Bitmap Image Files} {.bmp}} {{All files} *}"))
					if (!nchar(.Filename))
						tkfocus(plotINBtableWindow)
					else {
						ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
						if ( ans != ".bmp" ) .Filename <- paste(.Filename, ".bmp", sep="")
								
						if (!file.exists(.Filename)) file.remove(.Filename)
						
						plot.it.to.image(wtp, cedata, treatments.colors.plot, treatments.label.plot,
											.Filename = .Filename, img.type = ImgFormatselected,
											img.width = g.imgWidth, img.height = g.imgHeight)
					}
				}
			}
			tkdestroy(exportImgGraphWindow)
			tkwm.deiconify(plotINBtableWindow)
			tkfocus(plotINBtableWindow)
		}
				
		OnCancel <- function()
		{
			tkdestroy(exportImgGraphWindow)
			tkwm.deiconify(plotINBtableWindow)
			tkfocus(plotINBtableWindow)
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
		
		tkgrid(framePlot)
		tkfocus(exportImgGraphWindow)
# 		posiciona.janela.no.mouse(exportImgGraphWindow)
	}
	
	Build.INB <- function(wtp, cedata, to.export = FALSE) {
				inb <- cedata$Incr.Eff[1] * wtp - cedata$Incr.Cost[1]
				for (i in 2:dim(cedata)[1]) {
					balde.inb <- cedata$Incr.Eff[i] * wtp - cedata$Incr.Cost[i]
					inb <- rbind(inb, balde.inb)
				}
				
				inb <- t(inb)
				inb <- cbind(wtp, inb)
				if (to.export) {
					inb <- as.data.frame(inb)
# 					print(c("WTP", as.character(cedata$Strategy)))
					names(inb) <- c("WTP", as.character(cedata$Strategy))
				} else {
					colnames(inb) <- c("WTP", cedata$Strategy)
				}
# 				print(inb)
		
	}
	
	OnExportText <- function(Original.Dada) {
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
				tkfocus(filetypeWindow)
			else {
				
				ans <- substr(fileName,nchar(fileName)-3,nchar(fileName))
				if ( fileextChoice == ".csv" ) {
					if (ans == ".csv") {
						write.csv2(Original.Dada, file = fileName, row.names = FALSE)
					} else {
						fileName <- paste(fileName, ".csv", sep = "")
						write.csv2(Original.Dada, file = fileName, row.names = FALSE)
					}
				}
				if ( fileextChoice == ".txt" ) {
					if (ans == ".txt") {
						write.table(Original.Dada, file = fileName, sep = "\t")
					} else {
						fileName <- paste(fileName, ".txt", sep = "")
						write.table(Original.Dada, file = fileName, sep = "\t")
					}
				}
				if ( fileextChoice == " " ) {
					if (ans == ".txt") {
						write.table(Original.Dada, file = fileName, sep = "\t")
					} else {
						fileName <- paste(fileName, ".txt", sep = "")
						write.table(Original.Dada, file = fileName, sep = "\t")
					}
				}	
				tkfocus(plotINBtableWindow)
			}	
		    	}
	   	
	   	OnCancel <- function() {
	    	tkdestroy(filetypeWindow)
	    	tkfocus(plotINBtableWindow)
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

	OnOKINB <- function() {
	
		LIVal <- as.numeric(tclvalue(LIvar))
# 		print(LIVal)
		LSVal <- as.numeric(tclvalue(LSvar))
# 		print(LSVal)
		NPVal <- as.numeric(tclvalue(NPvar))
# 		print(NPVal)
		
		do.it <- TRUE
		if ( !(is.numeric(LIVal)) || (is.na(LIVal)) ) {
			do.it <- FALSE
			msg <- paste("O valor fornecido para o limite inferior não é válido.")
			tkmessageBox(message=msg)
			tkfocus(plotINBtableWindow)
		}
		if ( !(is.numeric(LSVal)) || (is.na(LSVal)) ) {
			do.it <- FALSE
			msg <- paste("O valor fornecido para o limite superior não é válido.")
			tkmessageBox(message=msg)
			tkfocus(plotINBtableWindow)
		}
		if ( !do.it && ( LIVal > LSVal )) {
			do.it <- FALSE
			msg <- paste("O limite inferior deve ser menor que o limite superior.")
			tkmessageBox(message=msg)
			tkfocus(plotINBtableWindow)
		}
		if ( !(is.numeric(NPVal)) || (is.na(NPVal)) || (NPVal < 2) ) {
			do.it <- FALSE
			NPVal <- as.integer(NPVal)
			msg <- paste("O valor fornecido para o número de intervalos não é válido.")
			tkmessageBox(message=msg)
			tkfocus(plotINBtableWindow)
		}
		
		if (do.it) {
			file.remove(.Filename)
			WTParray <- seq(LIVal, LSVal, round( (LSVal - LIVal ) / NPVal))
			
			respostaListbox <- Data.CEA$Node.N[as.numeric(tkcurselection(tl))+1]
			
			Data.alternative <- Data.CEA[Data.CEA$Node.N != respostaListbox,]
			Data.standart <- Data.CEA[Data.CEA$Node.N == respostaListbox, ]
			
			ans <- data.frame( 	Strategy = Data.standart$Node.name,
								Cost = Data.standart$Mean.Cost, 
								Incr.Cost = 0, 
								Effectiveness = Data.standart$Mean.Effectiveness, 
								Incr.Eff = 0, 
								CE.ratio = Data.standart$Mean.Cost / Data.standart$Mean.Effectiveness, 
								ICER = NA
								)
			
			for (i in 1:dim(Data.alternative)[1]) {
				ans.line <- data.frame( Strategy = Data.alternative$Node.name[i],
								Cost = Data.alternative$Mean.Cost[i], 
								Incr.Cost = Data.alternative$Mean.Cost[i] - Data.standart$Mean.Cost, 
								Effectiveness = Data.alternative$Mean.Effectiveness[i], 
								Incr.Eff = Data.alternative$Mean.Effectiveness[i] - Data.standart$Mean.Effectiveness, 
								CE.ratio = Data.alternative$Mean.Cost[i] / Data.alternative$Mean.Effectiveness[i], 
								ICER = ((Data.alternative$Mean.Cost[i] - Data.standart$Mean.Cost) /
											(Data.alternative$Mean.Effectiveness[i] - Data.standart$Mean.Effectiveness))
								)
				ans <- abind(ans, ans.line, along = 1)
				
			}
			ans <- as.data.frame(ans)
# 			print(ans)
			
			ans$Incr.Cost <- as.numeric(as.character(ans$Incr.Cost))
			ans$Incr.Eff <- as.numeric(as.character(ans$Incr.Eff))
			
# 			INB <- ans$Incr.Eff * WTParray - Incr.Cost
			
			plot.it.to.image(WTParray, ans, treatments.colors.plot, treatments.label.plot, 
										.Filename = .Filename, img.type = img.type,
										img.width = g.imgWidth, img.height = g.imgHeight)
					
			image1 <- tclVar()
			tcl("image","create","photo",image1,file=.Filename)
			tkcreate(fCanvas, "image", g.imgWidth/2, g.imgHeight/2, image = image1, anchor = "center")
			tkconfigure(fCanvas, scrollregion = c(0,0,g.imgWidth,g.imgHeight))
		}
		
	}
				
	OnCancel <- function() {
		tkdestroy(plotINBtableWindow)
	#	tkwm.deiconify(tt)
		tkfocus(tt)
	}
	
	tkgrid(frameButton, sticky = "swe")
	    	
	.Width.but <- 10
	.Height.but <- 1
	
	OK.WTP.but <- tkbutton(frameProp,text="OK", width=.Width.but, height=.Height.but, command=OnOKINB)
	tkgrid(OK.WTP.but, sticky = "s", padx = 5, pady = 5, columnspan = 2)
	
	OK.but <- tkbutton(frameButton,text="OK", width=.Width.but, height=.Height.but, command=OnCancel)
	ExportText.but <- tkbutton(frameButton,text="Relatório", width=.Width.but, height=.Height.but, command = function() OnExportText(Build.INB(WTParray, ans, to.export = TRUE)) )
	Export.but <- tkbutton(frameButton,text="Exportar", width=.Width.but, height=.Height.but, command=OnExportGraphic)
	
	tkgrid(OK.but, ExportText.but, Export.but, sticky = "s", padx = 5, pady = 5)
	tkbind(plotINBtableWindow, "<Return>",OnOKINB)
	tkbind(plotINBtableWindow, "<Escape>",OnCancel)
		
# 	posiciona.janela.no.mouse(plotINBtableWindow, 300, 180)
		
	tkfocus(plotINBtableWindow)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
