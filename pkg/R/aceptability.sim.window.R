`aceptability.sim.window` <-
function(Alltreatmentstable) {
	require(abind)
	
		ACsimtableWindow <- tktoplevel()
		title <- "ÁrvoRe - Curva de Aceitabilidade (INB)"
		tkwm.title(ACsimtableWindow,title)
		
		# Cria o primeiro frame
		FrameOverAll <- tkframe(ACsimtableWindow, borderwidth = 0, relief = "groove")
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
		
		# The WTP ---------------------------------------------------------------------
		WTPL1var <- tclVar(0.1)
		WTPL2var <- tclVar(10000)
		WTPpointsvar <- tclVar(10)
		PoinsOriginal <- 10
		
		WTPL1Value  <- tkentry(Frame1,width="20",textvariable=WTPL1var)
		tkgrid(tklabel(Frame1,text="Valor mínimo do willingness-to-pay (WTP)"),
				columnspan = 2, sticky = "n")
		tkgrid(WTPL1Value, columnspan = 2, sticky = "n")
		tkgrid(tklabel(Frame1,text="                                             "),
				columnspan = 2, sticky = "n")
				
		WTPL2Value  <- tkentry(Frame1,width="20",textvariable=WTPL2var)
		tkgrid(tklabel(Frame1,text="Valor máximo do willingness-to-pay (WTP)"),
				columnspan = 2, sticky = "n")
		tkgrid(WTPL2Value, columnspan = 2, sticky = "n")
		tkgrid(tklabel(Frame1,text="                                             "),
				columnspan = 2, sticky = "n")
		
	    ### Numeric format settings ###
		numericSpinBox <- tkwidget(Frame1, "SpinBox", editable=FALSE, range = c(0,100,1), width = 3)
		labeldigits <- tklabel(Frame1,text="Número de intervalos:")
		tkgrid(labeldigits, numericSpinBox, sticky = "nw", padx = 5, pady = 5)
		tcl(numericSpinBox, "setvalue", paste("@", PoinsOriginal,sep = ""))
		
						
		# Monta os frames
		tkgrid(Frame1, sticky = "nwe", padx = 5, pady = 5)
		tkgrid(Frame2, sticky = "s", padx = 5, pady = 5)
		tkgrid(FrameOverAll, sticky = "nswe", columnspan = 2)
		
		OnOK <- function() {
			respostaListbox <- n.treat[as.numeric(tkcurselection(tl))+1]
			WTPL1Val <- as.numeric(tclvalue(WTPL1var))
			WTPL2Val <- as.numeric(tclvalue(WTPL2var))
			WTPPoints <- as.integer(tclvalue(tcl(numericSpinBox,"getvalue")))
			
			WTP <- seq(WTPL1Val, WTPL2Val, (WTPL2Val-WTPL1Val)/WTPPoints )
			
			WTPVal <- 0.1
			
			if ( WTPL1Val < WTPL2Val ) {
				Data.alternative.Cost <- subset(Data.CEA.Cost, NT != respostaListbox)			
				Data.standart.Cost <- subset(Data.CEA.Cost, NT == respostaListbox)
				Data.alternative.Effectiveness <- subset(Data.CEA.Effectiveness, NT != respostaListbox)			
				Data.standart.Effectiveness <- subset(Data.CEA.Effectiveness, NT == respostaListbox)
				Data.alternative.CE <- subset(Data.CEA.CE, NT != respostaListbox)			
				Data.standart.CE <- subset(Data.CEA.CE, NT == respostaListbox)
				
				ans <- data.frame( Standart = rep(0,length(WTP)))
				names.ans <- c("Padrão")
				
				for (i in 1:dim(Data.alternative.Cost)[1]) {
					
					inb <- (Data.alternative.Effectiveness$Mean[i] - Data.standart.Effectiveness$Mean[1]) *
							WTP - (Data.alternative.Cost$Mean[i] - Data.standart.Cost$Mean[1])
					var.inb <- ( 	WTP^2
								) * Data.alternative.Effectiveness$Variance[i] +
								Data.alternative.Cost$Variance[i] -
								2 * WTP * ( Data.alternative.Cost$CovDcDe[i] )
					inb.stat.test <- inb/var.inb^0.5
					Strategy <- Data.alternative.Cost$Treatment[i]
					p.val.inb <- pnorm(inb.stat.test)
					
					ans.line <- data.frame( p.val.inb )
					names.ans <- c(names.ans, Strategy)	
					ans <- abind(ans, ans.line, along = 2)
					
				}
				ans <- as.data.frame(ans)
				names(ans) <- names.ans
# 				print(ans)
				
				OnAC <- function(WTP, ACProbabilities) {
					ACGraphWindow <- tktoplevel()
					title.window <- "ÁrvoRe - MC Simulação - Gráficos"
					tkwm.title(ACGraphWindow, title.window)
					
					frametext <- "Gráfico"
					frameOverall <- tkwidget(ACGraphWindow, "labelframe", borderwidth = 2, relief = "groove", 
											labelanchor = "n", text = frametext)
					frameButton <- tkwidget(ACGraphWindow, "labelframe", borderwidth = 0, relief = "groove")
					
					tkgrid(frameOverall, sticky = "nwe")
					tkgrid(frameButton, sticky = "swe")
					
					# Image setings.
					g.imgHeight <- 480
					g.imgWidth <- 640
					
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
					
					# Initial colors to treatments points
					treatments.colors.plot <- 1:length(names(ACProbabilities))
					# The treatments names
					treatments.label.plot <- names(ACProbabilities)
					
					# What plot?
					plot.it.to.image <- function(ACProbabilities, WTP, treatments.colors.plot,
													treatments.label.plot,
													.Filename, img.type = "png", img.quality = 90, 
													img.width = 600, img.height = 600, ...) {
						
						if (img.type == "png") {
							png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
								Graphtitle <- "Curva de Aceitabilidade"
								xlabel <- "willingness-to-pay"
								ylabel <- "Pr(INB > 0)"
								ylim1 <- -0.1
								ylim2 <- 1.1
								plot(WTP, ACProbabilities[,1], 
										col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
										xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
								lines(WTP, ACProbabilities[,1], 
										col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
										xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
								if (dim(ACProbabilities)[2] > 1) {
									for (i in 2:dim(ACProbabilities)[2]) {
										lines(WTP, ACProbabilities[,i], 
										col = treatments.colors.plot[i], pch = "*", main = Graphtitle,
										xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
										points(WTP, ACProbabilities[,i], 
										col = treatments.colors.plot[i], pch = "*", main = Graphtitle,
										xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
									}
								}
								smartlegend( x="right", y= "top", inset=0,
								             legend = c(treatments.label.plot),
									     fill=c(treatments.colors.plot),
									     bg = "gray")
							dev.off()
						} else {
							if (img.type == "jpg") {
								jpeg(filename = .Filename, width = img.width, height = img.height,
								     units = "px", pointsize = 12, quality = img.quality, bg = "white",
								     res = NA, restoreConsole = FALSE)								
										Graphtitle <- "Curva de Aceitabilidade"
										xlabel <- "willingness-to-pay"
										ylabel <- "Pr(INB > 0)"
										ylim1 <- -0.1
										ylim2 <- 1.1
										plot(WTP, ACProbabilities[,1], 
												col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
												xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
										lines(WTP, ACProbabilities[,1], 
												col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
												xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
										if (dim(ACProbabilities)[2] > 1) {
											for (i in 2:dim(ACProbabilities)[2]) {
												lines(WTP, ACProbabilities[,i], 
												col = treatments.colors.plot[i], pch = "*", main = Graphtitle,
												xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
												points(WTP, ACProbabilities[,i], 
												col = treatments.colors.plot[i], pch = "*", main = Graphtitle,
												xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
											}
										}
										smartlegend( x="right", y= "top", inset=0,
										             legend = c(treatments.label.plot),
											     fill=c(treatments.colors.plot),
											     bg = "gray")
										
								dev.off()
							} else {
								bmp(filename = .Filename, width = img.width, height = img.height,
							    	units = "px", pointsize = 12, bg = "white", res = NA,
							    	restoreConsole = FALSE)
										Graphtitle <- "Curva de Aceitabilidade"
										xlabel <- "willingness-to-pay"
										ylabel <- "Pr(INB > 0)"
										ylim1 <- -0.1
										ylim2 <- 1.1
										plot(WTP, ACProbabilities[,1], 
												col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
												xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
										lines(WTP, ACProbabilities[,1], 
												col = treatments.colors.plot[1], pch = "*", main = Graphtitle,
												xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
										if (dim(ACProbabilities)[2] > 1) {
											for (i in 2:dim(ACProbabilities)[2]) {
												lines(WTP, ACProbabilities[,i], 
												col = treatments.colors.plot[i], pch = "*", main = Graphtitle,
												xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
												points(WTP, ACProbabilities[,i], 
												col = treatments.colors.plot[i], pch = "*", main = Graphtitle,
												xlab = xlabel, ylab = ylabel, ylim = c(ylim1,ylim2))
											}
										}
										smartlegend( x="right", y= "top", inset=0,
										             legend = c(treatments.label.plot),
											     fill=c(treatments.colors.plot),
											     bg = "gray")
										
								dev.off()
									}
						}
					}
					
					# Default img type
					img.type <- "png"
					plot.it.to.image(ACProbabilities, WTP, treatments.colors.plot, treatments.label.plot, 
										.Filename = .Filename, type = type, img.type = img.type,
										img.width = g.imgWidth, img.height = g.imgHeight)
					
					image1 <- tclVar()
					tcl("image","create","photo",image1,file=.Filename)
					tkcreate(fCanvas, "image", g.imgWidth/2, g.imgHeight/2, image = image1, anchor = "center")
					tkconfigure(fCanvas, scrollregion = c(0,0,g.imgWidth,g.imgHeight))
					
					
					OnOK <- function() {
						file.remove(.Filename)
						tkdestroy(ACGraphWindow)
						tkwm.deiconify(ACsimtableWindow)
						tkfocus(ACsimtableWindow)
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
						sliderlabel2 <- tklabel(frameUpperRigth,text = "%")
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
									tkfocus(ACGraphWindow)
								else {
									ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
									if ( ans != ".png" ) .Filename <- paste(.Filename, ".png", sep="")
											
									if (!file.exists(.Filename)) file.remove(.Filename)
									
									plot.it.to.image(ACProbabilities, WTP, treatments.colors.plot, treatments.label.plot,
														.Filename = .Filename, type = type, img.type = ImgFormatselected)
								}
							} else {
								if (ImgFormatselected == "jpg") {
									.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Jpeg Image Files} {.jpg}} {{All files} *}"))
									if (!nchar(.Filename))
										tkfocus(ACGraphWindow)
									else {
										ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
										if ( ans != ".jpg" ) .Filename <- paste(.Filename, ".jpg", sep="")
												
										if (!file.exists(.Filename)) file.remove(.Filename)
										
										plot.it.to.image(ACProbabilities, WTP, treatments.colors.plot, treatments.label.plot,
															.Filename = .Filename, type = type, img.type = ImgFormatselected,
															img.quality = ImgQualityselected)
									}
								} else {
									.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Bitmap Image Files} {.bmp}} {{All files} *}"))
									if (!nchar(.Filename))
										tkfocus(ACGraphWindow)
									else {
										ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
										if ( ans != ".bmp" ) .Filename <- paste(.Filename, ".bmp", sep="")
												
										if (!file.exists(.Filename)) file.remove(.Filename)
										
										plot.it.to.image(ACProbabilities, WTP, treatments.colors.plot, treatments.label.plot,
															.Filename = .Filename, type = type, img.type = ImgFormatselected)
									}
								}
							}
							tkdestroy(exportImgGraphWindow)
							tkwm.deiconify(ACGraphWindow)
							tkfocus(ACGraphWindow)
						}
						
						OnCancel <- function()
						{
							tkdestroy(exportImgGraphWindow)
							tkwm.deiconify(ACGraphWindow)
							tkfocus(ACGraphWindow)
						}
						
						.Width.but <- 10
						.Height.but <- 1
						
						OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
						tkbind(exportImgGraphWindow, "<Return>",OnOK)
						Cancel.but <-tkbutton(frameLower,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCancel)
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
			
					tkbind(ACGraphWindow, "<Return>", OnOK)
					tkbind(ACGraphWindow, "<Escape>", OnCancel)
			
					tkwm.deiconify(ACGraphWindow)
					tkfocus(ACGraphWindow)
					
				}
							
				OnAC(WTP, ans)
				
			}
	    }
			
	    OnCancel <- function() {
			tkdestroy(ACsimtableWindow)
	# 		tkwm.deiconify(tt)
			tkfocus(tt)
	    }
	    
	  	.Width.but <- 10
		.Height.but <- 1
			
		OK.but <-tkbutton(Frame2,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <-tkbutton(Frame2,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCancel)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		tkbind(ACsimtableWindow, "<Return>",OnOK)
		tkbind(ACsimtableWindow, "<Escape>",OnOK)
		
		posiciona.janela.no.mouse(ACsimtableWindow, 310, 310)
		
		tkfocus(ACsimtableWindow)

}

