# FUNCTION :: onGraph.summary.simwindow							# Criada em June 25, 2008 06:33:00 AM 
#		Use this function to do something.				
#
#
# Revision : 	Xxxxxxxxxxx	- Comentários.sobre.esta.revisão
#				
#
# Parameters
#		Xxxxxxx : xxxx.

# Esta função faz alguma coisa
onGraph.summary.simwindow <- function(Mktable, Alltreatmentstable, selected.treatment) {
		# The data
		Cost <- apply(Mktable$Cost,2,sum, na.rm = TRUE)
		Effectiveness <- apply(Mktable$Effectiveness,2,sum, na.rm = TRUE)
	
		# The main window
		graphsimulationWindow <- tktoplevel()
		title.window <- "ÁrvoRe - MC Simulação - Gráficos"
		tkwm.title(graphsimulationWindow, title.window)
		
		# Frames
		frameOverall <- tkwidget(graphsimulationWindow, "labelframe", borderwidth = 0, relief = "groove")
		frameResume <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove", text = "Tipos de Gráficos")
		frameDistribution <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove", 
										text = "Distribuição")
		frameOtherGraphs <- tkwidget(frameOverall, "labelframe", borderwidth = 2, relief = "groove", 
										text = "Custo-Efetividade")
		frameLower <- tkframe(frameOverall, relief="groove", borderwidth = 0)
		
		
		OnShowIt <- function(type = "Other", SurvivalData = Mktable$Survival,...) {
			
			aGraphWindow <- tktoplevel()
			title.window <- "ÁrvoRe - MC Simulação - Gráficos"
			tkwm.title(aGraphWindow, title.window)
			
			frametext <- "Gráfico"
			frameOverall <- tkwidget(aGraphWindow, "labelframe", borderwidth = 2, relief = "groove", 
									labelanchor = "n", text = frametext)
			frameButton <- tkwidget(aGraphWindow, "labelframe", borderwidth = 0, relief = "groove")
			
			tkgrid(frameOverall, sticky = "nwe")
			tkgrid(frameButton, sticky = "swe")
			
			# Image setings.
			g.imgHeight <- 480
			g.imgWidth <- 640
			
			# Canvas window configurations
			C.Height <- g.imgHeight
			C.Width <- g.imgWidth
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
                        #~ Effectiveness <- Effectiveness[!is.na(Effectiveness)]
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
		
		OnCE <- function(Alltreatmentstable) {
			OnCE.Graph.summary.simwindow(Alltreatmentstable)
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
		Distrib.incrementals.but <- tkbutton(frameDistribution, state = "disabled", text = label.but4, 
			width=.Width.but, height=.Height.but, command = OnDistrib.incrementals)
		CE.but <- tkbutton(frameOtherGraphs, text = label.but5, 
			width=.Width.but, height=.Height.but, command = function() OnCE.Graph.summary.simwindow(Alltreatmentstable))
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
		Cancel.but <- tkbutton(frameLower,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCancel)
			
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		
		tkgrid(frameDistribution,sticky="nwe")
		tkgrid(frameOtherGraphs,sticky="nwe")
		tkgrid(frameResume,sticky="nwe")
		tkgrid(frameLower, sticky = "s")
		tkgrid(frameOverall)
	
		tkbind(graphsimulationWindow, "<Return>", OnOK)
		tkbind(graphsimulationWindow, "<Escape>", OnCancel)
			
		tkfocus(graphsimulationWindow)

}