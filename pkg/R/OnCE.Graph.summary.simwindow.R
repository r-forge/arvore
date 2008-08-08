# FUNCTION :: OnCE.Graph.summary.simwindow							# Criada em June 25, 2008 07:01:38 AM 
#		Use this function to do something.				
#
#
# Revision : 	Xxxxxxxx - Comentários.sobre.esta.revisão
#				
#
# Parameters
#		Alltreatmentstable : summary of simulation dataframe.

# Esta função faz alguma coisa

OnCE.Graph.summary.simwindow <- function(Alltreatmentstable) {
	CEGraphWindow <- tktoplevel()
	title.window <- "ÁrvoRe - MC Simulação - Gráficos"
	tkwm.title(CEGraphWindow, title.window)
	
	frametext <- "Gráfico"
	frameOverall <- tkwidget(CEGraphWindow, "labelframe", borderwidth = 2, relief = "groove", 
							labelanchor = "n", text = frametext)
	frameButton <- tkwidget(CEGraphWindow, "labelframe", borderwidth = 0, relief = "groove")
	
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
# 				plot(AllTreatEffectiveness$Mean, AllTreatCost$Mean, 
# 						col = treatments.colors.plot, pch = "*", main = Graphtitle,
# 						xlab = xlabel, ylab = ylabel)
# 				smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
# 				             legend = c(treatments.label.plot), #legend parameters
# 					     fill=c(treatments.colors.plot),                        #legend parameters
# 					     bg = "gray")
				plot(c(0,AllTreatEffectiveness$Mean), c(0,AllTreatCost$Mean),
						col = c(0,treatments.colors.plot), pch = "*", main = Graphtitle,
						xlab = xlabel, ylab = ylabel)
				for (i in 1:length(AllTreatEffectiveness$Mean)) {
					lines(c(0,AllTreatEffectiveness$Mean[i]),c(0,AllTreatCost$Mean[i]), 
							col = treatments.colors.plot[i], lty = 2)
				}
				smartlegend( x="right", y= "top", inset=0,                             #smartlegend parameters
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
# 					plot(AllTreatEffectiveness$Mean, AllTreatCost$Mean, 
# 							col = treatments.colors.plot, pch = "*", main = Graphtitle,
# 							xlab = xlabel, ylab = ylabel)
# 							
# 					smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
# 					             legend = c(treatments.label.plot), #legend parameters
# 						     fill=c(treatments.colors.plot),                        #legend parameters
# 						     bg = "gray")								
					plot(c(0,AllTreatEffectiveness$Mean), c(0,AllTreatCost$Mean),
							col = c(0,treatments.colors.plot), pch = "*", main = Graphtitle,
							xlab = xlabel, ylab = ylabel)
					for (i in 1:length(AllTreatEffectiveness$Mean)) {
						lines(c(0,AllTreatEffectiveness$Mean[i]),c(0,AllTreatCost$Mean[i]), 
								col = treatments.colors.plot[i], lty = 2)
					}
					smartlegend( x="right", y= "top", inset=0,                             #smartlegend parameters
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
# 					plot(AllTreatEffectiveness$Mean, AllTreatCost$Mean, 
# 							col = treatments.colors.plot, pch = "*", main = Graphtitle,
# 							xlab = xlabel, ylab = ylabel)
# 							
# 					smartlegend( x="left", y= "top", inset=0,                             #smartlegend parameters
# 					             legend = c(treatments.label.plot), #legend parameters
# 						     fill=c(treatments.colors.plot),                        #legend parameters
# 						     bg = "gray")								
					plot(c(0,AllTreatEffectiveness$Mean), c(0,AllTreatCost$Mean),
							col = c(0,treatments.colors.plot), pch = "*", main = Graphtitle,
							xlab = xlabel, ylab = ylabel)
					for (i in 1:length(AllTreatEffectiveness$Mean)) {
						lines(c(0,AllTreatEffectiveness$Mean[i]),c(0,AllTreatCost$Mean[i]), 
								col = treatments.colors.plot[i], lty = 2)
					}
					smartlegend( x="right", y= "top", inset=0,                             #smartlegend parameters
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
# 			posiciona.janela.no.mouse(exportImgGraphWindow)
	}
	
    .Width.but <- 10
	.Height.but <- 1
	
	OK.but <- tkbutton(frameButton,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
	Export.but <- tkbutton(frameButton,text="Exportar...", width=.Width.but, height=.Height.but, command=OnExportGraphic)

	tkgrid(OK.but, Export.but, sticky = "s", padx = 5, pady = 5)
# 		tkconfigure(Export.but, state = "disabled")

	tkbind(CEGraphWindow, "<Return>", OnOK)
	tkbind(CEGraphWindow, "<Escape>", OnCancel)

	tkwm.deiconify(CEGraphWindow)
	tkfocus(CEGraphWindow)
	
}
