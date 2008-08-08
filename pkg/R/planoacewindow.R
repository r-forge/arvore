`planoacewindow` <-
function(TheTree) {
	require(abind)
	require(gplots)
		
	plotCEtableWindow <- tktoplevel()
	title <- "ÁrvoRe - Análise de Custo-Efetividade"
	tkwm.title(plotCEtableWindow,title)
				
	# What plot?
	frametext <- "Gráfico"
	frameOverall <- tkwidget(plotCEtableWindow, "labelframe", borderwidth = 2, relief = "groove", 
							labelanchor = "n", text = frametext)
	frameButton <- tkwidget(plotCEtableWindow, "labelframe", borderwidth = 0, relief = "groove")
		
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
	.Filename <- paste(tempdir(),"\\", "grafico.arvoreCE.png", sep="")
		
	# The data to plot	
	Data.CEA <- cost.effectiveness.table(TheTree)		
	AllTreatCost <- Data.CEA$Mean.Cost
# 	print(AllTreatCost)
	AllTreatEffectiveness <- Data.CEA$Mean.Effectiveness
# 	print(AllTreatEffectiveness)
	AllTreatCE <- Data.CEA$Mean.Cost / Data.CEA$Mean.Effectiveness
	
	# Initial colors to treatments points
	treatments.colors.plot <- 1:length(Data.CEA$Node.name)
	# The treatments names
	treatments.label.plot <- Data.CEA$Node.name
		
	# Default img type
	img.type <- "png"
	img.quality <- 90
		
	plot.it.to.image <- function(AllTreatEffectiveness, AllTreatCost, treatments.colors.plot,
										treatments.label.plot,
										.Filename, img.type = "png", img.quality = 90,
										img.width = 600, img.height = 600, ...) {
			
		if (img.type == "png") {
			png(file=.Filename, width = img.width, height = img.height, bg = "white", restoreConsole = FALSE)
					Graphtitle <- "Plano Custo-Efetividade"
					xlabel <- "Efetividade"
					ylabel <- "Custo"
					plot(c(0,AllTreatEffectiveness), c(0,AllTreatCost), 
							col = c(0,treatments.colors.plot), pch = "*", main = Graphtitle,
							xlab = xlabel, ylab = ylabel)
					for (i in 1:length(AllTreatEffectiveness)) {
						lines(c(0,AllTreatEffectiveness[i]), c(0,AllTreatCost[i]), col = treatments.colors.plot[i])
					}
					smartlegend( x="center", y= "top", inset=0,                             #smartlegend parameters
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
					plot(c(0,AllTreatEffectiveness), c(0,AllTreatCost), 
							col = c(0,treatments.colors.plot), pch = "*", main = Graphtitle,
							xlab = xlabel, ylab = ylabel)
					for (i in 1:length(AllTreatEffectiveness)) {
						lines(c(0,AllTreatEffectiveness[i]), c(0,AllTreatCost[i]), col = treatments.colors.plot[i])
					}
					smartlegend( x="center", y= "top", inset=0,                             #smartlegend parameters
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
					plot(c(0,AllTreatEffectiveness), c(0,AllTreatCost), 
							col = c(0,treatments.colors.plot), pch = "*", main = Graphtitle,
							xlab = xlabel, ylab = ylabel)
					for (i in 1:length(AllTreatEffectiveness)) {
						lines(c(0,AllTreatEffectiveness[i]), c(0,AllTreatCost[i]), col = treatments.colors.plot[i])
					}
					smartlegend( x="center", y= "top", inset=0,                             #smartlegend parameters
					             legend = c(treatments.label.plot), #legend parameters
						     fill=c(treatments.colors.plot),                        #legend parameters
						     bg = "gray")
				dev.off()
			}
		}
	}
			
	plot.it.to.image(AllTreatEffectiveness, AllTreatCost, treatments.colors.plot, treatments.label.plot, 
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
					tkfocus(CEGraphWindow)
				else {
					ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
					if ( ans != ".png" ) .Filename <- paste(.Filename, ".png", sep="")
							
					if (!file.exists(.Filename)) file.remove(.Filename)
					
					plot.it.to.image(AllTreatEffectiveness, AllTreatCost, treatments.colors.plot, treatments.label.plot,
										.Filename = .Filename, img.type = ImgFormatselected,
										img.width = g.imgWidth, img.height = g.imgHeight)
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
											.Filename = .Filename, img.type = ImgFormatselected,
											img.quality = ImgQualityselected,
											img.width = g.imgWidth, img.height = g.imgHeight)
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
											.Filename = .Filename, img.type = ImgFormatselected,
											img.width = g.imgWidth, img.height = g.imgHeight)
					}
				}
			}
			tkdestroy(exportImgGraphWindow)
			tkwm.deiconify(plotCEtableWindow)
			tkfocus(plotCEtableWindow)
		}
				
		OnCancel <- function()
		{
			tkdestroy(exportImgGraphWindow)
			tkwm.deiconify(plotCEtableWindow)
			tkfocus(plotCEtableWindow)
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
# 		posiciona.janela.no.mouse(exportImgGraphWindow)
	}
		
	OnOK <- function() {
		file.remove(.Filename)	
		tkdestroy(plotCEtableWindow)
		tkfocus(tt)
	}
			
	OnCancel <- function() {
		tkdestroy(plotCEtableWindow)
		file.remove(.Filename)	
	#	tkwm.deiconify(tt)
		tkfocus(tt)
	}
	    
	.Width.but <- 10
	.Height.but <- 1
			
	OK.but <- tkbutton(frameButton,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
	Cancel.but <- tkbutton(frameButton,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCancel)
	Export.but <- tkbutton(frameButton,text="Exportar", width=.Width.but, height=.Height.but, command=OnExportGraphic)
	
	tkgrid(OK.but, Cancel.but, Export.but, sticky = "s", padx = 5, pady = 5)
	tkbind(plotCEtableWindow, "<Return>",OnOK)
	tkbind(plotCEtableWindow, "<Escape>",OnOK)
		
# 	posiciona.janela.no.mouse(plotCEtableWindow, 300, 180)
		
	tkfocus(plotCEtableWindow)

}

