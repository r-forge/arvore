\name{export.tree.graph}
\alias{export.tree.graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
export.tree.graph(...)
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
	
	exportgraphWindow <- tktoplevel()
	title <- "ÁrvoRe - Exportar Imagem"
	tkwm.title(exportgraphWindow,title)
	
	frameOverall <- tkframe(exportgraphWindow)
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
	
	OnOK <- function()
	{
		ImgFormatselected <- as.character(tclvalue(rbValue))
		ImgQualityselected <- as.numeric(as.character(tclvalue(QualityValue)))
		if (ImgFormatselected == "png") {
			.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Portable network graphics Image Files} {.png}} {{All files} *}"))
			if (!nchar(.Filename))
				tkfocus(tt)
			else {
				ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
				if ( ans != ".png" ) .Filename <- paste(.Filename, ".png", sep="")
						
				if (!file.exists(.Filename)) file.remove(.Filename)
				
				png(file=.Filename, width = imgWidth, height = imgHeight, bg = "white", restoreConsole = FALSE)
					plot.tree(TheTree, line.type = .treeangle, show.probability = .probabilityconf, 
								show.payoffs = .payoffsconf, show.notes = .notesconf, 
								node.name.font.size = .node.name.font.size, payoffs.font.size = .payoffs.font.size, 
								notes.font.size = .notes.font.size)
				dev.off()
		
			}
		} else {
			if (ImgFormatselected == "jpg") {
				.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Jpeg Image Files} {.jpg}} {{All files} *}"))
				if (!nchar(.Filename))
					tkfocus(tt)
				else {
					ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
					if ( ans != ".jpg" ) .Filename <- paste(.Filename, ".jpg", sep="")
							
					if (!file.exists(.Filename)) file.remove(.Filename)
					
					jpeg(filename = .Filename, width = imgWidth, height = imgHeight,
					     units = "px", pointsize = 12, quality = ImgQualityselected, bg = "white",
					     res = NA, restoreConsole = TRUE)
					plot.tree(TheTree, line.type = .treeangle, show.probability = .probabilityconf, 
								show.payoffs = .payoffsconf, show.notes = .notesconf, 
								node.name.font.size = .node.name.font.size, payoffs.font.size = .payoffs.font.size, 
								notes.font.size = .notes.font.size)
					dev.off()
			
				}
			} else {
				.Filename <- tclvalue(tkgetSaveFile(filetypes="{{Bitmap Image Files} {.bmp}} {{All files} *}"))
				if (!nchar(.Filename))
					tkfocus(tt)
				else {
					ans <- substr(.Filename,nchar(.Filename)-3,nchar(.Filename))
					if ( ans != ".bmp" ) .Filename <- paste(.Filename, ".bmp", sep="")
							
					if (!file.exists(.Filename)) file.remove(.Filename)
					
					bmp(filename = .Filename, width = imgWidth, height = imgHeight,
					    units = "px", pointsize = 12, bg = "white", res = NA,
					    restoreConsole = TRUE)
					plot.tree(TheTree, line.type = .treeangle, show.probability = .probabilityconf, 
								show.payoffs = .payoffsconf, show.notes = .notesconf, 
								node.name.font.size = .node.name.font.size, payoffs.font.size = .payoffs.font.size, 
								notes.font.size = .notes.font.size)
					dev.off()
			
				}
			}
		}
		tkdestroy(exportgraphWindow)
		tkfocus(tt)
	}
	
	OnCancel <- function()
	{
		tkdestroy(exportgraphWindow)
		tkfocus(tt)
	}
	
	.Width.but <- 10
	.Height.but <- 1
	
	OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
	tkbind(exportgraphWindow, "<Return>",OnOK)
	Cancel.but <-tkbutton(frameLower,text="Cancel", width=.Width.but, height=.Height.but, command=OnCancel)
	tkbind(exportgraphWindow, "<Escape>",OnCancel)
	tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5, columnspan = 2, sticky = "s")
	
	tkbind(rb1, "<Enter>",Onformat)
	tkbind(rb2, "<Enter>",Onformat)
	tkbind(rb3, "<Enter>",Onformat)
	tkbind(rb1, "<Leave>",Onformat)
	tkbind(rb2, "<Leave>",Onformat)
	tkbind(rb3, "<Leave>",Onformat)
	
	tkgrid(frameOverall)
	tkfocus(exportgraphWindow)
	posiciona.janela.no.mouse(exportgraphWindow)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
