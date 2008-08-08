`atualiza.grafico` <-
function(...) {
	.Filename <- paste(tempdir(),"\\", "arvore.png", sep="")
	if (!file.exists(.Filename)) file.remove(.Filename)
	
	png(file=.Filename, width = imgWidth, height = imgHeight, bg = "white", restoreConsole = FALSE)
		plot.tree(TheTree, line.type = .treeangle, show.probability = .probabilityconf, 
					show.payoffs = .payoffsconf, show.notes = .notesconf, 
					node.name.font.size = .node.name.font.size, payoffs.font.size = .payoffs.font.size, 
					notes.font.size = .notes.font.size)
	dev.off()

	image1 <- tclVar()
	tcl("image","create","photo",image1,file=.Filename)
	tkcreate(Canvas, "image", imgWidth/2, imgHeight/2, image = image1, anchor = "center")
	tkconfigure(Canvas, scrollregion = c(0,0,imgWidth,imgHeight))
	
	file.remove(.Filename)
	tkwm.deiconify(tt)
}

