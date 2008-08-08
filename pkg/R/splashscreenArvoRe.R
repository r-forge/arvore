`splashscreenArvoRe` <-
function() {
	splashArvoRe <- tktoplevel()
	Width <- 640
	Height <- 480
	tkwm.title(splashArvoRe, paste("ÁrvoRe - ", .arvore.version, sep=""))
	for (i in 1:length(.libPaths())) {
		icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Arvore.png",sep=""))
		if (file.exists(icon.but)) {
			icn <- tkimage.create("photo", file=icon.but)
			new.but <- tkbutton(splashArvoRe, image=icn, width=Width, height=Height, 
							command=function() tkdestroy(splashArvoRe))
	 		tkgrid(new.but)
		} 
	}
	posiciona.janela.tela(splashArvoRe)
	tkfocus(splashArvoRe)
	tcl("tkwait","window",splashArvoRe)
}

