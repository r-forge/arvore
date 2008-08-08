`zoom.in.but` <-
function(imgHeight) {
	if (imgHeight < 8000) {
		imgHeight <- round(imgHeight * 1.1, digits = 0)
		imgWidth <- round((4/3) * imgHeight, digits = 0)
	} else {
		msg <- paste("Este é um tamanho de imagem consideravelmente grande. Deseja realmente ampliar?")
		ans <- tkmessageBox(message = msg,  icon = "question", type = "yesnocancel", default = "no")
		ans <- as.character(tclvalue(ans))
			if ( ans == "yes" ) {
				imgHeight <- round(imgHeight * 1.1, digits = 0)
				imgWidth <- round((4/3) * imgHeight, digits = 0)
			}
		tkfocus(tt)	
	}
	set.zoom.image.tree(imgHeight, imgWidth)
	refreshF5()
}

