`typenodewindows` <-
function() {
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		# A janela Tk
		typenodeWindow <- tktoplevel(height = 200, width = 150)
		title <- "ÁrvoRe - Tipo Nodo"
		tkwm.title(typenodeWindow,title)
		tkgrid(tklabel(typenodeWindow,text="Selecione o tipo do nodo"), column = 0, row = 0, sticky = "n")

		Frame1 <- tkframe(typenodeWindow, height = 200, width = 150,
							borderwidth = 2, relief = "groove")
		Frame2 <- tkframe(typenodeWindow, height = 200, width = 150,
							borderwidth = 0, relief = "groove")

		tkgrid(Frame1, sticky = "n")
		tkgrid(Frame2, sticky = "s")

		# Type Chance
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/C.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				rb1 <- tkradiobutton(Frame1)
				rbValue <- tclVar("C")
				tkconfigure(rb1,variable=rbValue,value="C")
				tkgrid(	tklabel(Frame1,image=icn), 
						tklabel(Frame1,text="Chance "),rb1, sticky = "ne")

			} else {
				rb1 <- tkradiobutton(Frame1)
				rbValue <- tclVar("C")
				tkconfigure(rb1,variable=rbValue,value="C")
				tkgrid(	tklabel(Frame1,text="Chance "),rb1, sticky = "ne")
			}
		}
		
		# Type Decision
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/D.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				rb2 <- tkradiobutton(Frame1)
				tkconfigure(rb2, variable=rbValue, value="D")
				tkgrid(	tklabel(Frame1,image=icn), 
						tklabel(Frame1,text="Decision "),rb2, sticky = "ne")

			} else {
				rb2 <- tkradiobutton(Frame1)
				tkconfigure(rb2,variable=rbValue,value="D")
				tkgrid( rb2, column = 0, row = 2, sticky = "nw")
				tkgrid(	tklabel(Frame1,text="Decision "),rb2, sticky = "ne")
			}
		}

		# Type Logic
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/L.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				rb3 <- tkradiobutton(Frame1)
				tkconfigure(rb3,variable=rbValue,value="L")
				rb3text <- tklabel(Frame1,text="Logic ")
				tkgrid(	tklabel(Frame1,image=icn), rb3text,
						rb3, sticky = "ne")
			} else {
				rb3 <- tkradiobutton(Frame1)
				tkconfigure(rb3,variable=rbValue,value="L")
				rb3text <- tklabel(Frame1,text="Logic ")
				tkgrid(	rb3text ,rb3, sticky = "ne")
			}
		}

		tkconfigure(rb3, state = "disabled")
		tkconfigure(rb3text, state = "disabled")
		
		# Type Markov
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/M.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				rb4 <- tkradiobutton(Frame1)
				tkconfigure(rb4,variable=rbValue,value="M")
				tkgrid(	tklabel(Frame1,image=icn), 
						tklabel(Frame1,text="Markov "),rb4, sticky = "ne")
			} else {
				rb4 <- tkradiobutton(Frame1)
				tkconfigure(rb4,variable=rbValue,value="M")
				tkgrid(	tklabel(Frame1,text="Markov ") ,rb4, sticky = "ne")
			}
		}

		# Type Terminal
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/T.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				rb5 <- tkradiobutton(Frame1)
				tkconfigure(rb5,variable=rbValue,value="T")
				tkgrid(	tklabel(Frame1,image=icn), 
						tklabel(Frame1,text="Terminal "),rb5, sticky = "ne")
			} else {
				rb5 <- tkradiobutton(Frame1)
				tkconfigure(rb5,variable=rbValue,value="T")
				tkgrid(	tklabel(Frame1,text="Terminal ") ,rb5, sticky = "ne")
			}
		}

		# Type Label
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/X.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				rb6 <- tkradiobutton(Frame1)
				tkconfigure(rb6,variable=rbValue,value="X")
				rb6text <- tklabel(Frame1,text="Label ")
				tkgrid(	tklabel(Frame1,image=icn), rb6text, rb6, sticky = "ne")
			} else {
				rb6 <- tkradiobutton(Frame1)
				tkconfigure(rb6,variable=rbValue,value="X")
				rb6text <- tklabel(Frame1,text="Label ")
				tkgrid(	rb6text ,rb6, sticky = "ne")
			}
		}

		tkconfigure(rb6, state = "disabled")
		tkconfigure(rb6text, state = "disabled")

		tkfocus(typenodeWindow)

		OnOK <- function()
		{
 			nodo.type <- as.character(tclvalue(rbValue))
 			safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
 			settypenode(TheTree, column = column, node.number = node.number, nodo.type = nodo.type, .EnvironmentArvoRe)
			refreshF5()
			tkdestroy(typenodeWindow)
			tkfocus(tt)
		}
		
		OnCancel <- function()
		{
			tkdestroy(typenodeWindow)
			tkfocus(tt)
		}
		
		OK.but <-tkbutton(Frame2,text="    OK    ",command=OnOK)
		tkbind(typenodeWindow, "<Return>",OnOK)
		Cancel.but <-tkbutton(Frame2,text=" Cancelar ",command=OnCancel)
		tkbind(typenodeWindow, "<Escape>",OnCancel)

		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		
		posiciona.janela.no.mouse(typenodeWindow, 150, 200)
 		}
}

