`notesnodewindows` <-
function(...) {
	nodeSec <- nodoselecionado()
		if ( nodeSec[1] == " ") {
			msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
			tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
			tkfocus(tt)
		} else {
			node.number <- as.numeric(nodeSec[3])
			column <- as.numeric(nodeSec[2])
			
			notesWindow <- tktoplevel()
			title <- "ÁrvoRe - Comentários do Nodo"
			tkwm.title(notesWindow,title)
			
			position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
			
			Notesvar <- tclVar(TheTree$Note[position])
			entry.Value  <- tkentry(notesWindow, width="20", textvariable=Notesvar)
			tkgrid(tklabel(notesWindow,text="Nota"))
			tkgrid(entry.Value)
			
			OnOK <- function()
			{
				NotesVal <- as.character(tclvalue(Notesvar))
				tkdestroy(notesWindow)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				setnotesnode(TheTree, column = column, node.number = node.number, nodo.note = NotesVal, .EnvironmentArvoRe)
				refreshF5()
				tkfocus(tt)
			}
			OK.but <-tkbutton(notesWindow, text="    OK    ", command=OnOK)
			tkbind(entry.Value, "<Return>", OnOK)
			
			OnCancel <- function()
			{
				tkdestroy(notesWindow)
				tkfocus(tt)
			}

			Cancel.but <-tkbutton(notesWindow, text=" Cancelar ", command=OnCancel)
			tkbind(notesWindow, "<Escape>", OnCancel)

			tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)

			posiciona.janela.no.mouse(notesWindow, 200, 100)
			tkfocus(notesWindow)
		}	
}

