`probwindows` <-
function() {
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		probWindow <- tktoplevel()
		title <- "ÁrvoRe - Probabilidade Nodo"
		tkwm.title(probWindow,title)
		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		Probvar <- tclVar(TheTree$Prob[position])

		entry.Value  <- tkentry(probWindow,width="20",textvariable=Probvar)
		tkgrid(tklabel(probWindow,text="Probabilidade"))
		tkgrid(entry.Value)
		OnOK <- function()
		{
			ProbVal <- as.numeric(tclvalue(Probvar))
			if ( (is.numeric(ProbVal)) && (!is.na(ProbVal)) && (ProbVal <= 1) && (ProbVal >= 0) ) {
				tkdestroy(probWindow)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				setprob(TheTree, nodeSec[2], nodeSec[3], ProbVal, .EnvironmentArvoRe)
				tkfocus(tt)
			} else {
				msg <- paste("Este não é um valor de probabilidade válido '",ProbVal, "'")
				tkmessageBox(message=msg)
				tkfocus(probWindow)
			}
		}
		OK.but <-tkbutton(probWindow,text="    OK    ",command=OnOK)
		tkbind(entry.Value, "<Return>",OnOK)
		
		OnCancel <- function()
		{
			tkdestroy(probWindow)
			tkfocus(tt)
		}
		
		Cancel.but <-tkbutton(probWindow,text=" Cancelar ",command=OnCancel)
		tkbind(probWindow, "<Escape>",OnCancel)

		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)

		posiciona.janela.no.mouse(probWindow, 200, 100)
		tkfocus(probWindow)
	}
}

