`nodenamewindows` <-
function() {
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		nodenameWindow <- tktoplevel()
		title <- "ÁrvoRe - Nome Nodo"
		tkwm.title(nodenameWindow,title)

		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		Namevar <- tclVar(TheTree$Node.name[position])
		
		entry.Value  <- tkentry(nodenameWindow,width="20",textvariable=Namevar)
		tkgrid(tklabel(nodenameWindow,text="Nome do Nodo"), sticky = "n")
		tkgrid(entry.Value, sticky = "n")
		OnOK <- function()
		{
			NameVal <- as.character(tclvalue(Namevar))
			if ( (is.character(NameVal)) && (!is.na(NameVal)) && (nchar(NameVal) > 0) ) {
				tkdestroy(nodenameWindow)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
 				setnodename(TheTree, nodeSec[2], nodeSec[3], NameVal, .EnvironmentArvoRe)
 				refreshF5()
				tkfocus(tt)
			} else {
				msg <- paste("Este não é um nome de nodo válido '",NameVal, "'")
				tkmessageBox(message=msg)
				tkfocus(nodenameWindow)
			}
		}
		OK.but <-tkbutton(nodenameWindow,text="    OK    ",command=OnOK)
		tkbind(entry.Value, "<Return>",OnOK)
		
		OnCancel <- function()
		{
			tkdestroy(nodenameWindow)
			tkfocus(tt)
		}
		
		Cancel.but <-tkbutton(nodenameWindow, text=" Cancelar ", command=OnCancel)
		tkbind(nodenameWindow, "<Escape>",OnCancel)

		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)

		tkfocus(nodenameWindow)
		posiciona.janela.no.mouse(nodenameWindow, 200, 100)
	}
}

