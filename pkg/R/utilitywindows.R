`utilitywindows` <-
function() {
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		utilityWindow <- tktoplevel()
		title <- "ÁrvoRe - Payoffs Nodo"
		tkwm.title(utilityWindow,title)

		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		
		utilityvar <- tclVar(TheTree$Payoff1[position])
		effectivenessvar <- tclVar(TheTree$Payoff2[position])
		
		entry.Value  <- tkentry(utilityWindow,width="20",textvariable=utilityvar)
		tkgrid(tklabel(utilityWindow,text="Valor do custo"))
		tkgrid(entry.Value)
		
		entry.Value.effectiveness  <- tkentry(utilityWindow,width="20",textvariable=effectivenessvar)
		label.entry.Value.effect <- tklabel(utilityWindow,text="Valor da efetividade")
		tkgrid(label.entry.Value.effect)
		tkgrid(entry.Value.effectiveness)
		
		if (.modeltypeArvore == "SD") {
			tkconfigure(entry.Value.effectiveness, state = "disabled")
			tkconfigure(label.entry.Value.effect, state = "disabled")
		}
		
		OnOK <- function()
		{
			utilityVal <- as.numeric(tclvalue(utilityvar))
			effectivenessVal <- as.numeric(tclvalue(effectivenessvar))
			
			if ( (is.numeric(utilityVal)) && (!is.na(utilityVal)) &&
				 (is.numeric(effectivenessVal)) && (!is.na(effectivenessVal)) ) {
				tkdestroy(utilityWindow)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				setutility(TheTree, nodeSec[2], nodeSec[3], utilityVal, .EnvironmentArvoRe)
				seteffectiveness(TheTree, nodeSec[2], nodeSec[3], effectivenessVal, .EnvironmentArvoRe)
				refreshF5()
				tkfocus(tt)
			} else {
				msg <- paste("Este não é um valor de utilidade válido '",utilityVal, "'")
				tkmessageBox(message=msg)
				tkfocus(utilityWindow)
			}
		}
		OK.but <-tkbutton(utilityWindow,text="    OK    ",command=OnOK)
		
		OnCancel <- function()
		{
			tkdestroy(utilityWindow)
			tkfocus(tt)
		}
		
		Cancel.but <-tkbutton(utilityWindow,text=" Cancelar ",command=OnCancel)
		
		tkbind(entry.Value, "<Return>",OnOK)
		tkbind(entry.Value.effectiveness, "<Return>",OnOK)
		tkbind(utilityWindow, "<Escape>",OnCancel)

		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)

		posiciona.janela.no.mouse(utilityWindow, 200, 130)
		tkfocus(utilityWindow)
	}
}

