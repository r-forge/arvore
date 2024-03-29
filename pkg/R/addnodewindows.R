`addnodewindows` <-
function() {
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da �rvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "�rvoRe - AVISO")
		tkfocus(tt)
	} else {
		node.number <- as.numeric(nodeSec[3])
		node.col <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == node.col)),which(TheTree$Node.N == node.number))
		node.type <- TheTree$Type[position]
		if (node.type == "T") {
			msg <- paste(" O nodo selecionado � de tipo 'Terminal'.\n Altere o tipo do nodo e tente novamente.")
			tkmessageBox(message = msg, icon="warning", title = "�rvoRe - AVISO")
			tkfocus(tt)
		} else {
			# A janela Tk
			addnodeWindow <- tktoplevel()
			title <- "�rvoRe - Novo Nodo"
			tkwm.title(addnodeWindow,title)
			
			NomeVar <- tclVar("Novo Nodo")
			NomeEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=NomeVar)
			tkgrid(tklabel(addnodeWindow,text="Nome do nodo"))
			tkgrid(NomeEntryWidget)
			
			ProbabilidadeVar <- tclVar("0.0")
			ProbabilityEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=ProbabilidadeVar)
			tkgrid(tklabel(addnodeWindow,text="Probabilidade"))
			tkgrid(ProbabilityEntryWidget)
			
			UtilidadeVar <- tclVar("0.0")
			UtilityEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=UtilidadeVar)
			tkgrid(tklabel(addnodeWindow,text="Custo / Payoff"))
			tkgrid(UtilityEntryWidget)
	
			EffectivenessVar <- tclVar("0.0")
			EffectivenessEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=EffectivenessVar)
			tkgrid(tklabel(addnodeWindow,text="Efetividade / Payoff"))
			tkgrid(EffectivenessEntryWidget)
		
			NotasVar <- tclVar(" ")
			NotesEntryWidget <- tkentry(addnodeWindow,width="20",textvariable=NotasVar)
			tkgrid(tklabel(addnodeWindow,text="Notas"))
			tkgrid(NotesEntryWidget)		
			
			tkfocus(addnodeWindow)
	
			OnOK <- function()
			{
				NameVal <- tclvalue(NomeVar)
				ProbabilidadeVal <- as.numeric( tclvalue(ProbabilidadeVar) )
				UtilidadeVal <- as.numeric( tclvalue(UtilidadeVar) )
				EffectivenessVal <- as.numeric( tclvalue(EffectivenessVar) )
				NotasVal <- tclvalue(NotasVar)
				
				if ( (ProbabilidadeVal < 0) || (ProbabilidadeVal > 1) ) {
					msg <- paste("Este n�o � um valor de probabilidade v�lido '",ProbVal, "'")
					tkmessageBox(message=msg)
					tkfocus(addnodeWindow)
				} else {
					NewTree <- add.node(TheTree,
									node.col = node.col,
									node.number = node.number, 
									node.name = NameVal, 
									node.prob = ProbabilidadeVal, 
									node.type = "C", 
									node.notes = NotasVal,
									node.destiny = " ",
									node.utility = UtilidadeVal,
									node.effectiveness = EffectivenessVal)
					safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
					setaddnode(NewTree, .EnvironmentArvoRe)
					refreshF5()
					tkdestroy(addnodeWindow)
					tkfocus(tt)
				}
					
			}
			OK.but <-tkbutton(addnodeWindow,text="   OK   ",command=OnOK)
			tkbind(NomeEntryWidget, "<Return>",OnOK)
			tkbind(ProbabilidadeVar, "<Return>",OnOK)
			tkbind(UtilityEntryWidget, "<Return>",OnOK)
			tkbind(EffectivenessEntryWidget, "<Return>",OnOK)
			tkbind(NotasVar, "<Return>",OnOK)
	
			OnCancel <- function()
			{
				tkdestroy(addnodeWindow)
				tkfocus(tt)
			}
			
			Cancel.but <-tkbutton(addnodeWindow,text=" Cancelar ",command=OnCancel)
			tkbind(addnodeWindow, "<Escape>",OnCancel)
	
			tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
	
			posiciona.janela.no.mouse(addnodeWindow, 250, 230)
	 		}
 		}
}

