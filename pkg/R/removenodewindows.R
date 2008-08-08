`removenodewindows` <-
function(...) {
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		node.number <- as.numeric(nodeSec[3])
		node.col <- as.numeric(nodeSec[2])
		if (node.col > 1) {
			position <- intersect(which((TheTree$Level == node.col)),which(TheTree$Node.N == node.number))
			Removenamevar <- TheTree$Node.name[position]
			
			msg <- paste("Deseja realmente excluir o nodo '", Removenamevar, "'?", sep = "")
			ans <- tkmessageBox(message=msg, icon="question",type="yesnocancel",default="no")
			ans <- as.character(tclvalue(ans))
			if (ans == "yes") {
				NewTheTree <- remove.node(TheTree, node.col, node.number)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				setremovenode(NewTheTree, .EnvironmentArvoRe)
				refreshF5()
				tkfocus(tt)
			}
		} else {
			msg <- paste("Não é possível remover o nodo raiz.")
			tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
			tkfocus(tt)
		}
	}
}

