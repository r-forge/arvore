`refreshF5` <-
function(...) {
			clearTreeTkArvore(TheTree)
			theTreeTkArvore(TheTree)
			atualiza.grafico()
			tcl(treeWidget,"opentree", "1.1")	# Expande a �rvore
			settreevartype(TheTree) # para ajustar os tipos de vari�veis no TheTree.
}

