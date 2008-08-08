`new.file.bot` <-
function(...) {
	ans <- tkmessageBox(message="Deseja salvar a árvore atual?",icon="question",type="yesnocancel",default="yes")
	ans <- tclvalue(ans)
	if (ans != "yes") {
		if (ans == "no") {
			clearTreeTkArvore(TheTree)
			new.tree()
			theTreeTkArvore(TheTree)
			atualiza.grafico()
		} else {
			tkfocus(tt)
		}
	} else {
		save.file.arv()
		clearTreeTkArvore(TheTree)
		new.tree()
		theTreeTkArvore(TheTree)
		atualiza.grafico()		
	}
}

