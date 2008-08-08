`load.file.arv` <-
function(...) {
	if (.workstatus != "saved") {
		ans <- tkmessageBox(message="Deseja salvar a árvore atual?",icon="question",type="yesnocancel",default="yes")
		ans <- tclvalue(ans)
		if (ans == "yes") {
			save.file.arv()
		}
	}
	fileName <- tclvalue(tkgetOpenFile(filetypes="{{ArvoRe Files} {.arv}} {{All files} *}"))
	if (!nchar(fileName))
		tkfocus(tt)
	else {
		clearTreeTkArvore(TheTree)
		load(fileName, envir = .EnvironmentArvoRe)
		load(fileName)
		theTreeTkArvore(TheTree)
		atualiza.grafico()
	}	
	assign(".workstatus", "saved", .EnvironmentArvoRe)
	assign(".opennedfile", fileName, .EnvironmentArvoRe)
	titletext <- paste("ÁrvoRe - Janela Principal - [", fileName, "]", sep = "")
	tkwm.title(tt, titletext)

}

