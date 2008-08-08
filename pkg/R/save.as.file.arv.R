`save.as.file.arv` <-
function(...) {
	fileName<-tclvalue(tkgetSaveFile(filetypes="{{ArvoRe Files} {.arv}} {{All files} *}"))
	if (!nchar(fileName))
		tkfocus(tt)
	else {
		ans <- substr(fileName,nchar(fileName)-3,nchar(fileName))
		if ( ans != ".arv" ) fileName <- paste(fileName, ".arv", sep="")
		save(TheTree, .EnvironmentArvoRe, .modeltypeArvore, markov.propertiesMAT, file = fileName, ascii = TRUE)
		assign(".workstatus", "saved", .EnvironmentArvoRe)
		assign(".opennedfile", fileName, .EnvironmentArvoRe)
		.Windowtitle <- paste("ÁrvoRe - Janela Principal", " - [", .opennedfile, "]", sep = "")
		tkwm.title(tt, .Windowtitle)
		tkfocus(tt)
	}	
}

