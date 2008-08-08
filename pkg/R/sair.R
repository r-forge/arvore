`sair` <-
function() {
	ReturnVal <- tkmessageBox(title = "Sair do Programa",
	message = "Deseja realmente sair do programa?",
    icon = "question", type = "yesnocancel", default = "no")
    if (tclvalue(ReturnVal) == "yes") {
	    if (.workstatus == "saved") {
			tkdestroy(tt)
		} else {
			ReturnVal <- tkmessageBox(title = "Sair do Programa", message="Deseja salvar a árvore atual?",
						icon="question", type="yesnocancel", default="yes")
			if (tclvalue(ReturnVal) == "yes") {
    			save.file.arv()
    			tkdestroy(tt)
			} else {
				tkdestroy(tt)
			}
		}
		# clear all arvoRe objects
		.final.objects <- objects(envir = .EnvironmentArvoRe, all.names = TRUE)
		.init.objects <- get(".init.objects", .EnvironmentArvoRe)
		toremove.objects <- setdiff(.final.objects, .init.objects)
		rm(list = toremove.objects, envir = .EnvironmentArvoRe)
	}
	else tkfocus(tt)
}

