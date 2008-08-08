`set.model.type` <-
function(typemodel) {
	safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
	if (( typemodel == "CE")||( typemodel == "SD")) {
		assign(".modeltypeArvore", typemodel, .EnvironmentArvoRe)
	} else {
		cat("Error!! \n")	
	}
	refreshF5()
	assign(".workstatus", "unsaved", .EnvironmentArvoRe)
}

