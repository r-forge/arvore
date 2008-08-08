`changedofunction` <-
function(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure) {
	TheTree.old <- TheTree
	.EnvironmentArvoRe.old <- .EnvironmentArvoRe
	.modeltypeArvore.old <- .modeltypeArvore
	
	assign("TheTree", get("TheTree", .EnvironmentArvore.Secure), .EnvironmentArvoRe)
	assign(".EnvironmentArvoRe", get(".EnvironmentArvoRe", .EnvironmentArvore.Secure), .EnvironmentArvoRe)
	assign(".modeltypeArvore", get(".modeltypeArvore", .EnvironmentArvore.Secure), .EnvironmentArvoRe)
	
	safedofunction(TheTree.old, .EnvironmentArvoRe.old, .modeltypeArvore.old)
	refreshF5()
}

