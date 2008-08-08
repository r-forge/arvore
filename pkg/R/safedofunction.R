`safedofunction` <-
function(TheTree, .EnvironmentArvoRe, .modeltypeArvore) {
	assign("TheTree", TheTree, .EnvironmentArvore.Secure)
	assign(".modeltypeArvore", .modeltypeArvore, .EnvironmentArvore.Secure)
}

