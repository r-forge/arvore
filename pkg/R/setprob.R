`setprob` <-
function(TheTree, column, node.number, pvalue, .EnvironmentArvoRe) {
	if (!is.numeric(node.number)) node.number <- as.numeric(node.number)
	if (!is.numeric(column)) column <- as.numeric(column)
	
	position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
	if (length(position) >= 1) {
		TheTree$Prob[position] <- pvalue

		assign("TheTree", TheTree, envir = .EnvironmentArvoRe)
		assign(".workstatus", "unsaved", .EnvironmentArvoRe)
	}
	refreshF5()
}

