`settypenode` <-
function(TheTree, column, node.number, nodo.type, .EnvironmentArvoRe) {
	position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
	if (length(position) >= 1) {
		TheTree$Type[position] <- nodo.type

		assign("TheTree", TheTree, envir = .EnvironmentArvoRe)
		assign(".workstatus", "unsaved", .EnvironmentArvoRe)
	}
}

