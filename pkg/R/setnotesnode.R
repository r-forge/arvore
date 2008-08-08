`setnotesnode` <-
function(TheTree, column, node.number, nodo.note, .EnvironmentArvoRe) {
	position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
	if (length(position) >= 1) {
		TheTree$Note[position] <- nodo.note

		assign("TheTree", TheTree, envir = .EnvironmentArvoRe)
		assign(".workstatus", "unsaved", .EnvironmentArvoRe)
	}
}

