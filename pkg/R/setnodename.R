`setnodename` <-
function(TheTree, column, node.number, nodename, .EnvironmentArvoRe) {
	if (!is.numeric(node.number)) node.number <- as.numeric(node.number)
	if (!is.numeric(column)) column <- as.numeric(column)
	
	position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
	if (length(position) >= 1) {
# 		old.father.name <- TheTree$Node.name[position]
		TheTree$Node.name[position] <- nodename

		position <- intersect(which((TheTree$Level == (column+1) )),which(TheTree$Father == node.number))

		if (length(position) >= 1) {
			TheTree$Father.Name[position] <- nodename
		}

	assign("TheTree", TheTree, envir = .EnvironmentArvoRe)
	assign(".workstatus", "unsaved", .EnvironmentArvoRe)
	}
}

