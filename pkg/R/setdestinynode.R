`setdestinynode` <-
function(TheTree, .EnvironmentArvoRe) {
	assign("TheTree", TheTree, envir = .EnvironmentArvoRe)
	assign(".workstatus", "unsaved", .EnvironmentArvoRe)
}

