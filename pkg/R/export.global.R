`export.global` <-
function(x, nome) {
	assign(nome, x, env = .GlobalEnv)
}

