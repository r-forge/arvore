`terminal.markov.coort.table` <-
function(TheTree, trials = 2) {
	# cria a tabela de resposta
	Coorte.Ind <- matrix("1",1,trials)		# Matriz com cada individuo
	Coorte.Cost <- matrix(TheTree$Payoff1,1,trials)	# Matriz com custo de cada individuo
	Coorte.Effec <- matrix(TheTree$Payoff2,1,trials)	# Matriz com a efetividade de cada individuo
	ans <- list(Path = Coorte.Ind, Cost = Coorte.Cost, Effectiveness = Coorte.Effec)
	return(ans)	# And return the result
}

