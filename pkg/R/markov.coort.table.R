# FUNCTION :: markov.coort.table					# Mar 05, 2008 03:32:43 PM 
#		Use this function to do some thing.				# Revision : June 28, 2008 09:54:12 PM 
#
# Parameters
#		TheTree : structure tree dataframe.
#		markov.propertiesMAT : base nodes properties of Markov nodes.
#		markov.termination : stop simulation when FALSE.
#		initial.coort : initial coort size.
#		seed : seed used in RNG.
#		absorventstatedeath : 
#
# Example
#		markov.coort.table( TheTree, "(.stage == 10)||(.total.reward >= 110000)" )
#
markov.coort.table <- function(TheTree, markov.propertiesMAT, markov.termination, initial.coort = 10000, seed = FALSE,
								absorventstatedeath = 1) {

	# ajusta a semente escolhida pelo usuário
	if (seed != FALSE) {
		set.seed(seed)	
	}
		
	# Convert the tree to matrix format
	MatrixTheTree <- convert2matrix(TheTree)
	x <- MatrixTheTree$x								# Structure matrix
	y <- MatrixTheTree$y								# Node name matrix
	#~ typeMAT <- MatrixTheTree$typeMAT					# Node type matrix
	utilityMAT <- MatrixTheTree$utilityMAT				# Node Cost matrix 
	effectivenessMAT <- MatrixTheTree$effectivenessMAT	# Node effectiveness matrix
	probMAT <- MatrixTheTree$probMAT					# Node probability matrix
	destinyMAT <- MatrixTheTree$destinyMAT				# Terminal node destiny matrix
	
	num.col.x <- dim(x)[2]
	num.lin.x <- dim(x)[1]

	SummaryTreeTable <- subset(TheTree, Level == 2)
	col.pos <- as.numeric(SummaryTreeTable$Level)
	MARKOV.states <- as.numeric(SummaryTreeTable$Node.N) # MARKOV.states
	MARKOV.states.init.prob <- as.numeric(SummaryTreeTable$Prob) # MARKOV.states
	MARKOV.states.init.cost.rwd <- as.numeric(markov.propertiesMAT$Initial.cost) # MARKOV.states
	MARKOV.states.incr.cost.rwd <- as.numeric(markov.propertiesMAT$Incremental.cost) # MARKOV.states
	MARKOV.states.final.cost.rwd <- as.numeric(markov.propertiesMAT$Final.cost) # MARKOV.states
	MARKOV.states.init.effectiveness.rwd <- as.numeric(markov.propertiesMAT$Initial.effectiveness) # MARKOV.states
	MARKOV.states.incr.effectiveness.rwd <- as.numeric(markov.propertiesMAT$Incremental.effectiveness) # MARKOV.states
	MARKOV.states.final.effectiveness.rwd <- as.numeric(markov.propertiesMAT$Final.effectiveness) # MARKOV.states
	
	MARKOV.states.names <- SummaryTreeTable$Node.name
	
    # Aplica desconto nas payoffs de quem não volta para a árvore associada.
    MARKOV.discount.costs <- SummaryTreeTable$Payoff1
    MARKOV.discount.effectiveness <- SummaryTreeTable$Payoff2
	
	# listas para comportar matrizes associadas a cada Markov state
	MARKOV.states.arvores <- list()
	MARKOV.states.rotulos <- list()
	MARKOV.states.destino <- list()
	MARKOV.states.probs <- list()
	MARKOV.states.costs <- list()
	MARKOV.states.effectiveness <- list()
	
	# fragmenta a matriz da árvore em sub-árvores associadas a cada Markov state
	for (i in 1:length(MARKOV.states.names)) {
		MARKOV.state <- MARKOV.states[i]
		selected.lines <- which(x[,col.pos[i]] == MARKOV.state)
		
		sub.x <- x[selected.lines, col.pos[i]:num.col.x]
		sub.y <- y[selected.lines, col.pos[i]:num.col.x]
		sub.probMAT <- probMAT[selected.lines, col.pos[i]:num.col.x]
		sub.utilityMAT <- utilityMAT[selected.lines, col.pos[i]:num.col.x]
		sub.effectivenessMAT <- effectivenessMAT[selected.lines, col.pos[i]:num.col.x]
		#~ sub.typeMAT <- utilityMAT[selected.lines, col.pos[i]:num.col.x]
		sub.destiny <- destinyMAT[selected.lines]
		
		# se a fragmentação resulta em matriz linha, então é preciso definir que isso é
		# uma matriz... senão vira vetor e não funciona.
		if(length(selected.lines) == 1)  {
			sub.x <- sub.x[!is.na(sub.x)]
			n.mat <- length(sub.x) + 1
			sub.x <- matrix(c(1, sub.x) , 1, n.mat)
			sub.y <- matrix(sub.y[1], 1, n.mat)
			sub.probMAT <- matrix(1.0, 1, n.mat)
			sub.utilityMAT <- matrix(c(0,sub.utilityMAT), 1, n.mat)
			sub.effectivenessMAT <- matrix(c(0,sub.effectivenessMAT), 1, n.mat)
			#~ sub.typeMAT <- matrix(c("D",sub.typeMAT), 1, n.mat)
		} else {
			sub.probMAT[,1] <- 1.0	# Agora o nodo raiz recebe prob = 1.
		}
		
		# ajusta custo e efetividade: serão acumulados através dos nodos.
		sub.utilityMAT <- apply(sub.utilityMAT, 1, sum)
		sub.effectivenessMAT <- apply(sub.effectivenessMAT, 1, sum)

		# abaixo se manda cada matriz de sub-árvore para suas listas.
		MARKOV.states.arvores[[i]] <- sub.x
		MARKOV.states.rotulos[[i]] <- sub.y
		MARKOV.states.destino[[i]] <- sub.destiny
		MARKOV.states.probs[[i]] <- sub.probMAT
		MARKOV.states.costs[[i]] <- sub.utilityMAT
		MARKOV.states.effectiveness[[i]] <- sub.effectivenessMAT
	}

	# ajusta nomes nas listas.
	names(MARKOV.states.arvores) <- c(as.array(as.character(MARKOV.states)))
	names(MARKOV.states.rotulos) <- names(MARKOV.states.arvores)
	names(MARKOV.states.destino) <- names(MARKOV.states.arvores)
	names(MARKOV.states.probs) <- names(MARKOV.states.arvores)
	names(MARKOV.states.costs) <- names(MARKOV.states.arvores)
	names(MARKOV.states.effectiveness) <- names(MARKOV.states.arvores)

	# ajuste para quem não retorna à árvore associada
    for (i in 1:length(MARKOV.states.names)) {
		MARKOV.states.costs[[as.character(MARKOV.states[i])]] <- MARKOV.states.costs[[as.character(MARKOV.states[i])]] -
        	MARKOV.discount.costs[as.numeric(i)] + 
        	MARKOV.discount.costs[as.numeric(MARKOV.states.destino[[as.character(MARKOV.states[i])]])]
		MARKOV.states.effectiveness[[as.character(MARKOV.states[i])]] <- MARKOV.states.effectiveness[[as.character(MARKOV.states[i])]] -
        	MARKOV.discount.effectiveness[as.numeric(i)] + 
        	MARKOV.discount.effectiveness[as.numeric(MARKOV.states.destino[[as.character(MARKOV.states[i])]])]
    }

	# Busca por estados absorventes
	if (absorventstatedeath == 1) {
		nodos.test.absorvent <- names(MARKOV.states.destino)
		absorventstate <- array(,0)
		
		for (i in nodos.test.absorvent) {
			destinyofthisstate <- MARKOV.states.destino[[i]]	
			checkdestiny <- ( destinyofthisstate == i )
			if ( sum(checkdestiny) == length(destinyofthisstate) ) {
# 				cat("Ele é absorvente '", i, "' chamado '", MARKOV.states.rotulos[[i]][1,1],"'\n")
				absorventstate <- c(absorventstate, i)
			}
		}
	}
	
	# cria a tabela que comportará os individuos
	num.markov.states <- length(MARKOV.states)
	Coorte.Ind <- matrix(MARKOV.states[num.markov.states],1,initial.coort)	# Matriz com cada individuo
	Coorte.Cost <- matrix(0,1,initial.coort)	# Matriz com custo de cada individuo
	Coorte.Effec <- matrix(0,1,initial.coort)	# Matriz com a efetividade de cada individuo
	
	# sorteia a distribuição inicial
	init.distr.Prob <- cumsum(MARKOV.states.init.prob)
	sorteados <- runif(initial.coort,0,1)
	if (num.markov.states > 1) {
		for (i in (num.markov.states-1):1) {
			positions <- which( sorteados <= init.distr.Prob[i] )
			Coorte.Ind[1,positions] <- MARKOV.states[i]
			Coorte.Cost[1,positions] <- MARKOV.states.init.cost.rwd[i]
			Coorte.Effec[1,positions] <- MARKOV.states.init.effectiveness.rwd[i]
		}
	}
	
	# control variables
	.stop.sim <- TRUE
	.stage <- 1
	.stage.cost <- sum(Coorte.Cost)
	.stage.eff <- sum(Coorte.Effec)
	.stage.reward <- .stage.cost
	.total.cost <- .stage.cost
	.total.eff <- .stage.eff
	.total.reward <- .stage.cost	# ajusta a soma do ciclo zero para zero.

	while( ! eval( parse(text = markov.termination) ) ) {
		.stage <- .stage + 1
		Coorte.Ind.LINE <- matrix(MARKOV.states[num.markov.states],1,initial.coort)
		Coorte.Cost.LINE <- matrix(0,1,initial.coort)
		Coorte.Effec.LINE <- matrix(0,1,initial.coort)
		
		for (i in 1:num.markov.states ) {
			positions <- which(Coorte.Ind[.stage - 1,] == MARKOV.states[i])
			indvs <- length(positions)
			if ( indvs != 0 ) {
				arvore <- MARKOV.states.arvores[[as.character(MARKOV.states[i])]]
				rotulos <- MARKOV.states.rotulos[[as.character(MARKOV.states[i])]]
				destinos <- MARKOV.states.destino[[as.character(MARKOV.states[i])]]
				probabilidades <- MARKOV.states.probs[[as.character(MARKOV.states[i])]]
				custos <- MARKOV.states.costs[[as.character(MARKOV.states[i])]]
				efetividades <- MARKOV.states.effectiveness[[as.character(MARKOV.states[i])]]
				sorteado <- runif(indvs,0,1)
				linprobs <- cumsum(apply(probabilidades, 1, prod)) # observa a probabilidade de cada ramo acontecer numa runif
				valn <- length(linprobs)
				linprobs.Matrix <- matrix(linprobs, indvs, valn, byrow = TRUE) # podemos ter problema de memória aqui!!!
				resultado <- valn - apply(sorteado <= linprobs.Matrix, 1, sum) + 1
				ans.dest <- destinos[resultado]	# quantos vão para cada categoria
				ans.cost <- custos[resultado]
				ans.effectiveness <- efetividades[resultado]
			}
			Coorte.Ind.LINE[1,positions] <- ans.dest
			Coorte.Cost.LINE[1,positions] <- ans.cost
			Coorte.Effec.LINE[1,positions] <- ans.effectiveness
		}
		.stage.cost <- sum(Coorte.Cost.LINE)
		.stage.eff <- sum(Coorte.Effec.LINE)
		.stage.reward <- .stage.cost
		.total.cost <- .total.cost + .stage.cost
		.total.eff <- .total.eff + .stage.eff
		.total.reward <- .total.cost	# ajusta a soma do ciclo zero para zero.
		
		Coorte.Ind <- rbind(Coorte.Ind, Coorte.Ind.LINE)
		Coorte.Cost <- rbind(Coorte.Cost, Coorte.Cost.LINE)
		Coorte.Effec <- rbind(Coorte.Effec, Coorte.Effec.LINE)
	}
	
	# Definições para a soma de valores no final da simulação (the final reward)
	for (i in num.markov.states:1) {
		positions <- which( Coorte.Ind[.stage,] <= MARKOV.states[i] )
		Coorte.Cost[.stage,positions] <- MARKOV.states.final.cost.rwd[i] + Coorte.Cost[.stage,positions] 
		Coorte.Effec[.stage,positions] <- MARKOV.states.final.effectiveness.rwd[i] + Coorte.Effec[.stage,positions]
	}
		
	# Aplica NA para individuos dos estados absorventes considerados morte
	if (absorventstatedeath == 1) {
		SurvivalCurve <- replace(Coorte.Ind, which( Coorte.Ind == absorventstate), NA)
# 		Coorte.Ind <- replace(Coorte.Ind, which( Coorte.Ind == absorventstate), NA)
# 		Coorte.Cost <- replace(Coorte.Cost, which( SurvivalCurve == NA), NA)
		Coorte.Effec <- replace(Coorte.Effec, which( is.na(SurvivalCurve)), NA)
		SurvivalCurve <- apply(!is.na(SurvivalCurve), 1, sum)
		SurvivalCurve <- as.array(SurvivalCurve)
		names(SurvivalCurve) <- paste("Cycle ", 0:(length(SurvivalCurve)-1), sep = "")
	} else {
		SurvivalCurve <- rep( dim(Coorte.Ind)[2], dim(Coorte.Ind)[1])
		names(SurvivalCurve) <- paste("Cycle ", 0:(length(SurvivalCurve)-1), sep = "")
	}
	
	ans <- list(Path = Coorte.Ind, Cost = Coorte.Cost, Effectiveness = Coorte.Effec, Survival = SurvivalCurve)
	return(ans)	# And return the result
}
