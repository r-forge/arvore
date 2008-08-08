\name{markov.coort.table}
\alias{markov.coort.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
markov.coort.table(TheTree, markov.propertiesMAT, markov.termination, initial.coort = 10000, seed = FALSE, absorventstatedeath = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
  \item{markov.propertiesMAT}{ ~~Describe \code{markov.propertiesMAT} here~~ }
  \item{markov.termination}{ ~~Describe \code{markov.termination} here~~ }
  \item{initial.coort}{ ~~Describe \code{initial.coort} here~~ }
  \item{seed}{ ~~Describe \code{seed} here~~ }
  \item{absorventstatedeath}{ ~~Describe \code{absorventstatedeath} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(TheTree, markov.propertiesMAT, markov.termination, initial.coort = 10000, seed = FALSE,
								absorventstatedeath = 1) {

	# ajusta a semente escolhida pelo usuário
	if (seed != FALSE) {
		set.seed(seed)	
	}
		
	# Convert the tree to matrix format
	MatrixTheTree <- convert2matrix(TheTree)
	x <- MatrixTheTree$x								# Structure matrix
	y <- MatrixTheTree$y								# Node name matrix
	typeMAT <- MatrixTheTree$typeMAT					# Node type matrix
	utilityMAT <- MatrixTheTree$utilityMAT				# Node Cost matrix 
	effectivenessMAT <- MatrixTheTree$effectivenessMAT	# Node effectiveness matrix
	probMAT <- MatrixTheTree$probMAT					# Node probability matrix
	destinyMAT <- MatrixTheTree$destinyMAT				# Terminal node destiny matrix

	num.col.x <- dim(x)[2]
	num.lin.x <- dim(x)[1]

	SummaryTreeTable <- subset(TheTree, Level == 2)
	col.pos <- as.numeric(SummaryTreeTable$Level)
	MARKOV.states <- as.numeric(SummaryTreeTable$Node.N) # MARKOV.states
# 	print(MARKOV.states)
	MARKOV.states.init.prob <- as.numeric(SummaryTreeTable$Prob) # MARKOV.states
# 	print(MARKOV.states.init.prob)
	MARKOV.states.init.cost.rwd <- as.numeric(markov.propertiesMAT$Initial.cost) # MARKOV.states
	MARKOV.states.incr.cost.rwd <- as.numeric(markov.propertiesMAT$Incremental.cost) # MARKOV.states
	MARKOV.states.final.cost.rwd <- as.numeric(markov.propertiesMAT$Final.cost) # MARKOV.states
	MARKOV.states.init.effectiveness.rwd <- as.numeric(markov.propertiesMAT$Initial.effectiveness) # MARKOV.states
	MARKOV.states.incr.effectiveness.rwd <- as.numeric(markov.propertiesMAT$Incremental.effectiveness) # MARKOV.states
	MARKOV.states.final.effectiveness.rwd <- as.numeric(markov.propertiesMAT$Final.effectiveness) # MARKOV.states
	
	MARKOV.states.names <- SummaryTreeTable$Node.name
# 	print(MARKOV.states.names)
	
	# listas para comportar matrizes associadas a cada Markov state
	MARKOV.states.arvores <- list()
	MARKOV.states.rotulos <- list()
	MARKOV.states.destino <- list()
	MARKOV.states.probs <- list()
	MARKOV.states.utilities <- list()
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
		sub.typeMAT <- utilityMAT[selected.lines, col.pos[i]:num.col.x]
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
			sub.typeMAT <- matrix(c("D",sub.typeMAT), 1, n.mat)
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
		MARKOV.states.utilities[[i]] <- sub.utilityMAT / sub.effectivenessMAT
	}

	# ajusta nomes nas listas.
	names(MARKOV.states.arvores) <- c(as.array(as.character(MARKOV.states)))
	names(MARKOV.states.rotulos) <- names(MARKOV.states.arvores)
	names(MARKOV.states.destino) <- names(MARKOV.states.arvores)
	names(MARKOV.states.probs) <- names(MARKOV.states.arvores)
	names(MARKOV.states.costs) <- names(MARKOV.states.arvores)
	names(MARKOV.states.effectiveness) <- names(MARKOV.states.arvores)
	names(MARKOV.states.utilities) <- names(MARKOV.states.arvores)

# 	print(" MOSTRANDO... MARKOV.states.arvores")
# 	print(MARKOV.states.arvores)
# 	assign("MARKOV.states.arvores", MARKOV.states.arvores, globalenv())
# 	print(" MOSTRANDO... MARKOV.states.rotulos")
# 	print(MARKOV.states.rotulos)
# 	assign("MARKOV.states.rotulos", MARKOV.states.rotulos, globalenv())
# 	print(" MOSTRANDO... MARKOV.states.destino")
# 	print(MARKOV.states.destino)
# 	assign("MARKOV.states.destino", MARKOV.states.destino, globalenv())
# 	print(" MOSTRANDO... MARKOV.states.probs")
# 	print(MARKOV.states.probs)
# 	assign("MARKOV.states.probs", MARKOV.states.probs, globalenv())
# 	print(" MOSTRANDO... MARKOV.states.utilities")
# 	print(MARKOV.states.utilities)
# 	assign("MARKOV.states.utilities", MARKOV.states.utilities, globalenv())
# 	print(" MOSTRANDO... MARKOV.states.costs")
# 	print(MARKOV.states.costs)
# 	assign("MARKOV.states.costs", MARKOV.states.costs, globalenv())
# 	print(" MOSTRANDO... MARKOV.states.effectiveness")
# 	print(MARKOV.states.effectiveness)
# 	assign("MARKOV.states.effectiveness", MARKOV.states.effectiveness, globalenv())
	
	# Busca por estados absorventes
	if (absorventstatedeath == 1) {
		nodos.test.absorvent <- names(MARKOV.states.destino)
		absorventstate <- array(,0)
		
		for (i in nodos.test.absorvent) {
			destinyofthisstate <- MARKOV.states.destino[[i]]	
# 			print(destinyofthisstate)
			checkdestiny <- ( destinyofthisstate == i )
# 			print(checkdestiny)
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
	.stage <- 0
	.stage.reward <- 0
	.stage.cost <- 0
	.stage.eff <- 0
	.total.reward <- 0	# ajusta a soma do ciclo zero para zero.
	.total.cost <- 0
	.total.eff <- 0

	while( ! eval( parse(text = markov.termination) ) ) {
		.stage <- .stage + 1
		Coorte.Ind.LINE <- matrix(MARKOV.states[num.markov.states],1,initial.coort)
		Coorte.Cost.LINE <- matrix(0,1,initial.coort)
		Coorte.Effec.LINE <- matrix(0,1,initial.coort)
		
		for (i in 1:num.markov.states ) {
			positions <- which(Coorte.Ind[.stage,] == MARKOV.states[i])
			indvs <- length(positions)
			if ( indvs != 0 ) {
				arvore <- MARKOV.states.arvores[[as.character(MARKOV.states[i])]]
				rotulos <- MARKOV.states.rotulos[[as.character(MARKOV.states[i])]]
				destinos <- MARKOV.states.destino[[as.character(MARKOV.states[i])]]
				probabilidades <- MARKOV.states.probs[[as.character(MARKOV.states[i])]]
				custos <- MARKOV.states.costs[[as.character(MARKOV.states[i])]]
				efetividades <- MARKOV.states.effectiveness[[as.character(MARKOV.states[i])]]
				utilidades <- MARKOV.states.utilities[[as.character(MARKOV.states[i])]]
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
		Coorte.Ind <- rbind(Coorte.Ind, Coorte.Ind.LINE)
		Coorte.Cost <- rbind(Coorte.Cost, Coorte.Cost.LINE)
		Coorte.Effec <- rbind(Coorte.Effec, Coorte.Effec.LINE)
	}
	
	# Definições para a soma de valores no final da simulação (the final reward)
	for (i in num.markov.states:1) {
		positions <- which( Coorte.Ind[.stage + 1,] <= MARKOV.states[i] )
		Coorte.Cost[.stage + 1,positions] <- MARKOV.states.final.cost.rwd[i]
		Coorte.Effec[.stage + 1,positions] <- MARKOV.states.final.effectiveness.rwd[i]
	}
		
# 	print(" MOSTRANDO... Coorte.Ind")
# 	print(Coorte.Ind)
# 	print(" MOSTRANDO... Coorte.Cost")
# 	print(Coorte.Cost)
# 	print(" MOSTRANDO... Coorte.Effec")
# 	print(Coorte.Effec)

	# Aplica NA para individuos dos estados absorventes considerados morte
	if (absorventstatedeath == 1) {
		SurvivalCurve <- replace(Coorte.Ind, which( Coorte.Ind == absorventstate), NA)
# 		Coorte.Ind <- replace(Coorte.Ind, which( Coorte.Ind == absorventstate), NA)
# 		Coorte.Cost2 <- replace(Coorte.Ind, which( SurvivalCurve == NA), NA)
# 		Coorte.Effec2 <- replace(Coorte.Ind, which( SurvivalCurve == NA), NA)
		SurvivalCurve <- apply(!is.na(SurvivalCurve), 1, sum)
		SurvivalCurve <- as.array(SurvivalCurve)
		names(SurvivalCurve) <- paste("Cycle ", 0:(length(SurvivalCurve)-1), sep = "")
# 		print(SurvivalCurve)
	} else {
		SurvivalCurve <- rep( dim(Coorte.Ind)[2], dim(Coorte.Ind)[1])
		names(SurvivalCurve) <- paste("Cycle ", 0:(length(SurvivalCurve)-1), sep = "")
# 		print(SurvivalCurve)
	}
	
# 	print(" MOSTRANDO... Coorte.Ind")
# 	print(Coorte.Ind)
# 	print(" MOSTRANDO... Coorte.Cost")
# 	print(Coorte.Cost)
# 	print(" MOSTRANDO... Coorte.Effec")
# 	print(Coorte.Effec)

	ans <- list(Path = Coorte.Ind, Cost = Coorte.Cost, Effectiveness = Coorte.Effec, Survival = SurvivalCurve)
	return(ans)	# And return the result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
