`convert2matrix` <-
function(TheTree) {

	n.levels <- max(TheTree$Level)
	
	x <- matrix(NA, 0, n.levels)
	y <- matrix(NA, 0, n.levels)
	probMAT <- matrix(NA, 0, n.levels)
	typeMAT <- matrix(NA, 0, n.levels)
	effectivenessMAT <- matrix(NA, 0, n.levels)
	utilityMAT <- matrix(NA, 0, n.levels)
	destinyMAT <- matrix(NA, 0, n.levels)
	
	for (i in n.levels:1) {
		Data.level <- TheTree[TheTree$Level == i,]
		nodes <- as.numeric(levels(as.factor(Data.level$Node.N)))
		
		line.x <- array(NA, n.levels)
		line.y <- array(NA, n.levels)
		line.prob <- array(1, n.levels)
		line.type <- array(NA, n.levels)
		line.effectiveness <- array(1, n.levels)
		line.utility <- array(0, n.levels)
		line.destiny <- array(NA, n.levels)
		
		for (j in nodes) {
			if (sum( x[,i] == j, na.rm = TRUE ) < 1) {
				Data.Node <- Data.level[ Data.level$Node.N == j,]
				
				father.node <- as.numeric(Data.Node$Father[1])
				label.father <- Data.Node$Father.Name[1]
				
				line.x[i] <- j
				line.y[i] <- Data.Node$Node.name[1]
				line.prob[i] <- Data.Node$Prob[1]
				line.type[i] <- Data.Node$Type[1]
				line.effectiveness[i] <- as.numeric(as.character(Data.Node$Payoff2[1]))
				line.utility[i] <- as.numeric(as.character(Data.Node$Payoff1[1]))
				line.destiny[i] <- Data.Node$Destiny[1]
				

				if (i > 1) {
					for (k in (i-1):1) {
						line.x[k] <- father.node
						line.y[k] <- label.father
						
						Data.node.return <- TheTree[TheTree$Level == k,]
						Data.node.return <- Data.node.return[Data.node.return$Node.N == as.character(line.x[k]),]
						father.node <- as.numeric(Data.node.return$Father[1])
						label.father <- Data.node.return$Father.Name
						prob.father <- Data.node.return$Prob
						type.father <- Data.node.return$Type
						effectiveness.father <- Data.node.return$Payoff2[1]
						utility.father <- Data.node.return$Payoff1[1]
						destiny.father <- Data.node.return$Destiny

						line.prob[k] <- prob.father
						line.type[k] <- type.father
						line.effectiveness[k] <- as.numeric(as.character(effectiveness.father))
						line.utility[k] <- as.numeric(as.character(utility.father))
						line.destiny[k] <- destiny.father

					}
				}
				x <- rbind(x,line.x)
				y <- rbind(y,line.y)
				probMAT <- rbind(probMAT,line.prob)
				typeMAT <- rbind(typeMAT,line.type)
				effectivenessMAT <- rbind(effectivenessMAT,line.effectiveness)
				utilityMAT <- rbind(utilityMAT,line.utility)
				destinyMAT <- rbind(destinyMAT,line.destiny)
			}
		}
	}
	
	x <- as.matrix(x)
	y <- as.matrix(y)
	probMAT <- as.matrix(probMAT)
	typeMAT <- as.matrix(typeMAT)
	effectivenessMAT <- as.matrix(effectivenessMAT)
	utilityMAT <- as.matrix(utilityMAT)
	destinyMAT <- as.matrix(destinyMAT)
	
# ordena as matrizes para nao haver problema com a plot.tree - June 21, 2008 
if(dim(x)[1] != 1) {
	for (i in 1:dim(x)[2]) {
		if ( sum(is.na(x[,i])) == 0 ) { # whatcolorder <- c(whatcolorder, i)
			y <- y[order(x[,i]),]
			probMAT <- probMAT[order(x[,i]),]
			typeMAT <- typeMAT[order(x[,i]),]
			effectivenessMAT <- effectivenessMAT[order(x[,i]),]
			utilityMAT <- utilityMAT[order(x[,i]),]
			destinyMAT <- destinyMAT[order(x[,i]),]
			x <- x[order(x[,i]),]
		}
	}
}
	
	x <- as.matrix(x)
	y <- as.matrix(y)
	probMAT <- as.matrix(probMAT)
	typeMAT <- as.matrix(typeMAT)
	effectivenessMAT <- as.matrix(effectivenessMAT)
	utilityMAT <- as.matrix(utilityMAT)
	destinyMAT <- as.matrix(destinyMAT)
	
	colnames(x) <- NULL
	rownames(x) <- NULL
	colnames(y) <- NULL
	rownames(y) <- NULL
	colnames(probMAT) <- NULL
	rownames(probMAT) <- NULL
	colnames(typeMAT) <- NULL
	rownames(typeMAT) <- NULL
	colnames(effectivenessMAT) <- NULL
	rownames(effectivenessMAT) <- NULL
	colnames(utilityMAT) <- NULL
	rownames(utilityMAT) <- NULL
	colnames(destinyMAT) <- NULL
	rownames(destinyMAT) <- NULL
	dl <- dim(destinyMAT)[1]
	destinyarray <- array(0,dl)
	for (i in 1:dl) {
		balde <- destinyMAT[i, !is.na(destinyMAT[i,]) ]
		destinyarray[i] <- balde[length(balde)]
	}
	ans <- list( x = x, y = y, probMAT = probMAT, typeMAT = typeMAT, effectivenessMAT = effectivenessMAT,
					utilityMAT = utilityMAT, destinyMAT = destinyarray)
	return(ans)
}

