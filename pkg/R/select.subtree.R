`select.subtree` <-
function(TheTree, node.col, node.number, change.row.names = FALSE) {
	require(abind)
	levelmax <- max(TheTree$Level)
	variables <- names(TheTree)
	
	ans <- subset(TheTree, Level == node.col, select = variables)
	ans <- subset(ans, Node.N == node.number, select = variables)
# 	print(ans)
	
	if (node.col != levelmax) {
		i <- (node.col+1)
		pais <- node.number
		while (i != 0) {
			
			Datatmp <- subset(TheTree, Level == i, select = variables)
			novos.pais <- array(,0)
			for (j in pais) {
				DatatmpP <- subset(Datatmp, Father == j, select = variables)
				if (dim(DatatmpP)[1] != 0) {
					ans <- abind(ans, DatatmpP, along=1)
# 					print(ans)
					novos.pais <- c(novos.pais, DatatmpP$Node.N)
				}
			}
			pais <- novos.pais
			
			if (i == levelmax) {
				i <- 0
			} else {
				i <- i + 1
			}
			if( length(pais) == 0) i <- 0
		}
	}
	
	ans <- as.data.frame(ans)
	
	ans$Level <- as.numeric(ans$Level)
	ans$Node.N <- as.numeric(as.character(ans$Node.N))
	ans$Node.name <- as.character(ans$Node.name)
	ans$Father <- as.numeric(as.character(ans$Father))
	ans$Father.Name <- as.character(ans$Father.Name)
	ans$Prob <- as.numeric(as.character(ans$Prob))
	ans$Type <- as.character(ans$Type)
	ans$Note <- as.character(ans$Note)
	ans$Destiny <- as.character(ans$Destiny)
	ans$Payoff1 <- as.numeric(as.character(ans$Payoff1))
	ans$Payoff2 <- as.numeric(as.character(ans$Payoff2))
# 	
# 	# Ajusta a numeração dos nodos
# 	levelmax <- max(ans$Level)
# 	for (i in 1:levelmax) {
# 		positions <- which(ans$Level == i)
# 		n.node <- as.numeric(names(table(ans$Node.N[positions])))
# 		size.n.node <- length(n.node)
# 		for (j in 1:size.n.node) {
# 			positions.node.replace <- which(ans$Node.N == n.node[j])
# 			positions.node.replace <- intersect(positions, positions.node.replace)
# 			ans$Node.N[positions.node.replace] <- j
# 			if (i != levelmax) {
# 				positions.next.level <- which(ans$Level == (i+1))
# 				positions.node.as.father <- which(ans$Father == n.node[j])
# 				positions.node.as.father <- intersect(positions.next.level, positions.node.as.father)
# 				ans$Father[positions] <- j
# 			}
# 		}
# 	}
	ans <- ans[ order(ans$Level,ans$Node.N),]
 	if (change.row.names) rownames(ans) <- NULL
	return(ans)
}

