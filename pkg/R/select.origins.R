`select.origins` <-
function(TheTree, node.col, node.number) {
	require(abind)
	position <- intersect(which((TheTree$Level == node.col)),which(TheTree$Node.N == node.number))
	ans <- TheTree[position,]
	
	levelnodevalue <- node.col - 1
	nodenumbervalue <- ans$Father[1] #[position]
	
	while ( levelnodevalue > 0) {
		position <- intersect(which((TheTree$Level == levelnodevalue)),which(TheTree$Node.N == nodenumbervalue))
		subData <- TheTree[position,]
		ans <- abind(subData, ans, along=1)
		nodenumbervalue <- subData$Father[1]
		levelnodevalue <- levelnodevalue - 1
	}
	ans <- as.data.frame(ans)
	
	ans$Level <- as.numeric(ans$Level)
	ans$Node.N <- as.numeric(ans$Node.N)
	ans$Node.name <- as.character(ans$Node.name)
	ans$Father <- as.numeric(ans$Father)
	ans$Father.Name <- as.character(ans$Father.Name)
	ans$Prob <- as.numeric(ans$Prob)
	ans$Type <- as.character(ans$Type)
	ans$Note <- as.character(ans$Note)
	ans$Destiny <- as.character(ans$Destiny)
	ans$Payoff1 <- as.numeric(as.character(ans$Payoff1))
	ans$Payoff2 <- as.numeric(as.character(ans$Payoff2))

	return(ans)
}

