`add.node` <-
function(TheTree, node.col, node.number, node.name, node.prob, node.type = "C",
						node.notes = " ", node.destiny = " ", node.utility = 0, node.effectiveness = 0,
						playnumb = 2) {
	require(abind)
	
	variables <- names(TheTree)
	
	num.col <- dim(TheTree)[2]
	num.lin <- dim(TheTree)[1]
	
	Levelmax <- max(TheTree$Level)
	new.node.level <- node.col + 1
	
	Data.father <- subset(TheTree, Level == node.col, select = variables)
	Data.father <- subset(Data.father, Node.N == node.number, select = variables)
	father.name <- Data.father$Node.name[1]
	
	if(new.node.level <= Levelmax) {
		Data <- subset(TheTree, Level == new.node.level, select = variables)
		new.node.number <- max(Data$Node.N) + 1
	} else {
		new.node.number <- 1
	}
	
	Payoffs <- matrix(c(0,1), 1, playnumb)
	
	colnames(Payoffs) <- paste("Payoff",1:length(Payoffs),sep="")

	ans <- data.frame(	Level = new.node.level, Node.N = new.node.number, Node.name = node.name,
						 Father = node.number, Father.Name = father.name,
						 Prob = node.prob, Type = node.type, Note = node.notes, Destiny = node.destiny,
						 Payoff1 = node.utility, Payoff2 = node.effectiveness)
	ans <- abind(TheTree, ans, along=1)
	ans <- as.data.frame(ans)
	
	ans$Level <- as.numeric(as.character(ans$Level))
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
	
	ans <- ans[ order(ans$Level,ans$Father, ans$Node.N),]
	
	.stopit <- FALSE
	i <- 1
	nans <- dim(ans)[1]
	while ( !.stopit ) {
		i <- i + 1
		GTtflag <- 	( as.numeric(ans$Node.N[i]) < as.numeric(ans$Node.N[i-1]) ) &&
					( as.numeric(ans$Level[i]) == as.numeric(ans$Level[i-1]) )
			if (GTtflag) {
				old.value <- ans$Node.N[i-1]
				ans$Node.N[i-1] <- ans$Node.N[i]
				ans$Node.N[i] <- old.value
				usedlevel <- ans$Level[i-1] + 1
				position <- intersect(which(ans$Level == usedlevel),which(ans$Father == old.value))
				if ( length(position) > 0) {
					ans$Father[position] <- old.value
					ans$Father.Name[position] <- ans$Node.name[i-1]
				}
				ans <- ans[ order(ans$Level,ans$Father, ans$Node.N),]
				i <- 1
			} else {
				if (i >= nans) .stopit <- TRUE
			}
		}
		
	rownames(ans) <- NULL
	return(ans)
}

