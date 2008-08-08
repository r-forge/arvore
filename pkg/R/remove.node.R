`remove.node` <-
function(TheTree, node.col, node.number) {
	removelines <- select.subtree(TheTree, node.col, node.number, change.row.names = FALSE)
	removelines <- rownames(removelines)
	
	num.lin <- dim(TheTree)[1]
	
	whoiwant <- as.numeric(setdiff(as.character(1:num.lin), removelines))
	
	ans <- TheTree[whoiwant,]
	
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
	
	position <- which(ans$Level == 2)
	
if( ( length(position) > 1 ) && ( dim(ans)[1] > 2 )) {
	#- Correção para o primeiro do nível ---------------------------------------------------------------
	.stopit <- FALSE
	i <- 1
	nans <- dim(ans)[1]
	while ( !.stopit ) {
		i <- i + 1
		GTtflag <- 	( as.numeric(ans$Node.N[i]) != 1 ) &&
					( as.numeric(ans$Level[i]) > as.numeric(ans$Level[i-1]) )
			if (GTtflag) {
				old.value <- ans$Node.N[i]
				ans$Node.N[i] <- 1
				usedlevel <- ans$Level[i] + 1
				position <- intersect(which(ans$Level == usedlevel),which(ans$Father == old.value))
				if ( length(position) > 0) {
					ans$Father[position] <- ans$Node.N[i]
					ans$Father.Name[position] <- ans$Node.name[i]
				}
				ans <- ans[ order(ans$Level,ans$Father, ans$Node.N),]
				i <- 1
			} else {
				if (i >= nans) .stopit <- TRUE
			}
		}

	#- Correção para numeracao dos nodos -------------------------------------------------------------
	.stopit <- FALSE
	i <- 1
	nans <- dim(ans)[1]
	while ( !.stopit ) {
		i <- i + 1
		GTtflag <- 	( as.numeric(ans$Node.N[i]) > as.numeric(ans$Node.N[i-1])+1 ) &&
					( as.numeric(ans$Level[i]) == as.numeric(ans$Level[i-1]) )
			if (GTtflag) {
				old.value <- ans$Node.N[i]
				ans$Node.N[i] <- ans$Node.N[i-1] + 1
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
	#--------------------------------------------------------------------------------------------------
}		
	rownames(ans) <- NULL
	return(ans)
}

