`select.markov.propertiesMAT` <-
function(TheTree, SubTree, markov.propertiesMAT) {
	require(abind)
	
	selected.lines <- rownames(SubTree)
	check.tree <- TheTree[selected.lines,]
	wanted.level <- check.tree$Level[1]+1
	check.tree <- check.tree[check.tree$Level == wanted.level,]
	ans <- data.frame(	"Level" = array(,0),
						"Node.N" = array(,0),
						"Node.name" = array(,0),
						"Father" = array(,0),
						"Father.Name" = array(,0),
						"Initial.cost" = array(,0), 
						"Incremental.cost" = array(,0), 
						"Final.cost" = array(,0),
						"Initial.effectiveness" = array(,0), 
						"Incremental.effectiveness" = array(,0), 
						"Final.effectiveness" = array(,0))
	for (i in 1:length(check.tree$Node.N) ) {
		balde <- subset(markov.propertiesMAT, Node.N == check.tree$Node.N[i])
		n.lin.balde <- dim(balde)[1]
		if (n.lin.balde > 0) {
			ans <- abind(ans, balde, along = 1)
		} else {
			balde <- data.frame(	"Level" = check.tree$Level[i],
									"Node.N" = check.tree$Node.N[i],
									"Node.name" = check.tree$Node.name[i],
									"Father" = check.tree$Father[i],
									"Father.Name" = check.tree$Father.Name[i],
									"Initial.cost" = 0, 
									"Incremental.cost" = check.tree$Payoff1[i], 
									"Final.cost" = 0,
									"Initial.effectiveness" = 0, 
									"Incremental.effectiveness" = check.tree$Payoff2[i], 
									"Final.effectiveness" = 0)
			ans <- abind(ans, balde, along = 1)
		}
	}
	ans <- as.data.frame(ans)
	
	wanted.level.sub <- SubTree$Level[1]+1
	subSubTree <- subset(SubTree, Level == wanted.level.sub)
	ans$Level <- subSubTree$Level
	ans$Node.N <- subSubTree$Node.N
	ans$Father <- subSubTree$Father
	ans$Father.Name <- subSubTree$Father.Name
	rownames(ans) <- rownames(subSubTree)
	
	ans$Level <- as.numeric(as.character(ans$Level))
	ans$Node.N <- as.numeric(as.character(ans$Node.N))
	ans$Node.name <- (as.character(ans$Node.name))
	ans$Father <- as.numeric(as.character(ans$Father))
	ans$Father.Name <- (as.character(ans$Father.Name))
	ans$Initial.cost <- as.numeric(as.character(ans$Initial.cost))
	ans$Incremental.cost <- as.numeric(as.character(ans$Incremental.cost))
	ans$Final.cost <- as.numeric(as.character(ans$Final.cost))
	ans$Initial.effectiveness <- as.numeric(as.character(ans$Initial.effectiveness))
	ans$Incremental.effectiveness <- as.numeric(as.character(ans$Incremental.effectiveness))
	ans$Final.effectiveness <- as.numeric(as.character(ans$Final.effectiveness))
	
	return(ans)
}

