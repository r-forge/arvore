`set.markov.nodes.properties` <-
function(TheTree, markov.propertiesMAT, column, node.number, 
					Initial.rwd.cost = 0, 
					Incremental.rwd.cost = 0, 
					Final.rwd.cost = 0,
					Initial.rwd.effectiveness = 1, 
					Incremental.rwd.effectiveness = 1, 
					Final.rwd.effectiveness = 1	) {
	
	require(abind)
	
	if (!is.numeric(node.number)) node.number <- as.numeric(node.number)
	if (!is.numeric(column)) column <- as.numeric(column)
	
	position.markov <- intersect(which((markov.propertiesMAT$Level == column)),
									which(markov.propertiesMAT$Node.N == node.number))
	position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
	
	if (length(position.markov) != 0) {
		markov.propertiesMAT$Node.name[position.markov] <- TheTree$Node.name[position]
		markov.propertiesMAT$Father[position.markov] <- TheTree$Father[position]
		markov.propertiesMAT$Father.Name[position.markov] <- TheTree$Father.Name[position]		
		markov.propertiesMAT$Initial.cost[position.markov] <- Initial.rwd.cost
		markov.propertiesMAT$Incremental.cost[position.markov] <- Incremental.rwd.cost
		markov.propertiesMAT$Final.cost[position.markov] <- Final.rwd.cost
		markov.propertiesMAT$Initial.effectiveness[position.markov] <- Initial.rwd.effectiveness
		markov.propertiesMAT$Incremental.effectiveness[position.markov] <- Incremental.rwd.effectiveness
		markov.propertiesMAT$Final.effectiveness[position.markov] <- Final.rwd.effectiveness
	} else {
		markov.propertiesLINE <- data.frame("Level" = column,
											"Node.N" = node.number,
											"Node.name" = TheTree$Node.name[position],
											"Father" = TheTree$Father[position],
											"Father.Name" = TheTree$Father.Name[position],
											"Initial.cost" = Initial.rwd.cost, 
											"Incremental.cost" = Incremental.rwd.cost, 
											"Final.cost" = Final.rwd.cost,
											"Initial.effectiveness" = Initial.rwd.effectiveness, 
											"Incremental.effectiveness" = Incremental.rwd.effectiveness, 
											"Final.effectiveness" = Final.rwd.effectiveness)
		markov.propertiesMAT <- abind(markov.propertiesMAT, markov.propertiesLINE, along=1)
		markov.propertiesMAT <- as.data.frame(markov.propertiesMAT)
		
		markov.propertiesMAT$Level <- as.numeric(as.character(markov.propertiesMAT$Level))
		markov.propertiesMAT$Node.N <- as.numeric(as.character(markov.propertiesMAT$Node.N))
		markov.propertiesMAT$Node.name <- (as.character(markov.propertiesMAT$Node.name))
		markov.propertiesMAT$Father <- as.numeric(as.character(markov.propertiesMAT$Father))
		markov.propertiesMAT$Father.Name <- (as.character(markov.propertiesMAT$Father.Name))
		markov.propertiesMAT$Initial.cost <- as.numeric(as.character(markov.propertiesMAT$Initial.cost))
		markov.propertiesMAT$Incremental.cost <- as.numeric(as.character(markov.propertiesMAT$Incremental.cost))
		markov.propertiesMAT$Final.cost <- as.numeric(as.character(markov.propertiesMAT$Final.cost))
		markov.propertiesMAT$Initial.effectiveness <- as.numeric(as.character(markov.propertiesMAT$Initial.effectiveness))
		markov.propertiesMAT$Incremental.effectiveness <- as.numeric(as.character(markov.propertiesMAT$Incremental.effectiveness))
		markov.propertiesMAT$Final.effectiveness <- as.numeric(as.character(markov.propertiesMAT$Final.effectiveness))

	}
	
	setutility(TheTree, column, node.number, Incremental.rwd.cost, .EnvironmentArvoRe)
	TheTree <- get("TheTree", .EnvironmentArvoRe)
	seteffectiveness(TheTree, column, node.number, Incremental.rwd.effectiveness, .EnvironmentArvoRe)
	
	assign("markov.propertiesMAT", markov.propertiesMAT, envir = .EnvironmentArvoRe)
	assign(".workstatus", "unsaved", .EnvironmentArvoRe)
}

