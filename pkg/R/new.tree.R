`new.tree` <-
function() {
	Payoffs <- matrix(0,1,2)
	
	colnames(Payoffs) <- paste("Payoff",1:length(Payoffs),sep="")
	
	TheTree <- data.frame( Level = 1, Node.N = 1, Node.name = "Decision",
							 Father = 0, Father.Name = "root",
							 Prob = 1, Type = "D", Note = " ", Destiny = " ",
							 Payoffs)
	
	TheTree$Level <- as.numeric(TheTree$Level)
	TheTree$Node.N <- as.numeric(TheTree$Node.N)
	TheTree$Node.name <- as.character(TheTree$Node.name)
	TheTree$Father <- as.numeric(TheTree$Father)
	TheTree$Father.Name <- as.character(TheTree$Father.Name)
	TheTree$Prob <- as.numeric(TheTree$Prob)
	TheTree$Type <- as.character(TheTree$Type)
	TheTree$Note <- as.character(TheTree$Note)
	TheTree$Destiny <- as.character(TheTree$Destiny)
	TheTree$Payoff1 <- as.numeric(TheTree$Payoff1)
	TheTree$Payoff2 <- as.numeric(TheTree$Payoff2)
	
	markov.propertiesMAT <- data.frame(	"Level" = array(,0),
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
											
	assign("TheTree", TheTree, .EnvironmentArvoRe)
	assign("markov.propertiesMAT", markov.propertiesMAT, .EnvironmentArvoRe)
}

