`settreevartype` <-
function(TheTree) {
	TheTree$Level <- as.numeric(TheTree$Level)
	TheTree$Node.N <- as.numeric(TheTree$Node.N)
	TheTree$Node.name <- as.character(TheTree$Node.name)
	TheTree$Father <- as.numeric(TheTree$Father)
	TheTree$Father.Name <- as.character(TheTree$Father.Name)
	TheTree$Prob <- as.numeric(TheTree$Prob)
	TheTree$Type <- as.character(TheTree$Type)
	TheTree$Note <- as.character(TheTree$Note)
	TheTree$Destiny <- as.character(TheTree$Destiny)
	TheTree$Payoff1 <- as.numeric(as.character(TheTree$Payoff1))
	TheTree$Payoff2 <- as.numeric(as.character(TheTree$Payoff2))
	assign("TheTree", TheTree, .EnvironmentArvoRe)
}

