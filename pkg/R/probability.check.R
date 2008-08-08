`probability.check` <-
function(k) {
	names(k) <- c("Level", "Node.N", "Node.name", "Father", "Father.Name",
 					"Prob", "Type", "Note", "Destiny", "Payoff1", "Payoff2")
	Levels <- 2:max(k$Level)
	variables <- names(k)
	
	ans <- ""
	for (i in Levels) {
		Data <- subset(k, Level == i, select = variables)
		nodes <- as.numeric(names(table(Data$Father)))
		for (j in nodes) {
			Data2 <- subset(Data, Father == j, select = variables)
			psum <- sum(Data2$Prob)
			if (psum != 1) {
				nome.pai <- Data2$Father.Name[1]
				ans <- paste(ans, 
				"Há problema em [ NÍVEL = ", i-1, ", NODO = ", nome.pai, " ] \n", sep = "")
			}
		}
	}
	ans2 <- "1"
	if (nchar(ans) == 0) {
		ans <- "As probabilidades somam 1. Tudo ok!"
		ans2 <- "0"
	}
		
	return(c(ans,ans2))
}

