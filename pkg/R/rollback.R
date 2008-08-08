`rollback` <-
function(TheTree) {
	Matrixset <- convert2matrix(TheTree)
	
	x <- Matrixset$x
	probMAT <- Matrixset$probMAT
	utilityMAT <- Matrixset$utilityMAT
	effectivenessMAT <- Matrixset$effectivenessMAT
	
	num.col <- dim(probMAT)[2]
	num.lin <- dim(probMAT)[1]
	
	ans.ce <- matrix(0, num.lin, num.col)
	ans.cost <- matrix(0, num.lin, num.col)
	ans.effectiveness <- matrix(0, num.lin, num.col)
	
	for (i in 1:(num.col)) {
		nodes <- as.numeric(names(table(x[,i])))
		for (j in nodes) {
			position <- which(x[,i] == j)
			sub.x <- x[position, i:num.col]
			lines.sub <- length(position)
			column.sub <- num.col - i + 1
			sub.x <- matrix(sub.x, lines.sub, column.sub)
			sub.prob <- probMAT[position, i:num.col]
			sub.prob <- matrix(sub.prob, lines.sub, column.sub)
			sub.util <- utilityMAT[position, i:num.col]
			sub.util <- matrix(sub.util, lines.sub, column.sub)
			sub.effectiveness <- effectivenessMAT[position, i:num.col]
			sub.effectiveness <- matrix(sub.effectiveness, lines.sub, column.sub)
			
			if (is.null(sub.prob)) {
				sub.prob[,1] <- 1
				sub.util[,1] <- 0
				sub.effectiveness[,1] <- 1
				val.expected.ce <- sum ( apply(sub.prob, 1, prod) * apply(sub.util/sub.effectiveness, 1, sum) )
				val.expected.cost <- sum ( apply(sub.prob, 1, prod) * apply(sub.util, 1, sum) )
				val.expected.effectiveness <- sum ( apply(sub.prob, 1, prod) * apply(sub.effectiveness, 1, sum) )
			} else {
				sub.prob[,1] <- 1
				val.expected <- sum ( apply(sub.prob,1,prod) * apply(sub.util/sub.effectiveness,1,sum) )
				val.expected.cost <- sum ( apply(sub.prob, 1, prod) * apply(sub.util, 1, sum) )
				val.expected.effectiveness <- sum ( apply(sub.prob, 1, prod) * apply(sub.effectiveness, 1, sum) )
			}
			ans.ce[position, i] <- val.expected
			ans.cost[position, i] <- val.expected.cost
			ans.effectiveness[position, i] <- val.expected.effectiveness
		}	
	}
	ans <- list("CE" = ans.ce, "Cost" = ans.cost, "Effectiveness" = ans.effectiveness)
	return(ans)
}

