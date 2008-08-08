`probString2Numeric` <-
function(probMAT) {
	n.lin <- dim(probMAT)[1]
	n.col <- dim(probMAT)[2]
	
	ans <- matrix(, n.lin, n.col)
	
	for (i in 1:n.lin) {
		for (j in 1:n.col) {
			ans[i,j] <- exec.text(probMAT[i,j])
		}
	}
	return(ans)
}

