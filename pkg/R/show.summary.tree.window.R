`show.summary.tree.window` <-
function(...) {
	k <- TheTree
	
	displayInTable(as.matrix(k), title="Informa��o da �rvore",
					height=10,width=8,nrow=dim(k)[1],ncol=dim(k)[2], 
					titlerows = FALSE, titlecols = TRUE)
}

