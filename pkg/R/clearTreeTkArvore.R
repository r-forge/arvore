`clearTreeTkArvore` <-
function(TheTree) {
	i <- TheTree$Level
	j <- TheTree$Node.N
	
	osnodos <- paste(i,".",j,sep="")
	tkdelete(treeWidget,osnodos[j])
	tkdelete(treeWidget,"1.1")	
}

