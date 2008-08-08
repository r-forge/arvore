`nodoselecionado` <-
function() {
	ans <- tclvalue(tcl(treeWidget,"selection","get"))
	if ( ans == "") {
		return(" ")
	} else {
		pos <- 1
		while (pos <= nchar(ans)) {
			if ( substr(ans, pos, pos) == "." ) {
				ans.node <- substr(ans,1,pos-1)
				ans.col <- substr(ans,pos+1,nchar(ans))
				pos <- nchar(ans) + 1
			}
			pos <- pos + 1
		}	
		return(c(ans,ans.node,ans.col))	
	}
}

