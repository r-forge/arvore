`theTreeTkArvore` <-
function(TheTree) {
	
	num.lin <- dim(TheTree)[1]
	num.levels <- max(TheTree$Level)
	
	for (i in 1:length(.libPaths())) {
		SubDataSet <- subset(TheTree, Level == 1)
		osnodos <- SubDataSet$Node.N
		osnodosnomes <- SubDataSet$Node.name
		osnodostipos <- SubDataSet$Type
		osnodos <- paste(i,".",osnodos,sep="")

		icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/", osnodostipos,".png",sep=""))
		if (file.exists(icon.but)) {
			icn <- tkimage.create("photo", file=icon.but)
			tkinsert(treeWidget,"end","root","1.1",text=osnodosnomes, image = icn)
		} else {
			tkinsert(treeWidget,"end","root","1.1",text=osnodosnomes)
		}
	}
	
	if (num.lin > 1) {
		for (i in 2:num.levels) {
			SubDataSet <- subset(TheTree, Level == i)
			osnodos <- SubDataSet$Node.N
			paisnodos <- SubDataSet$Father
			osnodosnomes <- SubDataSet$Node.name
			osnodostipos <- SubDataSet$Type
# 	  		cat("DEBUG : Criei os nodos \n ", osnodos, " cujos pais são ", paisnodos, "\n")
				
			osnodos <- paste(i,".",osnodos,sep="")
			paisnodos <- paste((i-1),".",paisnodos,sep="")
				
			for (j in 1:length(osnodos)) {
					tipofilename <- osnodostipos[j]
					for (i in 1:length(.libPaths())) {
						icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/", tipofilename,".png",sep=""))
						if (file.exists(icon.but)) {
							icn <- tkimage.create("photo", file=icon.but)
							tkinsert(treeWidget,"end",paisnodos[j],osnodos[j],text=osnodosnomes[j], image = icn)
						} else {
							tkinsert(treeWidget,"end",paisnodos[j],osnodos[j],text=osnodosnomes[j])
						}
					}
			}
		}
	}
}

