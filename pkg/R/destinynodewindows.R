`destinynodewindows` <-
function(...) {
	nodeSec <- nodoselecionado()
	if ( .modeltypeArvore != "CE") {
		msg <- paste(" Você não está utilizando um modelo Markov.\n Altere o tipo de modelo para poder definir destino a um nodo.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		if ( nodeSec[1] == " ") {
			msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
			tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
			tkfocus(tt)
		} else {
			node.number <- as.numeric(nodeSec[3])
			column <- as.numeric(nodeSec[2])
			position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
	
			node.type <- TheTree$Type[position]
			
			
			if (node.type == "T") {
				node.Origins <- select.origins(TheTree, node.col = column, node.number = node.number)
				position.exist.markov <- which(node.Origins$Type == "M")
				
				if (length(position.exist.markov) > 0) {
					destinyWindow <- tktoplevel()
					title <- "ÁrvoRe - Destino do Nodo"
					tkwm.title(destinyWindow,title)
					
					position.exist.markov <- max(position.exist.markov)
					column.markov <- node.Origins$Level[position.exist.markov]
					number.markov.node <- node.Origins$Node.N[position.exist.markov]
					
					k <- subset(TheTree, Level == column.markov + 1)
					k <- subset(k, Father == number.markov.node)
					k <- k[union( which(k$Type == "C"), which(k$Type == "T")), ]
					
					markov.nodes <- as.character(k$Node.name)
					markov.nodes.position <- as.numeric(k$Node.N)
					markov.nodes.col <- as.numeric(k$Level)
					
					heightlistbox <- length(markov.nodes)
					
					scr <- tkscrollbar(destinyWindow, repeatinterval=5, command=function(...)tkyview(tl,...))
					
					tl <- tklistbox(destinyWindow,height=heightlistbox,selectmode="single",
									yscrollcommand=function(...)tkset(scr,...),background="white")
					tkgrid(tklabel(destinyWindow,text="Seleciona um nodo de destino"))
					tkgrid(tl,scr)
					tkgrid.configure(scr,rowspan=4,sticky="nsw")
					
					for (i in (1:heightlistbox)) {
					    tkinsert(tl,"end",markov.nodes[i])
					}
					
					if(TheTree$Destiny[position[1]] != " ") {
						selected <- which( markov.nodes.position == as.numeric(TheTree$Destiny[position[1]]))
						tkselection.set(tl,selected-1)
					}
					
					OnOK <- function()
					{
						destinyChoice <- markov.nodes.position[as.numeric(tkcurselection(tl))+1]
						safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
						TheTree$Destiny[position] <- destinyChoice
						setdestinynode(TheTree, .EnvironmentArvoRe)
						tkdestroy(destinyWindow)
						tkfocus(tt)
					}
					
					OnCancel <- function()
					{
						tkdestroy(destinyWindow)
						tkfocus(tt)
					}
					
					OK.but <-tkbutton(destinyWindow,text="   OK     ",command=OnOK)
					tkbind(destinyWindow, "<Return>",OnOK)
					Cancel.but <-tkbutton(destinyWindow,text=" Cancelar ",command=OnCancel)
					tkbind(destinyWindow, "<Escape>",OnCancel)
			
					tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
					
					posiciona.janela.no.mouse(destinyWindow, 230, 150)
					
					tkfocus(destinyWindow)
				} else {
					msg <- paste("O nodo selecionado não é um nodo de transição de um nodo tipo 'Markov'. \n Apenas nodos desse tipo podem seguir um destino.")
					tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
					tkfocus(tt)
				}
			} else {
				msg <- paste("O nodo selecionado não é um nodo do tipo 'Terminal'. \n Apenas nodos desse tipo podem seguir um destino.")
				tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
				tkfocus(tt)
			}
			
		}
	}
}

