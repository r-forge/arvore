`plot.tree` <-
function(TheTree, line.type = "squared", show.probability = TRUE, 
						show.payoffs = TRUE, show.notes = FALSE, node.name.font.size = 12,
						payoffs.font.size = 0, notes.font.size = 0) {
	require(grid)
	
	MatrixTheTree <- convert2matrix(TheTree)
	x <- MatrixTheTree$x
	rotulos <- MatrixTheTree$y
	typeMAT <- MatrixTheTree$typeMAT
	utilityMAT <- MatrixTheTree$utilityMAT
	effectivenessMAT <- MatrixTheTree$effectivenessMAT
	probMAT <- MatrixTheTree$probMAT
	notesMAT <- MatrixTheTree$notesMAT
	
	nc <- dim(x)[2]
	nl <- dim(x)[1]

	# Objetos
	colorMAT <- matrix(0,nl,nc)
	ylabelspace <- .021
	colortext <- "black"
	if (payoffs.font.size == 0) payoffs.font.size <- round(node.name.font.size/2,0)
	if (notes.font.size == 0) notes.font.size <- round(node.name.font.size/2,0)

	grid.newpage()
	
	sizelabels <- matrix(0,nl,nc)
	for (i in 1:nl) {
		for (j in 1:nc) {
			sizelabels[i,j] <- nchar(rotulos[i,j])
		}
	}
	
	propcolx <- apply(sizelabels, 2, max)
 	propcolx <- cumsum(propcolx/2)
	
	xpos <- NA*x
	ypos <- NA*x
	deltax <- 1 / ( max(propcolx) + 6 )

	for( i in 1:nc) {
		nniveis <- nl
		niveis <- levels(as.factor(x[,i]))
		deltay <- 1 / (nniveis + 1)
		for (j in niveis) {
			positions <- which(x[,i] == j)
			ypos[positions, i] <- (nl - median(positions, na.rm = TRUE)) * deltay + deltay
		}
		xpos[,i] <- rep(1, nl) * deltax * propcolx[i]
	}
	
	if (nc > 1) {
		if (line.type == "normal") {
			for( i in 1:nl) {
				for( j in 1:(nc-1)) {
					linx <- c( xpos[i,j] , xpos[i,j+1] )
					liny <- c( ypos[i,j] , ypos[i,j+1] )
					d <- sum(is.na(c(linx,liny)))
					if (d == 0) grid.polyline( linx, liny )
				}	
			}
		} else {
			
			for( i in 1:(nc-1)) {	# plota as linhas verticais
				nodos <- as.numeric(names(table(x[,i])))
				for (j in nodos) {
					positions <- which(x[,i] == j)
					if ( (length(positions) >= 2) && (!is.na(x[positions[1],i+1])) ) {
						linx <- c( xpos[positions[1],i] + (xpos[positions[1],i+1] - xpos[positions[1],i])/2 , xpos[positions[1],i] + (xpos[positions[1],i+1] - xpos[positions[1],i])/2 )
						linymax <- max(ypos[positions,i+1], na.rm = TRUE)
						linymin <- min(ypos[positions,i+1], na.rm = TRUE)
						liny <- c(linymin,linymax)
						grid.polyline( linx, liny )
					}
				}
			}
			for( i in 1:nl) {	# plota as linhas horizontais
				for( j in 1:(nc-1)) {
					linx <- c( xpos[i,j] , xpos[i,j] + (xpos[i,j+1] - xpos[i,j])/2 )
					liny <- c( ypos[i,j] , ypos[i,j] )
					d <- sum(is.na(c(linx,liny)))
					if (d == 0) grid.polyline( linx, liny )
				}
				# plota linhas depois do nome para os nodos do último nível em uma dada "linha da matriz estrutura"
					linx <- c( xpos[i,nc] , xpos[i,nc] + (xpos[i,nc] - xpos[i,nc-1])/2 )
					liny <- c( ypos[i,nc] , ypos[i,nc] )
					d <- sum(is.na(c(linx,liny)))
					if (d == 0) grid.polyline( linx, liny )
				for( j in 2:nc) {
					linx <- c( xpos[i,j-1] + (xpos[i,j] - xpos[i,j-1]) / 2, xpos[i,j] )
					liny <- c( ypos[i,j] , ypos[i,j] )
					d <- sum(is.na(c(linx,liny)))
					if (d == 0) grid.polyline( linx, liny )
				}
			}
		}
	}

	# computa a matriz de cores
	for( i in 1:nl) {
		for (j in 1:nc) {
				if ( (typeMAT[i,j] == "C")&&(!is.na(x[i,j])) ) colorMAT[i,j] <- "green"
				else if ( (typeMAT[i,j] == "T")&&(!is.na(x[i,j])) ) colorMAT[i,j] <- "red"
				else if ( (typeMAT[i,j] == "M")&&(!is.na(x[i,j])) ) colorMAT[i,j] <- "yellow"
				else if ( (typeMAT[i,j] == "D")&&(!is.na(x[i,j])) ) colorMAT[i,j] <- "blue"
				else colorMAT[i,j] <- "grey"
		}
	}
	
	# plota grafico para o primeiro nodo
	grid.text(rotulos[1,1], x = xpos[1,1],
		y = ypos[1,1] + ylabelspace,
		just = "centre",
		rot = 0, gp = gpar(fontsize = node.name.font.size, col = colortext))
	
	if (nc > 1) {
		if ( line.type == "squared") {
			grid.circle(x = xpos[1,1] + (xpos[1,2]-xpos[1,1])/2, 
						y = ypos[1,1], 
						r = .012, default.units="npc", name=NULL,
		    			gp=gpar(fill=colorMAT[1,1]), 
		    			draw=TRUE, vp=NULL)
	   	} else {
			grid.circle(x = xpos[1,1], 
						y = ypos[1,1], 
						r = .012, default.units="npc", name=NULL,
		    			gp=gpar(fill=colorMAT[1,1]), 
		    			draw=TRUE, vp=NULL)
	   	}
		for( i in 1:nl) {
			for (j in 2:nc) {
				d <- sum(is.na(c(xpos[i,j],ypos[i,j])))
				if (d == 0) {
					grid.text(rotulos[i,j], x = xpos[i,j] ,
								y = ypos[i,j] + ylabelspace,
								just = "centre",
								rot = 0, gp = gpar(fontsize = node.name.font.size, col=colortext))
					minortext <- as.character("")
					if (show.probability) minortext <- paste("prob. ",probMAT[i,j], sep = "")
					if (show.payoffs) {
						minortext <- paste(minortext, "\n cost. ", utilityMAT[i,j], sep = "")
						if (.modeltypeArvore == "CE") {
							minortext <- paste(minortext, "\n effect. ", effectivenessMAT[i,j], sep = "")
						}
					}
					grid.text(minortext, 
								x = xpos[i,j],
								y = ypos[i,j] - 2 * ylabelspace,
								just = "centre",
								rot = 0, gp = gpar(fontsize = payoffs.font.size, col=colortext))
					if (show.notes) {
						nreptext <- sum(c(show.probability, show.payoffs, (.modeltypeArvore == "CE")))
						minortext2 <- paste(rep("\n",nreptext), notesMAT[i,j], sep = "")
						grid.text(minortext2, 
									x = xpos[i,j],
									y = ypos[i,j] - 2 * ylabelspace,
									just = "centre",
									rot = 0, gp = gpar(fontsize = notes.font.size, col=colortext))
					}
					# Desenhos dos nodos - para o caso "normal" e "squared"
					if ( line.type == "squared") {
						if ( j != nc) {
							if (typeMAT[i,j] != "T") {
								grid.circle(x = xpos[i,j] + (xpos[i,j+1] - xpos[i,j])/2,
											y = ypos[i,j], 
											r = .012, default.units="npc", name=NULL,
				            				gp=gpar(fill=colorMAT[i,j]), 
				            				draw=TRUE, vp=NULL)
			            	} else {
				            	triangX <- xpos[i,j] + (xpos[i,j+1] - xpos[i,j])/2
								grid.polygon(x = c( triangX, triangX + .015, triangX + .015),
											 y = c( ypos[i,j], ypos[i,j] + .015, ypos[i,j] - .015),
											default.units="npc", name=NULL,
				            				gp=gpar(fill=colorMAT[i,j]), 
				            				draw=TRUE, vp=NULL)
			            	}
		    			} else {
							if (typeMAT[i,j] != "T") {
								grid.circle(x = xpos[i,j] + (xpos[i,j] - xpos[i,j-1])/2,
											y = ypos[i,j], 
											r = .012, default.units="npc", name=NULL,
				            				gp=gpar(fill=colorMAT[i,j]), 
				            				draw=TRUE, vp=NULL)
			            	} else {
				            	triangX <- xpos[i,j] + (xpos[i,j] - xpos[i,j-1])/2
								grid.polygon(x = c( triangX, triangX + .015, triangX + .015),
											 y = c( ypos[i,j], ypos[i,j] + .015, ypos[i,j] - .015),
											default.units="npc", name=NULL,
				            				gp=gpar(fill=colorMAT[i,j]), 
				            				draw=TRUE, vp=NULL)
			            	}
		    			}
					} else {
						if ( j != nc) {
							if (typeMAT[i,j] != "T") {
								grid.circle(x = xpos[i,j],
											y = ypos[i,j], 
											r = .012, default.units="npc", name=NULL,
				            				gp=gpar(fill=colorMAT[i,j]), 
				            				draw=TRUE, vp=NULL)
			            	} else {
				            	triangX <- xpos[i,j] 
								grid.polygon(x = c( triangX, triangX + .015, triangX + .015),
											 y = c( ypos[i,j], ypos[i,j] + .015, ypos[i,j] - .015),
											default.units="npc", name=NULL,
				            				gp=gpar(fill=colorMAT[i,j]), 
				            				draw=TRUE, vp=NULL)
			            	}
		    			} else {
							if (typeMAT[i,j] != "T") {
								grid.circle(x = xpos[i,j],
											y = ypos[i,j], 
											r = .012, default.units="npc", name=NULL,
				            				gp=gpar(fill=colorMAT[i,j]), 
				            				draw=TRUE, vp=NULL)
			            	} else {
				            	triangX <- xpos[i,j]
								grid.polygon(x = c( triangX, triangX + .015, triangX + .015),
											 y = c( ypos[i,j], ypos[i,j] + .015, ypos[i,j] - .015),
											default.units="npc", name=NULL,
				            				gp=gpar(fill=colorMAT[i,j]), 
				            				draw=TRUE, vp=NULL)
			            	}
		    			}
					}
				}
			}
		}
	} else {
		grid.circle(x = xpos[1,1] + (xpos[1,1])/2, 
					y = ypos[1,1], 
					r = .012, default.units="npc", name=NULL,
	    			gp=gpar(fill=colorMAT[1,1]), 
	    			draw=TRUE, vp=NULL)
	}
	
}

