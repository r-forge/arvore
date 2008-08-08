`posiciona.janela.tela` <-
function(janela.nova) {
	MAX.height <- as.integer( tclvalue( tkwinfo("screenheight", janela.nova) ) )
	MAX.width <- as.integer( tclvalue( tkwinfo("screenwidth", janela.nova) ) )
	
	wm.height <- as.integer( tclvalue( tkwinfo("height", janela.nova) ) )
	wm.width <- as.integer( tclvalue( tkwinfo("width", janela.nova) ) )

	new.wm.y <- round( MAX.height/2 - wm.height/2 )
	new.wm.x <- round( MAX.width/2 - wm.width/2 )
	
	limite.sup.x <- round( MAX.width - wm.width )
	limite.inf.x <- 0
	limite.sup.y <- round( MAX.height - wm.height )
	limite.inf.y <- 0
	
	# Limitantes para o tamanho da tela. Quem tem tela virtural... #$%#$%
	if (new.wm.x > limite.sup.x) new.wm.x <- limite.sup.x
	if (new.wm.x < limite.inf.x) new.wm.x <- limite.inf.x
	if (new.wm.y > limite.sup.y) new.wm.y <- limite.sup.y
	if (new.wm.y < limite.inf.y) new.wm.y <- limite.sup.y
	
	posicao <- paste(wm.width, "x", wm.height, "+", new.wm.x,"+", new.wm.y, sep="")
	tkwm.geometry(janela.nova,posicao)
}

