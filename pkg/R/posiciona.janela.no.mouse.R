`posiciona.janela.no.mouse` <-
function(janela.nova, wm.width = -1, wm.height = -1) {
	MAX.height <- as.integer( tclvalue( tkwinfo("screenheight", janela.nova) ) )
	MAX.width <- as.integer( tclvalue( tkwinfo("screenwidth", janela.nova) ) )

	if (wm.height == -1) wm.height <- as.integer( tclvalue( tkwinfo("height", janela.nova) ) )
	if (wm.width == -1) wm.width <- as.integer( tclvalue( tkwinfo("width", janela.nova) ) )

	mouse.x.pos <- as.integer( tclvalue( tkwinfo("pointerx", janela.nova) ) )
	mouse.y.pos <- as.integer( tclvalue( tkwinfo("pointery", janela.nova) ) )
	
	new.wm.x <- round( mouse.x.pos - wm.width/2 )
	new.wm.y <- round( mouse.y.pos - wm.height/2 )
	
	limite.sup.x <- round( MAX.width - wm.width/2 )
	limite.inf.x <- 0
	limite.sup.y <- round( MAX.height - wm.height/2 )
	limite.inf.y <- 0
	
	# Limitantes para o tamanho da tela. Quem tem tela virtural... #$%#$%
	if (new.wm.x > limite.sup.x) new.wm.x <- limite.sup.x
	if (new.wm.x < limite.inf.x) new.wm.x <- limite.inf.x
	if (new.wm.y > limite.sup.y) new.wm.y <- limite.sup.y
	if (new.wm.y < limite.inf.y) new.wm.y <- limite.sup.y
	
	posicao <- paste(wm.width, "x", wm.height, "+", new.wm.x,"+", new.wm.y, sep="")
	tkwm.geometry(janela.nova,posicao)
}

