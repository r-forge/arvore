`posiciona.janela.centro` <-
function(janela.principal, janela.nova) {
	MAX.height <- as.integer( tclvalue( tkwinfo("screenheight", janela.principal) ) )
	MAX.width <- as.integer( tclvalue( tkwinfo("screenwidth", janela.principal) ) )

	wm.main.height <- as.integer( tclvalue( tkwinfo("height", janela.principal) ) )
	wm.main.width <- as.integer( tclvalue( tkwinfo("width", janela.principal) ) )

	wm.child.height <- as.integer( tclvalue( tkwinfo("height", janela.nova) ) )
	wm.child.width <- as.integer( tclvalue( tkwinfo("width", janela.nova) ) )

	wm.x <- as.integer( tclvalue( tkwinfo("x", janela.principal) ) )
	wm.y <- as.integer( tclvalue( tkwinfo("y", janela.principal) ) )
	
	new.wm.x <- wm.x + wm.main.width/2 - wm.child.width/2
	new.wm.y <- wm.y + wm.main.height/2 - wm.child.height/2
	
	new.wm.x <- round(new.wm.x)
	new.wm.y <- round(new.wm.y)
	
	limite.sup.x <- round( MAX.width - wm.child.width )
	limite.inf.x <- round( wm.child.width )
	limite.sup.y <- round( MAX.height - wm.child.height )
	limite.sup.y <- round( wm.child.height )

	# Limitantes para o tamanho da tela. Quem tem tela virtural... #$%#$%
	if (new.wm.x > limite.sup.x) new.wm.x <- limite.sup.x
	if (new.wm.x < limite.inf.x) new.wm.x <- limite.inf.x
	if (new.wm.y > limite.sup.y) new.wm.y <- limite.sup.y
	if (new.wm.y > limite.sup.y) new.wm.y <- limite.sup.y
	
	posicao <- paste(wm.child.width, "x", wm.child.height, "+", new.wm.x,"+", new.wm.y, sep="")
	tkwm.geometry(janela.nova,posicao)
}

