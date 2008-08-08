`dimensoes.janela` <-
function(janela, height, width) {
	MAX.height <- as.integer( tclvalue( tkwinfo("screenheight", janela) ) )
	MAX.width <- as.integer( tclvalue( tkwinfo("screenwidth", janela) ) )

	wm.x <- as.integer( tclvalue( tkwinfo("x", janela) ) )
	wm.y <- as.integer( tclvalue( tkwinfo("y", janela) ) )
	
	if( height > MAX.height ) height <- MAX.height
	if( width > MAX.width ) width <- MAX.width
	
	limite.sup.x <- round( MAX.width - width )
	limite.inf.x <- round( width )
	limite.sup.y <- round( MAX.height - height )
	limite.sup.y <- round( height )

	# Limitantes para o tamanho da tela. Quem tem tela virtural... #$%#$%
	if (wm.x > limite.sup.x) wm.x <- limite.sup.x
	if (wm.x < limite.inf.x) wm.x <- limite.inf.x
	if (wm.y > limite.sup.y) wm.y <- limite.sup.y
	if (wm.y > limite.sup.y) wm.y <- limite.sup.y
	
	posicao <- paste(width, "x", height, "+", wm.x,"+", wm.y, sep="")
	tkwm.geometry(janela,posicao)
}

