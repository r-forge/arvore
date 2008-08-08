`show.summary.rollback.window` <-
function(...) {
	k <- summary.rollback.table(TheTree)
	
	names(k) <- c("Nível", "Nodo N", "Nome do nodo", 
					"Custo Esperado", "Efetividade Esperada", "Razão C-E Esperada", 
					"Nome Nodo Pai", "Probabilidade",
					"Custo", "Efetividade", "Tipo")
					
	displayInTable(as.matrix(k), title="Valores Esperados (Roll-back)",
					height=10,width=8,nrow=dim(k)[1],ncol=dim(k)[2], 
					titlerows = FALSE, titlecols = TRUE)
}

