`sobre` <-
function(versionarvore, versiondate) {
	.Mensagem <- paste(
" ________________________________________ \n\n",
" ArvoRe - An�lise de Custo Efetividade no R              \n",
" (Simula��o de primeira ordem MCMC)                      \n\n",
paste("Vers�o : ", versionarvore, "                                     \n", sep=""),
paste("Vers�o : ", versiondate, "                                     \n", sep=""),
" ________________________________________ \n\n",
" Autor:                                                  \n",
" Isa�as V. Prestes                                       \n",
" IM - Departamento de Estat�stica                        \n",
" Universidade Federal do Rio Grande do Sul,              \n",
" Av. Bento Gon�alves, 9500, Porto Alegre, Brasil         \n",
" E-mail: isaias.prestes@ufrgs.br                         \n",
" URL: http://www.mat.ufrgs.br/~camey/                    \n",
" ________________________________________ \n",
" \n", sep = "")
	sobre.wm.title <- "Sobre o Programa"
	ReturnVal <- tkmessageBox(title = sobre.wm.title,
    message = .Mensagem, icon = "info", type = "ok")
    tkfocus(tt)
}

