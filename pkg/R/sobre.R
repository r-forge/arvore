`sobre` <-
function(versionarvore, versiondate) {
	.Mensagem <- paste(
" ________________________________________ \n\n",
" ArvoRe - Análise de Custo Efetividade no R              \n",
" (Simulação de primeira ordem MCMC)                      \n\n",
paste("Versão : ", versionarvore, "                                     \n", sep=""),
paste("Versão : ", versiondate, "                                     \n", sep=""),
" ________________________________________ \n\n",
" Autor:                                                  \n",
" Isaías V. Prestes                                       \n",
" IM - Departamento de Estatística                        \n",
" Universidade Federal do Rio Grande do Sul,              \n",
" Av. Bento Gonçalves, 9500, Porto Alegre, Brasil         \n",
" E-mail: isaias.prestes@ufrgs.br                         \n",
" URL: http://www.mat.ufrgs.br/~camey/                    \n",
" ________________________________________ \n",
" \n", sep = "")
	sobre.wm.title <- "Sobre o Programa"
	ReturnVal <- tkmessageBox(title = sobre.wm.title,
    message = .Mensagem, icon = "info", type = "ok")
    tkfocus(tt)
}

