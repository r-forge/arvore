\name{dialog.simulation.window}
\alias{dialog.simulation.window}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dialog.simulation.window(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(...) {
	.begin.sim <- TRUE	# Servirá como flag para se saber se se pode iniciar a simulação.
	
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		.begin.sim <- FALSE
		tkfocus(tt)
	} else {
		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		node.type <- TheTree$Type[position]
	}
	TestPartialTree <- select.subtree(TheTree, node.col = column, node.number = node.number, change.row.names = FALSE)$Type
	position.test <- which(	TestPartialTree == "M" )
	if (length(position.test) > 0) {
		if (dim(markov.propertiesMAT)[1] == 0) {
			msg <- paste("Propriedades dos nodos representantes dos estados Markov não \n",
						"foram definidos. Use o botão 'M' para ajustar as propriedades \n",
						"destes nodos.", sep = "")
			tkmessageBox(message = msg, icon="error", title = "ÁrvoRe - AVISO")
			.begin.sim <- FALSE
			tkfocus(tt)			
		}
	}
	if (.begin.sim) {
		if (node.type == "M") {
############ MARKOV ############
			dialogsimulationwindow <- tktoplevel()
			title <- "ÁrvoRe - Simulação Markov"
			tkwm.title(dialogsimulationwindow,title)
			
			Seedvar <- tclVar(0)
			Individuosvar <- tclVar(10000)
			Terminalvar <- tclVar("(.stage >= 10)")
			
			Seed.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Seedvar)
			tkgrid(tklabel(dialogsimulationwindow,text="Semente (zero indica semente não determinada)"),
					row = 0, column = 0, columnspan = 2, sticky = "n")
			tkgrid(Seed.Value, row = 1, column = 0, columnspan = 2, sticky = "n")
			
			Individuos.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Individuosvar)
			tkgrid(tklabel(dialogsimulationwindow,text="Número de indivíduos na coorte"),
					row = 2, column = 0, columnspan = 2, sticky = "n")
			tkgrid(Individuos.Value, row = 3, column = 0, columnspan = 2, sticky = "n")
			
			Terminal.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Terminalvar)
			tkgrid(tklabel(dialogsimulationwindow,text="Condição de término da simulação"),
					row = 4, column = 0, columnspan = 2, sticky = "n")
			tkgrid(Terminal.Value, row = 5, column = 0, columnspan = 2, sticky = "n")
			
			tkgrid(tklabel(dialogsimulationwindow,text=" "), columnspan = 2, sticky = "n")
			
			OnOK <- function()
			{
				tkconfigure(dialogsimulationwindow,cursor="watch")	# faz com que o cursor mude para busy
				
				SeedVal <- as.integer(tclvalue(Seedvar))
				IndividuosVal <- as.integer(tclvalue(Individuosvar))
				TerminalVal <- as.character(tclvalue(Terminalvar))
				
				if ( (is.numeric(SeedVal)) && (!is.na(SeedVal)) && (nchar(SeedVal) > 0) ) {
					if ( (is.numeric(IndividuosVal)) && (!is.na(IndividuosVal)) && (nchar(IndividuosVal) > 0) ) {
						PartialTree <- select.subtree(TheTree, node.col = column, node.number = node.number, change.row.names = FALSE)
						Partialmarkov.propertiesMAT <- select.markov.propertiesMAT(TheTree, PartialTree, markov.propertiesMAT)
						if (SeedVal == 0) SeedVal <- FALSE
						tempo1 <- Sys.time() 
						Mktable <- markov.coort.table(PartialTree, Partialmarkov.propertiesMAT, markov.termination = TerminalVal, 
														initial.coort = IndividuosVal, seed = SeedVal, absorventstatedeath = .absorventstateconf)
						tempo2 <- Sys.time()
# 						assign("Mktable", Mktable, .EnvironmentArvoRe)
						Mktable <- list(Mktable)
						names(Mktable) <- TheTree$Node.name[position]
						summary.simulation.window(Mktable, 
													tempo1 = tempo1, 
													tempo2 = tempo2, 
													CicloVal = dim(Mktable)[1], 
													tipo.nodo = "M",
													digits = .digits)
						tkdestroy(dialogsimulationwindow)
						tkfocus(tt)
					} else {
						msg <- paste("Este não é um valor válido para o número de de indivíduos na coorte '",IndividuosVal, "'")
						tkmessageBox(message=msg)
						tkconfigure(dialogsimulationwindow,cursor="arrow")
						tkfocus(dialogsimulationwindow)
					}
				} else {
					msg <- paste("Este não é um valor válido para o número de ciclos '",CicloVal, "'")
					tkmessageBox(message=msg)
					tkconfigure(dialogsimulationwindow,cursor="arrow")
					tkfocus(dialogsimulationwindow)
				}
			}
				
			OK.but <-tkbutton(dialogsimulationwindow,text="    OK    ",command=OnOK)
				
			tkbind(Seed.Value, "<Return>",OnOK)
			tkbind(Individuos.Value, "<Return>",OnOK)
			tkbind(Terminal.Value, "<Return>",OnOK)
			
			OnCancel <- function()
			{
				tkdestroy(dialogsimulationwindow)
				tkfocus(tt)
			}
				
			Cancel.but <-tkbutton(dialogsimulationwindow, text=" Cancelar ", command=OnCancel)
			tkbind(dialogsimulationwindow, "<Escape>",OnCancel)
		
			tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
			posiciona.janela.no.mouse(dialogsimulationwindow, 250, 200)
#	 		tcl("tkwait","window",dialogsimulationwindow)
			tkfocus(dialogsimulationwindow)
		} else {
			if (node.type == "D") {
############ DECISION ############
				dialogsimulationwindow <- tktoplevel()
				title <- "ÁrvoRe - Simulação Markov"
				tkwm.title(dialogsimulationwindow,title)
				
				Seedvar <- tclVar(0)
				Individuosvar <- tclVar(10000)
				Terminalvar <- tclVar("(.stage >= 10)")
				Trialssvar <- tclVar(10000)
				
				Seed.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Seedvar)
				tkgrid(tklabel(dialogsimulationwindow,text="Semente (zero indica semente não determinada)"),
						row = 0, column = 0, columnspan = 2, sticky = "n")
				tkgrid(Seed.Value, row = 1, column = 0, columnspan = 2, sticky = "n")
				
				Individuos.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Individuosvar)
				tkgrid(tklabel(dialogsimulationwindow,text="Número de indivíduos na coorte (Markov) \n Número de repetições (random walk) (Chance/Terminal)"),
						row = 2, column = 0, columnspan = 2, sticky = "n")
				tkgrid(Individuos.Value, row = 3, column = 0, columnspan = 2, sticky = "n")
				
				Terminal.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Terminalvar)
				tkgrid(tklabel(dialogsimulationwindow,text="Condição de término da simulação"),
						row = 4, column = 0, columnspan = 2, sticky = "n")
				tkgrid(Terminal.Value, row = 5, column = 0, columnspan = 2, sticky = "n")
				
# 				Trialss.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Trialssvar)
# 				tkgrid(tklabel(dialogsimulationwindow,text="Número de repetições (random walk)"),
# 						row = 6, column = 0, columnspan = 2, sticky = "n")
# 				tkgrid(Trialss.Value, row = 7, column = 0, columnspan = 2, sticky = "n")
					
				tkgrid(tklabel(dialogsimulationwindow,text=" "), columnspan = 2, sticky = "n")
				
				OnOK <- function()
				{
					tkconfigure(dialogsimulationwindow,cursor="watch")	# faz com que o cursor mude para busy
					
					SeedVal <- as.integer(tclvalue(Seedvar))
					IndividuosVal <- as.integer(tclvalue(Individuosvar))
					TerminalVal <- as.character(tclvalue(Terminalvar))
					TrialssVal <-IndividuosVal
# 					TrialssVal <- as.integer(tclvalue(Trialssvar))
					
					if ( (is.numeric(SeedVal)) && (!is.na(SeedVal)) && (nchar(SeedVal) > 0) ) {
						if ( (is.numeric(IndividuosVal)) && (!is.na(IndividuosVal)) && (nchar(IndividuosVal) > 0) ) {
							nodestoSim <- subset(TheTree, Level == column + 1)
							nodestoSim <- subset(nodestoSim, Father == node.number)
							
							Times.to.sim.init <- array(,0)
							Times.to.sim.final <- array(,0)
							
							Names.to.sim <- array(,0)
							Types.to.sim <- array(,0)
							
							Sim.list.to.resume <- list()
							
							for ( nodeinquestion in 1:length(nodestoSim$Node.N) ) {
								nodegotosim.Type <- nodestoSim$Type[nodeinquestion]
								nodegotosim.Name <- nodestoSim$Node.name[nodeinquestion]
								nodegotosim.Node.N <- nodestoSim$Node.N[nodeinquestion]
								nodegotosim.Level <- nodestoSim$Level[nodeinquestion]
								
								if ( nodegotosim.Type == "M") {
									PartialTree <- select.subtree(TheTree, 
																	node.col = nodegotosim.Level, 
																	node.number = nodegotosim.Node.N, 
																	change.row.names = FALSE)
									Partialmarkov.propertiesMAT <- select.markov.propertiesMAT(TheTree, 
																	PartialTree, 
																	markov.propertiesMAT)
									if (SeedVal == 0) SeedVal <- FALSE
									tempo1 <- Sys.time()
									Times.to.sim.init <- c(Times.to.sim.init, Sys.time())
									Sim.list.to.resume[[nodeinquestion]] <- markov.coort.table(PartialTree, 
																	markov.propertiesMAT = Partialmarkov.propertiesMAT, 
																	markov.termination = TerminalVal, 
																	initial.coort = IndividuosVal, 
																	seed = SeedVal, 
																	absorventstatedeath = .absorventstateconf)
									tempo2 <- Sys.time()
									Times.to.sim.final <- c(Times.to.sim.final, Sys.time())
									Names.to.sim <- c(Names.to.sim, nodegotosim.Name)
									Types.to.sim <- c(Types.to.sim, "M")
								}
								if ( nodegotosim.Type == "C") {
									PartialTree <- select.subtree(TheTree, 
																	node.col = nodegotosim.Level, 
																	node.number = nodegotosim.Node.N, 
																	change.row.names = FALSE)
									if (SeedVal == 0) SeedVal <- FALSE
									tempo1 <- Sys.time()
									Times.to.sim.init <- c(Times.to.sim.init, Sys.time())
									Sim.list.to.resume[[nodeinquestion]] <- simple.markov.coort.table(PartialTree, 
																			trials = TrialssVal, 
																			seed = SeedVal)
									tempo2 <- Sys.time()
									Times.to.sim.final <- c(Times.to.sim.final, Sys.time())
									Names.to.sim <- c(Names.to.sim, nodegotosim.Name)
									Types.to.sim <- c(Types.to.sim, "C")
								}
								if ( nodegotosim.Type == "T") {
									PartialTree <- select.subtree(TheTree, 
																	node.col = nodegotosim.Level, 
																	node.number = nodegotosim.Node.N, 
																	change.row.names = FALSE)
									Times.to.sim.init <- c(Times.to.sim.init, Sys.time())
									Sim.list.to.resume[[nodeinquestion]] <- terminal.markov.coort.table(PartialTree, trials = TrialssVal)
									Times.to.sim.final <- c(Times.to.sim.final, Sys.time())
									Names.to.sim <- c(Names.to.sim, nodegotosim.Name)
									Types.to.sim <- c(Types.to.sim, "T")
# 									cat("NODO Terminal : fazendo nada | dialog.simulation() \n")
								}
							}
							names(Sim.list.to.resume) <- Names.to.sim
							summary.simulation.window(Sim.list.to.resume, 
														tempo1 = Times.to.sim.init, 
														tempo2 = Times.to.sim.final, 
														CicloVal = 999, 
														tipo.nodo = Types.to.sim,
														digits = .digits)
							
							tkdestroy(dialogsimulationwindow)
							tkfocus(tt)
						} else {
							msg <- paste("Este não é um valor válido para o número de de indivíduos na coorte '",IndividuosVal, "'")
							tkmessageBox(message=msg)
							tkconfigure(dialogsimulationwindow,cursor="arrow")
							tkfocus(dialogsimulationwindow)
						}
					} else {
						msg <- paste("Este não é um valor válido para o número de ciclos '",CicloVal, "'")
						tkmessageBox(message=msg)
						tkconfigure(dialogsimulationwindow,cursor="arrow")
						tkfocus(dialogsimulationwindow)
					}
				}
					
				OK.but <-tkbutton(dialogsimulationwindow,text="    OK    ",command=OnOK)
					
				tkbind(Seed.Value, "<Return>",OnOK)
				tkbind(Individuos.Value, "<Return>",OnOK)
				tkbind(Terminal.Value, "<Return>",OnOK)
				
				OnCancel <- function()
				{
					tkdestroy(dialogsimulationwindow)
					tkfocus(tt)
				}
					
				Cancel.but <-tkbutton(dialogsimulationwindow, text=" Cancelar ", command=OnCancel)
				tkbind(dialogsimulationwindow, "<Escape>",OnCancel)
			
				tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
				posiciona.janela.no.mouse(dialogsimulationwindow, 300, 200)
	#	 		tcl("tkwait","window",dialogsimulationwindow)
				tkfocus(dialogsimulationwindow)
			} else {
				if (node.type == "C") {
############ CHANCE ############
					dialogsimulationwindow <- tktoplevel()
					title <- "ÁrvoRe - Simulação Markov"
					tkwm.title(dialogsimulationwindow,title)
					
					Seedvar <- tclVar(0)
					Trialssvar <- tclVar(10000)
# 					Terminalvar <- tclVar("(.stage >= 10)")
					
					Seed.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Seedvar)
					tkgrid(tklabel(dialogsimulationwindow,text="Semente (zero indica semente não determinada)"),
							row = 0, column = 0, columnspan = 2, sticky = "n")
					tkgrid(Seed.Value, row = 1, column = 0, columnspan = 2, sticky = "n")
					
					Trialss.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Trialssvar)
					tkgrid(tklabel(dialogsimulationwindow,text="Número de repetições (random walk)"),
							row = 2, column = 0, columnspan = 2, sticky = "n")
					tkgrid(Trialss.Value, row = 3, column = 0, columnspan = 2, sticky = "n")
					
# 					Terminal.Value  <- tkentry(dialogsimulationwindow,width="20",textvariable=Terminalvar)
# 					tkgrid(tklabel(dialogsimulationwindow,text="Número de indivíduos na coorte"), sticky = "n")
# 					tkgrid(Terminal.Value, sticky = "n")

					tkgrid(tklabel(dialogsimulationwindow,text=" "), columnspan = 2, sticky = "n")
					
					OnOK <- function()
					{
						tkconfigure(dialogsimulationwindow,cursor="watch")	# faz com que o cursor mude para busy
						
						SeedVal <- as.integer(tclvalue(Seedvar))
						TrialssVal <- as.integer(tclvalue(Trialssvar))
# 						TerminalVal <- as.character(tclvalue(Terminalvar))
						
						if ( (is.numeric(SeedVal)) && (!is.na(SeedVal)) && (nchar(SeedVal) > 0) ) {
							if ( (is.numeric(TrialssVal)) && (!is.na(TrialssVal)) && (nchar(TrialssVal) > 0) ) {
								PartialTree <- select.subtree(TheTree, node.col = column, node.number = node.number, change.row.names = FALSE)
								if (SeedVal == 0) SeedVal <- FALSE
								tempo1 <- Sys.time() 
								Mktable <- simple.markov.coort.table(PartialTree, trials = TrialssVal, seed = SeedVal)
								tempo2 <- Sys.time()
# 								assign("Mktable", Mktable, .EnvironmentArvoRe)
								Mktable <- list(Mktable)
								names(Mktable) <- TheTree$Node.name[position]
								summary.simulation.window(Mktable, 
															tempo1 = tempo1, 
															tempo2 = tempo2, 
															CicloVal = dim(Mktable)[1], 
															tipo.nodo = "C",
															digits = .digits)
								tkdestroy(dialogsimulationwindow)
								tkfocus(tt)
							} else {
								msg <- paste("Este não é um valor válido para o número de de indivíduos na coorte '",TrialssVal, "'")
								tkmessageBox(message=msg)
								tkconfigure(dialogsimulationwindow,cursor="arrow")
								tkfocus(dialogsimulationwindow)
							}
						} else {
							msg <- paste("Este não é um valor válido para o número de ciclos '",CicloVal, "'")
							tkmessageBox(message=msg)
							tkconfigure(dialogsimulationwindow,cursor="arrow")
							tkfocus(dialogsimulationwindow)
						}
					}
						
					OK.but <-tkbutton(dialogsimulationwindow,text="    OK    ",command=OnOK)
						
					tkbind(Seed.Value, "<Return>",OnOK)
					tkbind(Trialss.Value, "<Return>",OnOK)
# 					tkbind(Terminal.Value, "<Return>",OnOK)
					
					OnCancel <- function()
					{
						tkdestroy(dialogsimulationwindow)
						tkfocus(tt)
					}
						
					Cancel.but <-tkbutton(dialogsimulationwindow, text=" Cancelar ", command=OnCancel)
					tkbind(dialogsimulationwindow, "<Escape>",OnCancel)
				
					tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
					posiciona.janela.no.mouse(dialogsimulationwindow, 250, 150)
		#	 		tcl("tkwait","window",dialogsimulationwindow)
					tkfocus(dialogsimulationwindow)
				} else {
					if (node.type == "T") {
############ TERMINAL ############
						msg <- paste("O nodo selecionado é do tipo 'Terminal'. Selecione um outro \n nodo da árvore para executar simulação.")
						tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
						tkfocus(tt)
# 						
# 						PartialTree <- select.subtree(TheTree, 
# 														node.col = column, node.number = node.number, 
# 														change.row.names = FALSE)
# 						tempo1 <- Sys.time()
# 						Mktable <- terminal.markov.coort.table(PartialTree)
# 						print(Mktable)
# 						tempo2 <- Sys.time()
# 						summary.simulation.window(Mktable, 
# 													tempo1 = tempo1, 
# 													tempo2 = tempo2, 
# 													CicloVal = dim(Mktable)[1], 
# 													tipo.nodo = "M",
# 													digits = .digits)
					} else {
						cat("ERROR: Tipo não reconhecido \n")
						msg <- paste("O nodo selecionado é de tipo não reconhecido. Selecione um outro \n nodo da árvore para executar simulação.")
						tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
						tkfocus(tt)
					}
				}
			}
		}
		
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
