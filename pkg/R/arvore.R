`arvore` <-
function(...) {
	# Se .ArvoReRunning existe, então o ÁrvoRe já está em execução...
	if (!exists(".ArvoReRunning", envir = globalenv() )) {
		# ArvoRe Settings
		library(tcltk)
		###############################################################################
		# THE GAME!!
		###############################################################################
		# Configuration variables
		.EnvironmentArvoRe <- globalenv()
		.EnvironmentArvore.Secure <- new.env(parent = globalenv())
		.arvore.version <- "Alfa-0.1.4"								# The ArvoRe version
		.arvore.release.date <- "June 18, 2008 06:43:29 PM "		# The ArvoRe version date
		.modeltypeArvore <- "CE" 									# Default calculation method "Simple" # "CEA"
		.workstatus <- "saved"										# File status
		.opennedfile <- "newfile"									# File name
		.digits <- 3												# Decimal places
		.init.objects <- objects(all.names = TRUE, 
									envir = .EnvironmentArvoRe)		# Objetos existentes antes de abrir o ArvoRe
		.treeangle <- "squared"										# Tipo de ângulos exibidos para a árvore
		.notesconf <- 0												# Mostrar notas no gráfico { 1 = yes, 0 = no }
		.probabilityconf <- 1										# Mostrar probabilidades no gráfico { 1 = yes, 0 = no }
		.payoffsconf <- 1											# Mostrar payoffs no gráfico { 1 = yes, 0 = no }
		.node.name.font.size <- 12									# Tamanho da fonte para o nome do nodo exibido no gráfico.
		.payoffs.font.size	<- 8									# Tamanho da fonte para payoffs do nodo exibido no gráfico.
		.notes.font.size <- 6										# Tamanho da fonte para comentários do nodo exibido no gráfico.
		.absorventstateconf <- 1									# Estados absorventes da cadeia de Markov são interpretados como MORTE.
		
		assign(".EnvironmentArvoRe", .EnvironmentArvoRe, env = .GlobalEnv)
		assign(".EnvironmentArvore.Secure", .EnvironmentArvore.Secure, env = .GlobalEnv)
		assign(".arvore.version", .arvore.version, env = .GlobalEnv)
		assign(".modeltypeArvore", .modeltypeArvore, env = .GlobalEnv)
		assign(".workstatus", .workstatus, env = .GlobalEnv)
		assign(".opennedfile", .opennedfile, env = .GlobalEnv)
		assign(".digits", .digits, env = .GlobalEnv)
		assign(".init.objects", .init.objects, env = .GlobalEnv)
		assign(".treeangle", .treeangle, env = .GlobalEnv)
		assign(".notesconf", .notesconf, env = .GlobalEnv)
		assign(".probabilityconf", .probabilityconf, env = .GlobalEnv)
		assign(".payoffsconf", .payoffsconf, env = .GlobalEnv)
		assign(".node.name.font.size", .node.name.font.size, env = .GlobalEnv)
		assign(".node.name.font.size", .node.name.font.size, env = .GlobalEnv)
		assign(".payoffs.font.size", .payoffs.font.size, env = .GlobalEnv)
		assign(".notes.font.size", .notes.font.size, env = .GlobalEnv)
		assign(".absorventstateconf", .absorventstateconf, env = .GlobalEnv)
# 		assign("", x, env = .GlobalEnv)

		###############################################################################
		# The Tk things
		###############################################################################
		carregaTclpath()	# Carrega extensões da Tcltk
		tclRequire("Img")
		tclRequire("BWidget")
		#---------------------------------------------------------------------- 
		# tclRequire("Tk") # Used in TckTk 8.5
		
		# Create a new decision tree
		new.tree()
		
		# Set Running flag to TRUE
		.ArvoReRunning <- TRUE
		
		# The splashscreen
		splashscreenArvoRe()
		
		# The main window
		tt <- tktoplevel()
		
		# Send tt addres to .EnvironmentArvoRe
		assign("tt", tt, .EnvironmentArvoRe)
		
		.Windowtitle <- paste("ÁrvoRe - Janela Principal", " - [", .opennedfile, "]", sep = "")
		.Frametitle1 <- " Representação Gráfica da Árvore "
		.Frametitle2 <- paste("ÁrvoRe - versão ", .arvore.version, " | ", 
								" | ", "Rodando no R ", getRversion(), " ",
								sep="")
		.Frametitle3 <- " Configuração de Nodo "
		
		tkwm.title(tt, .Windowtitle)
		
		# Set max and min size to main ArvoRe window
		tkwm.minsize(tt,640,480)
		tkwm.maxsize(tt,1024,768)
		
		# The Frames
		frameOverall <- tkframe(tt)
		frameBottons <- tkframe(frameOverall,relief="groove",borderwidth=2)
		frameUpper <- tkframe(frameOverall,relief="groove",borderwidth=2)
		
			frameUpperLeft <- tkframe(frameUpper,relief="groove",borderwidth=2)
			
				frameUpperLeftUp <- tkframe(frameUpperLeft,relief="groove",borderwidth=2)
				frameUpperLeftDown <- tkframe(frameUpperLeft,relief="groove",borderwidth=2)
				
			frameUpperRigth <- tkframe(frameUpper,relief="groove",borderwidth=2)
			tkpack(tklabel(frameUpperRigth,text = .Frametitle1))
		
		frameLower <- tkframe(frameOverall,relief="sunken",borderwidth=2)
		tkpack(tklabel(frameLower,text = .Frametitle2, justify = "left"), fill = "x", expand = 0, side = "left")
		
		tkpack(frameBottons, anchor = "nw", expand = 0, side = "top")#, fill = "x")
		tkpack(frameUpperLeft, frameUpperRigth, side = "left", expand = 1, fill = "both")
		tkpack(frameUpper, anchor = "n", side = "top", expand = 1, fill = "both")
		tkpack(tklabel(frameOverall,text=" "))
		tkpack(frameLower, anchor = "sw", fill = "x", expand = 0, side = "bottom")
		tkpack(tklabel(frameOverall,text=" "))
		tkpack(frameOverall, anchor = "center", expand = 1, fill = "both")
		
		# The Menu
		topMenu <- tkmenu(tt)
		tkconfigure(tt,menu=topMenu)
		fileMenu <- tkmenu(topMenu,tearoff=FALSE)
			tkadd(fileMenu,"command",label="Novo           Ctrl+N",command=function() new.file.bot())
			tkadd(fileMenu,"command",label="Abrir...       Ctrl+O",command=function() load.file.arv())
			tkadd(fileMenu,"command",label="Salvar         Ctrl+S",command=function() save.file.arv())
			tkadd(fileMenu,"command",label="Salvar como... Ctrl+Alt+S",command=function() save.as.file.arv())
			tkadd(fileMenu,"separator")
			tkadd(fileMenu,"command",label="Exportar...    Ctrl+E",command=function() export.tree.graph())
			tkadd(fileMenu,"separator")
			tkadd(fileMenu,"command",label="Sair           Esc",command=function() sair())
		tkadd(topMenu,"cascade",label="Arquivo",menu=fileMenu)
		
		editMenu <- tkmenu(topMenu,tearoff=FALSE)
			tkadd(editMenu,"command",label="Desfazer",command=function() changedofunction(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure))
			tkadd(editMenu,"command",label="Refazer",command=function() changedofunction(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure))
			tkadd(editMenu,"separator")
			tkadd(editMenu,"command",label="Recortar",command=function() naoimplementado())
			tkadd(editMenu,"command",label="Copiar",command=function() naoimplementado())
			tkadd(editMenu,"command",label="Colar",command=function() naoimplementado())
			tkadd(editMenu,"separator")
			tkadd(editMenu,"command",label="Excluir",command=function() naoimplementado())
			tkadd(editMenu,"separator")
			tkadd(editMenu,"command",label="Recortar sub-árvore",command=function() naoimplementado())
			tkadd(editMenu,"command",label="Copiar sub-árvore",command=function() naoimplementado())
			tkadd(editMenu,"command",label="Colar sub-árvore",command=function() naoimplementado())
			tkadd(editMenu,"separator")
			tkadd(editMenu,"command",label="Excluir sub-árvore",command=function() naoimplementado())
			tkadd(editMenu,"separator")
			tkadd(editMenu,"command",label="Variáveis...",command=function() dialog.variable.window())
			tkadd(editMenu,"separator")
			tkadd(editMenu,"command",label="Configurações",command=function() properties.tree())
		tkadd(topMenu,"cascade",label="Editar",menu=editMenu)
		
		modelMenu <- tkmenu(topMenu,tearoff=FALSE)
			tkadd(modelMenu,"command",label="Árvore de decisão simples",command=function() set.model.type("SD") )
			tkadd(modelMenu,"separator")
			tkadd(modelMenu,"command",label="Árvore de decisão Custo-Efetividade",command=function() set.model.type("CE") )
		
		tkadd(topMenu,"cascade",label="Modelo",menu=modelMenu)
		
		analysisMenu <- tkmenu(topMenu,tearoff=FALSE)
			tkadd(analysisMenu,"command",label="Valores esperados (Roll Back)",command=function() show.summary.rollback.window())
			tkadd(analysisMenu,"separator")
			tkadd(analysisMenu,"command",label="Resumo da ACE (ICER)...",command=function() acewindow(TheTree))
			tkadd(analysisMenu,"command",label="Plano Custo-Efetividade",command=function() planoacewindow(TheTree))
			tkadd(analysisMenu,"command",label="Rede de Benefício (INB)",command=function() inbwindow(TheTree))
			tkadd(analysisMenu,"separator")
			tkadd(analysisMenu,"command",label="Resumo da árvore",command=function() show.summary.tree.window())
			tkadd(analysisMenu,"separator")
			tkadd(analysisMenu,"command",label="Verificar probabilidades",command=function() show.prob.check.window(TheTree))
		tkadd(topMenu,"cascade",label="Análise",menu=analysisMenu)
		
		windowMenu <- tkmenu(topMenu,tearoff=FALSE)
			tkadd(windowMenu,"command",label="Zoom +...",command=function() zoom.in.but(imgHeight))
			tkadd(windowMenu,"command",label="Zoom -...",command=function() zoom.out.but(imgHeight))
			tkadd(windowMenu,"separator")
			tkadd(windowMenu,"command",label="Resolução da janela...",command=function() naoimplementado())
		tkadd(topMenu,"cascade",label="Janela",menu=windowMenu)
		
		helpMenu <- tkmenu(topMenu,tearoff=FALSE)
			tkadd(helpMenu,"command",label="Ajuda",command=function() help.start())
			tkadd(helpMenu,"separator")
			tkadd(helpMenu,"command",label="Sobre o programa",command=function() sobre(.arvore.version, .arvore.release.date))
		tkadd(topMenu,"cascade",label="Ajuda",menu=helpMenu)
		
		# The top bottons
		.Height.but <- 3
		.Width.but <- 7
		.Height.img.but <- 32
		.Width.img.but <- 32
		
		#	New button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/New.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				new.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, 
									command=function() new.file.bot())
				tcl("DynamicHelp::add", new.but, "-type", "balloon", "-text", "Novo trabalho")
		
			} else {
				new.but <- tkbutton(frameBottons, text="Novo", width=.Width.but, height=.Height.but, command=function() new.file.bot())
				tcl("DynamicHelp::add", new.but, "-type", "balloon", "-text", "Novo trabalho")
			}
		}
		
		#	Open button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Open.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				open.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() load.file.arv())
				tcl("DynamicHelp::add", open.but, "-type", "balloon", "-text", "Abrir trabalho")
			} else {
				open.but <- tkbutton(frameBottons, text="Abrir", width=.Width.img.but, height=.Height.img.but, command=function() load.file.arv())
				tcl("DynamicHelp::add", open.but, "-type", "balloon", "-text", "Abrir trabalho")
			}
		}
		
		#	Save button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Save.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				save.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() save.file.arv())
				tcl("DynamicHelp::add", save.but, "-type", "balloon", "-text", "Salvar o trabalho atual")
			} else {
				save.but <- tkbutton(frameBottons, text="Salvar", width=.Width.img.but, height=.Height.img.but, command=function() save.file.arv())
				tcl("DynamicHelp::add", save.but, "-type", "balloon", "-text", "Salvar o trabalho atual")
		
			}
		}
		
		#	Save As button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/SaveAs.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				saveas.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() save.as.file.arv())
				tcl("DynamicHelp::add", saveas.but, "-type", "balloon", "-text", "Salvar como...")
			} else {
				saveas.but <- tkbutton(frameBottons, text="Salvar \n como...", width=.Width.img.but, height=.Height.img.but, command=function() save.as.file.arv())
				tcl("DynamicHelp::add", saveas.but, "-type", "balloon", "-text", "Salvar como...")
			}
		}
		
		#	Undo button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Undo.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				undo.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() changedofunction(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure))
				tcl("DynamicHelp::add", undo.but, "-type", "balloon", "-text", "Desfazer")
			} else {
				undo.but <- tkbutton(frameBottons, text="<=", width=.Width.img.but, height=.Height.img.but, command=function() changedofunction(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure))
				tcl("DynamicHelp::add", undo.but, "-type", "balloon", "-text", "Desfazer")
			}
		}
		
		
		#	Redo button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Redo.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				redo.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() changedofunction(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure))
				tcl("DynamicHelp::add", redo.but, "-type", "balloon", "-text", "Refazer")
			} else {
				redo.but <- tkbutton(frameBottons, text="=>", width=.Width.img.but, height=.Height.img.but, command=function() changedofunction(TheTree, .modeltypeArvore, .EnvironmentArvore.Secure))
				tcl("DynamicHelp::add", redo.but, "-type", "balloon", "-text", "Refazer")
			}
		}
	
		#	Markov properties button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Markov.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				markov.prop.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() markov.nodes.properties(TheTree, .EnvironmentArvoRe))
				tcl("DynamicHelp::add", markov.prop.but, "-type", "balloon", "-text", "Propriedades do estado Markov...")
			} else {
				markov.prop.but <- tkbutton(frameBottons, text="Markov \n Sim.", width=.Width.img.but, height=.Height.img.but, command=function() markov.nodes.properties(TheTree, .EnvironmentArvoRe))
				tcl("DynamicHelp::add", markov.prop.but, "-type", "balloon", "-text", "Propriedades do estado Markov...")
			}
		}
		
		#	Variable button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Variable.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				variable.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() dialog.variable.window())
				tcl("DynamicHelp::add", variable.but, "-type", "balloon", "-text", "Variáveis...")
			} else {
				variable.but <- tkbutton(frameBottons, text="Markov \n Sim.", width=.Width.img.but, height=.Height.img.but, command=function() dialog.variable.window())
				tcl("DynamicHelp::add", variable.but, "-type", "balloon", "-text", "Variáveis...")
			}
		}
		
		#	Simulation button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Simulation.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				simulation.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() dialog.simulation.window())
				tcl("DynamicHelp::add", simulation.but, "-type", "balloon", "-text", "Simular... (MCMC)")
			} else {
				simulation.but <- tkbutton(frameBottons, text="Markov \n Sim.", width=.Width.img.but, height=.Height.img.but, command=function() dialog.simulation.window())
				tcl("DynamicHelp::add", simulation.but, "-type", "balloon", "-text", "Simular... (MCMC)")
			}
		}
		
		#	Roll-Back button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Ball.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				rollback.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() show.summary.rollback.window())
				tcl("DynamicHelp::add", rollback.but, "-type", "balloon", "-text", "Roll-back")
			} else {
				rollback.but <- tkbutton(frameBottons, text="Roll-Back", width=.Width.img.but, height=.Height.img.but, command=function() show.summary.rollback.window())
				tcl("DynamicHelp::add", rollback.but, "-type", "balloon", "-text", "Roll-back")
			}
		}
		
		#	Sensitivity Analysis button 1-way
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Graph.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				sa.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() sa.1way.window())
				tcl("DynamicHelp::add", sa.but, "-type", "balloon", "-text", "Análise de Sensibilidade 1-way")
			} else {
				sa.but <- tkbutton(frameBottons, text="Análise de Sensibilidade 1-way", width=.Width.img.but, height=.Height.img.but, command=function() sa.1way.window())
				tcl("DynamicHelp::add", sa.but, "-type", "balloon", "-text", "Análise de Sensibilidade 1-way")
			}
		}
		
		#	Sensitivity Analysis button 2-way
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Graph2.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				sa2.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() sa.2way.window())
				tcl("DynamicHelp::add", sa2.but, "-type", "balloon", "-text", "Análise de Sensibilidade 2-way")
			} else {
				sa2.but <- tkbutton(frameBottons, text="Análise de Sensibilidade 2-way", width=.Width.img.but, height=.Height.img.but, command=function() sa.2way.window())
				tcl("DynamicHelp::add", sa2.but, "-type", "balloon", "-text", "Análise de Sensibilidade 2-way")
			}
		}
		
		#	Zoom In button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/ZoomPlus.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				zoom.in <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() zoom.in.but(imgHeight))
				tcl("DynamicHelp::add", zoom.in, "-type", "balloon", "-text", "Aumentar zoom")
			} else {
				zoom.in <- tkbutton(frameBottons, text="Zoom \n +", width=.Width.img.but, height=.Height.img.but, command=function() zoom.in.but(imgHeight))
				tcl("DynamicHelp::add", zoom.in, "-type", "balloon", "-text", "Aumentar zoom")
			}
		}
		
		
		#	Zoom Out button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/ZoomMinus.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				zoom.out <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() zoom.out.but(imgHeight))
				tcl("DynamicHelp::add", zoom.out, "-type", "balloon", "-text", "Diminuir zoom")
			} else {
				zoom.out <- tkbutton(frameBottons, text="Zoom \n -", width=.Width.img.but, height=.Height.img.but, command=function() zoom.out.but(imgHeight))
				tcl("DynamicHelp::add", zoom.out, "-type", "balloon", "-text", "Diminuir zoom")
			}
		}
		
		
		#	Exit button
		for (i in 1:length(.libPaths())) {
			icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/Exit.png",sep=""))
			if (file.exists(icon.but)) {
				icn <- tkimage.create("photo", file=icon.but)
				exit.but <- tkbutton(frameBottons, image=icn, width=.Width.img.but, height=.Height.img.but, command=function() sair())
				tcl("DynamicHelp::add", exit.but, "-type", "balloon", "-text", "Sair do programa")
			} else {
				exit.but <- tkbutton(frameBottons, text="Sair", width=.Width.img.but, height=.Height.img.but, command=function() sair())
				tcl("DynamicHelp::add", exit.but, "-type", "balloon", "-text", "\"Sair do programa \"")
			}
		}
		
		separator1 <- tklabel(frameBottons, text = " ")
		separator2 <- tklabel(frameBottons, text = " ")
		separator3 <- tklabel(frameBottons, text = " ")
		separator4 <- tklabel(frameBottons, text = " ")
		separator5 <- tklabel(frameBottons, text = " ")
		separator6 <- tklabel(frameBottons, text = " ")
		
		tkgrid(new.but, open.but, save.but, saveas.but, separator1,
				undo.but, redo.but, separator2,
				markov.prop.but, variable.but, separator3,
				simulation.but, rollback.but, separator4,
				sa.but, sa2.but, separator5,
				zoom.in, zoom.out, separator6,
				exit.but, 
				sticky = "nw")
		
		tkconfigure(new.but, activebackground = "white")
		tkflash(new.but)
		
		# The tree structure view
		
		xScr       <- tkscrollbar(frameUpperLeftUp,command=function(...)tkxview(treeWidget,...),orient="horizontal")
		yScr       <- tkscrollbar(frameUpperLeftUp,command=function(...)tkyview(treeWidget,...))
		treeWidget <- tkwidget(frameUpperLeftUp,"Tree", deltax = 25, deltay = 20,
										xscrollcommand=function(...)tkset(xScr,...), 
										yscrollcommand=function(...)tkset(yScr,...),
										width=30,height=15)
		tkgrid(treeWidget, yScr)
		tkgrid.configure(treeWidget,stick="nswe")
		tkgrid.configure(yScr,stick="nsw")
		tkgrid(xScr)
		tkgrid.configure(xScr,stick="nswe")
		
		tkgrid(frameUpperLeftUp, sticky = "nwe")
		
		# Send treeWidget addres to .EnvironmentArvoRe
		assign("treeWidget", treeWidget, .EnvironmentArvoRe)

		theTreeTkArvore(TheTree)
		
		# The Tree Bottons
		.Height.but <- 2
		.Width.but <- 16
		
		node.name.but <- tkbutton(frameUpperLeftDown, text="Nome", width=.Width.but, height=.Height.but, command=function() nodenamewindows())
		node.prob.but <- tkbutton(frameUpperLeftDown, text="Probabilidade", width=.Width.but, height=.Height.but, command=function() probwindows())
		node.playoff.but <- tkbutton(frameUpperLeftDown, text="Valores", width=.Width.but, height=.Height.but, command=function() utilitywindows())
		node.type <- tkbutton(frameUpperLeftDown, text="Tipo", width=.Width.but, height=.Height.but, command=function() typenodewindows())
		node.add <- tkbutton(frameUpperLeftDown, text="Adicionar", width=.Width.but, height=.Height.but, command=function() addnodewindows())
		node.remove <- tkbutton(frameUpperLeftDown, text="Remover", width=.Width.but, height=.Height.but, command=function() removenodewindows())
		node.destiny <- tkbutton(frameUpperLeftDown, text="Destino", width=.Width.but, height=.Height.but, command=function() destinynodewindows())
		node.notes <- tkbutton(frameUpperLeftDown, text="Comentários", width=.Width.but, height=.Height.but, command=function() notesnodewindows())
		
		tkgrid(tklabel(frameUpperLeft,text = .Frametitle3))
		tkgrid(node.name.but, row = 0, column = 0, sticky = "nw")
		tkgrid(node.prob.but, row = 0, column = 1, sticky = "nw")
		tkgrid(node.type, row = 1, column = 0, sticky = "nw")
		tkgrid(node.playoff.but, row = 1, column = 1, sticky = "nw")
		tkgrid(node.add, row = 2, column = 0, sticky = "nw")
		tkgrid(node.remove, row = 2, column = 1, sticky = "nw")
		tkgrid(node.destiny, row = 3, column = 0, sticky = "nw")
		tkgrid(node.notes, row = 3, column = 1, sticky = "nw")
				
		tkgrid(frameUpperLeftDown, sticky = "swe") #, side = "bottom", expand = 1, fill = "both")
		
		# Image window configurations
		Height <- 400
		Width <- 600
		Borderwidth <- 2
		
		# scrollbar objects
		Hscroll <- tkscrollbar(frameUpperRigth, orient="horiz", command = function(...)tkxview(Canvas,...) )
		Vscroll <- tkscrollbar(frameUpperRigth, command = function(...)tkyview(Canvas,...) )
		Canvas <- tkcanvas(frameUpperRigth, relief = "sunken", borderwidth = Borderwidth, 
							width = Width, height = Height,
							xscrollcommand = function(...)tkset(Hscroll,...), 
							yscrollcommand = function(...)tkset(Vscroll,...)
							)
		
		assign("Canvas", Canvas, .EnvironmentArvoRe)
					
		# Pack the scroll bars.
		tkpack(Hscroll, side = "bottom", fill = "x")
		tkpack(Vscroll, side = "right", fill = "y")
		# Pack the canvas
		tkpack(Canvas, anchor = "center", side = "right", fill = "both", expand = 1)
		
		# Image setings.
		imgHeight <- 600
		imgWidth <- 800
		
		assign("imgHeight", imgHeight, .EnvironmentArvoRe)
		assign("imgWidth", imgWidth, .EnvironmentArvoRe)

		# Image file name setings.
		.Filename <- paste(tempdir(),"\\", "arvore.png", sep="")
		
		# unlink(x, recursive = FALSE)
		
		png(file=.Filename, width = imgWidth, height = imgHeight, bg = "white", restoreConsole = FALSE)
			plot.tree(TheTree, line.type = .treeangle, show.probability = .probabilityconf, 
						show.payoffs = .payoffsconf, show.notes = .notesconf, 
						node.name.font.size = .node.name.font.size, payoffs.font.size = .payoffs.font.size, 
						notes.font.size = .notes.font.size)
		dev.off()
		
		image1 <- tclVar()
		tcl("image","create","photo",image1,file=.Filename)
		tkcreate(Canvas, "image", imgWidth/2, imgHeight/2, image = image1, anchor = "center")
		tkconfigure(Canvas, scrollregion = c(0,0,imgWidth,imgHeight))
		file.remove(.Filename)
		
		
		###############################################################################
		# The keys
		###############################################################################
		
		tkbind(tt, "<Escape>",sair)
		tkbind(tt, "<Control_L><n>",new.file.bot)
		tkbind(tt, "<Control_L><o>",load.file.arv)
		tkbind(tt, "<Control_L><Alt_L><s>",save.file.arv)
		tkbind(tt, "<Control_L><s>",save.file.arv)
		tkbind(tt, "<Control_L><e>",naoimplementado)
		tkbind(tt, "<F5>",refreshF5)
		
		
		###############################################################################
		
		posiciona.janela.tela(tt)
		tkfocus(tt)
		tkwm.deiconify(tt)
	} else {
		msg <- paste("O programa ÁrvoRe já está sendo executado.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
	}	
}

