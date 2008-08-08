`icer.sim.window` <-
function(Alltreatmentstable) {
	require(abind)
	
		CEsimtableWindow <- tktoplevel()
		title <- "ÁrvoRe - Análise de Custo-Efetividade"
		tkwm.title(CEsimtableWindow,title)
		
		# Cria o primeiro frame
		FrameOverAll <- tkframe(CEsimtableWindow, borderwidth = 0, relief = "groove")
		Frame1 <- tkframe(FrameOverAll, borderwidth = 2, relief = "groove")
		Frame2 <- tkframe(FrameOverAll, borderwidth = 0, relief = "sunken")
		
		# Cria o label
		textlabellista <- "Selecione o procedimento padrão para ACE. \n Ele será a base de comparação para os demais."
		rotulolista <- tklabel(Frame1, text = textlabellista)
		tkgrid(rotulolista, columnspan = 2)
		
		# Cria uma barra de rolagem
		scr <- tkscrollbar(Frame1, repeatinterval=5, command=function(...)tkyview(tl,...))
		
		Data.CEA <- Alltreatmentstable
		Data.CEA.Cost <- subset(Data.CEA, Data == "Cost")
		Data.CEA.Effectiveness <- subset(Data.CEA, Data == "Effectiveness")
		Data.CEA.CE <- subset(Data.CEA, Data == "C/E")
		n.treat <- 1:length(Data.CEA.Cost$Treatment)
		
		Data.CEA.Cost <- data.frame(NT = n.treat, Data.CEA.Cost)
		Data.CEA.Effectiveness <- data.frame(NT = n.treat, Data.CEA.Effectiveness)
		Data.CEA.CE <- data.frame(NT = n.treat, Data.CEA.CE)
		
# 		print(Data.CEA.Cost)
# 		print(Data.CEA.Effectiveness)
# 		print(Data.CEA.CE)

		# Cria os elementos da lista
		elementos <- Data.CEA.Cost$Treatment
		
		# Determina a altura da listbox
		heightlistbox <- length(elementos)
		larguratexto <- max(nchar(elementos)) + 4
		# Cria uma listbox					
		tl <- tklistbox(Frame1, height = 5, width = larguratexto, selectmode = "single",
						yscrollcommand = function(...)tkset(scr,...), background="white")
		
		# Adiciona os elementos à listbox
		for (i in (1:heightlistbox)) {
		    tkinsert(tl, "end", elementos[i])
		}
		
		# Monta a listbox e a barra de rolagem
		tkgrid(tl, scr, sticky="nse")
		
# 		tkgrid(tklabel(Frame1, text = " "))
		
		# Ajusta a barra de rolagem
		tkgrid.configure(scr, rowspan = 5, sticky="nsw")
		
		# Define o "Elemento 2" como padrão da listbox.
		# Para a listbox o índice começa em zero
		tkselection.set(tl, 0)
		
		# Monta os frames
		tkgrid(Frame1, Frame2, sticky = "nwe", padx = 5, pady = 5)
		tkgrid(FrameOverAll, sticky = "nswe", columnspan = 2)
		
		OnOK <- function() {
			respostaListbox <- n.treat[as.numeric(tkcurselection(tl))+1]
			
			Data.alternative.Cost <- subset(Data.CEA.Cost, NT != respostaListbox)			
			Data.standart.Cost <- subset(Data.CEA.Cost, NT == respostaListbox)
			Data.alternative.Effectiveness <- subset(Data.CEA.Effectiveness, NT != respostaListbox)			
			Data.standart.Effectiveness <- subset(Data.CEA.Effectiveness, NT == respostaListbox)
			Data.alternative.CE <- subset(Data.CEA.CE, NT != respostaListbox)			
			Data.standart.CE <- subset(Data.CEA.CE, NT == respostaListbox)
			
# 			print(Data.alternative.Cost)
# 			print(Data.standart.Cost)
# 			print(Data.alternative.Effectiveness)
# 			print(Data.standart.Effectiveness)
# 			print(Data.alternative.CE)
# 			print(Data.standart.CE)
			
# 			Data.alternative.Cost$Mean <- as.numeric(as.character(Data.alternative.Cost$Mean))
# 			Data.alternative.Effectiveness$Mean <- as.numeric(as.character(Data.alternative.Effectiveness$Mean))
# 			Data.alternative.Cost$Variance <- as.numeric(as.character(Data.alternative.Cost$Variance))
# 			Data.alternative.Effectiveness$Variance <- as.numeric(as.character(Data.alternative.Effectiveness$Variance))
# 			Data.alternative.Cost$CovDcDe <- as.numeric(as.character(Data.alternative.Cost$CovDcDe))
# 			Data.alternative.Effectiveness$CovDcDe <- as.numeric(as.character(Data.alternative.Effectiveness$CovDcDe))
# 			Data.standart.Cost$Mean <- as.numeric(as.character(Data.standart.Cost$Mean))
# 			Data.standart.Effectiveness$Mean <- as.numeric(as.character(Data.standart.Effectiveness$Mean))
# 			Data.standart.Cost$Variance <- as.numeric(as.character(Data.standart.Cost$Variance))
# 			Data.standart.Effectiveness$Variance <- as.numeric(as.character(Data.standart.Effectiveness$Variance))
# 			Data.standart.Cost$CovDcDe <- as.numeric(as.character(Data.standart.Cost$CovDcDe))
# 			Data.standart.Effectiveness$CovDcDe <- as.numeric(as.character(Data.standart.Effectiveness$CovDcDe))
			
			
			ans <- data.frame( 	Strategy = Data.standart.Cost$Treatment[1],
								Cost = Data.standart.Cost$Mean[1], 
								Incr.Cost = NA, 
								Effectiveness = Data.standart.Effectiveness$Mean[1], 
								Incr.Eff. = NA, 
								CE.ratio = Data.standart.Cost$Mean[1] / Data.standart.Effectiveness$Mean[1], 
								ICER = NA,
								Var.ICER = NA,
								Sd.ICER = NA,
								LL_IC95 = NA,
								UL_IC95 = NA
								)
			
			for (i in 1:dim(Data.alternative.Cost)[1]) {
				
				ans$Strategy <- as.character(ans$Strategy)
				ans$Cost <- as.numeric(as.character(ans$Cost))
				ans$Incr.Cost <- as.numeric(as.character(ans$Incr.Cost))
				ans$Effectiveness <- as.numeric(as.character(ans$Effectiveness))
				ans$Incr.Eff. <- as.numeric(as.character(ans$Incr.Eff.))
				ans$CE.ratio <- as.numeric(as.character(ans$CE.ratio))
				ans$ICER <- as.numeric(as.character(ans$ICER))
				ans$Var.ICER <- as.numeric(as.character(ans$Var.ICER))
				ans$Sd.ICER <- as.numeric(as.character(ans$Sd.ICER))
				ans$LL_IC95 <- as.numeric(as.character(ans$LL_IC95))
				ans$UL_IC95 <- as.numeric(as.character(ans$UL_IC95))
				
				
				icer <- (Data.alternative.Cost$Mean[i] - Data.standart.Cost$Mean[1]) /
										(Data.alternative.Effectiveness$Mean[i] - Data.standart.Effectiveness$Mean[1])
										
				var.icer <- ( icer ) * (
							( Data.alternative.Effectiveness$Variance[i] / Data.alternative.Effectiveness$Mean[i]^2 ) +
							( Data.alternative.Cost$Variance[i] / Data.alternative.Cost$Mean[i]^2 ) -
							2 * ( Data.alternative.Cost$CovDcDe[i] ) / 
										( Data.alternative.Effectiveness$Mean[i] / Data.alternative.Cost$Mean[i] )
							)
				print(var.icer)
				
				var.icer <- as.numeric(as.character(var.icer))
				
				ans.line <- data.frame( Strategy = Data.alternative.Cost$Treatment[i],
								Cost = Data.alternative.Cost$Mean[i], 
								Incr.Cost = Data.alternative.Cost$Mean[i] - Data.standart.Cost$Mean[1], 
								Effectiveness = Data.alternative.Effectiveness$Mean[i], 
								Incr.Eff. = Data.alternative.Effectiveness$Mean[i] - Data.standart.Effectiveness$Mean[1], 
								CE.ratio = Data.alternative.Cost$Mean[i] / Data.alternative.Effectiveness$Mean[i], 
								ICER = icer,
								Var.ICER = var.icer,
								Sd.ICER = (var.icer)^(1/2),
								LL_IC95 = icer - qnorm(1 - 0.05/2) * var.icer^0.5,
								UL_IC95 = icer + qnorm(1 - 0.05/2) * var.icer^0.5
								)
				ans <- rbind(ans, ans.line) #, along = 1)
				ans <- as.data.frame(ans)
				
			}
			ans <- as.data.frame(ans)
			
# 			print(ans)
			
			displayInTable(as.matrix(ans), title="ICER - Análise de Custo-Efetividade",
						height=10,width=8,nrow=dim(ans)[1],ncol=dim(ans)[2], 
						titlerows = FALSE, titlecols = TRUE, returntt = FALSE)
			rm(ans)
	    }
			
	    OnCancel <- function() {
			tkdestroy(CEsimtableWindow)
	# 		tkwm.deiconify(tt)
			tkfocus(tt)
	    }
	    
	  	.Width.but <- 10
		.Height.but <- 1
			
		OK.but <-tkbutton(CEsimtableWindow,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <-tkbutton(CEsimtableWindow,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCancel)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		tkbind(CEsimtableWindow, "<Return>",OnOK)
		tkbind(CEsimtableWindow, "<Escape>",OnOK)
		
		posiciona.janela.no.mouse(CEsimtableWindow, 300, 180)
		
		tkfocus(CEsimtableWindow)

}

