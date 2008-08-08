`dialog.variable.window` <-
function(...) {
	if (!exists("variableMAT",.EnvironmentArvoRe)) new.variable.list() # se não existe uma tabela de variaveis, então ele cria.
	
	variableWindow <- tktoplevel()
	title <- "ÁrvoRe - Variáveis"
	tkwm.title(variableWindow,title)
	
	frameOverall <- tkframe(variableWindow)
	frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=0)
	frameUpperLeft <- tkframe(frameUpper, relief="groove", borderwidth=2)
	frameUpperRigth <- tkframe(frameUpper, relief="groove", borderwidth=2)
	frameLower <- tkframe(frameOverall, relief="groove", borderwidth=2)
	
	scrvar <- tkscrollbar(frameUpperRigth, repeatinterval=5,
					command=function(...)tkyview(tlvar,...))
	tlvar <- tklistbox(frameUpperRigth,height=4,selectmode="single",
					yscrollcommand=function(...)tkset(scrvar,...),background="white")
	tkgrid(tklabel(frameUpperRigth,text="Variáveis"))
	tkgrid(tlvar,scrvar)
	tkgrid.configure(scrvar,rowspan=4,sticky="nsw")
	
	tkgrid(frameUpperLeft, frameUpperRigth,sticky="nsw")
	tkgrid(frameUpper,sticky="ns")
	tkgrid(frameLower,sticky="ns")
	
	variablesnames <- variableMAT[,1]
	
	if (length(variablesnames) > 0) {
		for (i in (1:length(variablesnames))) {
	    	tkinsert(tlvar,"end",variablesnames[i])
		}
	}
	
	AddSelection <- function()
	{
	    addvariableWindow <- tktoplevel()
	    title <- "ÁrvoRe - Variáveis"
	    tkwm.title(addvariableWindow,title)
	    
	    frameOverall <- tkframe(addvariableWindow)
	    frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=2)
	    frameLower <- tkframe(frameOverall, borderwidth=2)
	    
	    tkgrid(tklabel(frameOverall,text="Nova Variável"))
	    
	    Namevar <- tclVar("")
	    Fixvar <- tclVar(0)
	    Minvar <- tclVar(0)
	    Maxvar <- tclVar(0)
	    Notesvar <- tclVar("")
	    
	    campowidth <- 25
		Name.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Namevar)
		tkgrid(tklabel(frameUpper,text="Nome da variável"), sticky = "n")
		tkgrid(Name.var.Value, sticky = "n")
		
		Fix.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Fixvar)
		tkgrid(tklabel(frameUpper,text="Valor padrão da variável"), sticky = "n")
		tkgrid(Fix.var.Value, sticky = "n")
		
		Min.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Minvar)
		tkgrid(tklabel(frameUpper,text="Valor mínimo da variável"), sticky = "n")
		tkgrid(Min.var.Value, sticky = "n")

		Max.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Maxvar)
		tkgrid(tklabel(frameUpper,text="Valor máximo da variável"), sticky = "n")
		tkgrid(Max.var.Value, sticky = "n")

		Notes.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Notesvar)
		tkgrid(tklabel(frameUpper,text="Notas"), sticky = "n")
		tkgrid(Notes.var.Value, sticky = "n")
		
		OnOkAdd <- function() {
			Allok <- TRUE
			NameVal <- as.character(tclvalue(Namevar))
			FixVal <- as.integer(tclvalue(Fixvar))
			MinVal <- as.integer(tclvalue(Minvar))
			MaxVal <- as.integer(tclvalue(Maxvar))
			NotesVal <- as.character(tclvalue(Notesvar))
			
			if((nchar(NameVal) <= 0)&& Allok) {
				Allok <- FALSE
				msg <- "Este não é um nome válido para uma variável."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if((!is.numeric(FixVal))&& Allok) {
				Allok <- FALSE
				msg <- "Este não é um valor fixo válido para uma variável."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if((!is.numeric(MinVal))&& Allok) {
				Allok <- FALSE
				msg <- "Este não é um valor mínimo válido para uma variável."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if((!is.numeric(MaxVal))&& Allok) {
				Allok <- FALSE
				msg <- "Este não é um valor máximo válido para uma variável."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if((MinVal >= MaxVal)&& Allok) {
				Allok <- FALSE
				msg <- "O valor mínimo de uma variável deve ser menor que o valor máximo."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if(Allok) {
				newvariableline <- data.frame(Name = NameVal, Fix.Value = FixVal, Min.Value = MinVal,
											Max.Value = MaxVal, Notes = NotesVal)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				setvariablelist(variableMAT = variableMAT, newvariableline = newvariableline, action = "add")
				tkinsert(tlvar,"end",NameVal)
				tkdestroy(addvariableWindow)
				tkfocus(variableWindow)
			}
			
		}
		
		OnCanceladd <- function() {
			tkdestroy(addvariableWindow)
			tkfocus(variableWindow)
		}
		
		.Width.but <- 10
		.Height.but <- 1

		OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOkAdd)
		Cancel.but <-tkbutton(frameLower,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCanceladd)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		
		tkgrid(frameUpper,sticky="nwe")
		tkgrid(frameLower,sticky="nwe")
		tkgrid(frameOverall)
		
		tkbind(addvariableWindow, "<Return>",OnOkAdd)
		tkbind(addvariableWindow, "<Escape>",OnCanceladd)
		
		tkfocus(addvariableWindow)
	}

	DeleteSelection <- function()
	{
	    variableIndex <- as.integer(tkcurselection(tlvar))
	    variableslist <- variableMAT$Name
	    variabletodelete <- as.character(variableslist[variableIndex+1])
	    safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
	    setvariablelist(variableMAT = variableMAT, variable.name = variabletodelete, action = "delete")
	    tkdelete(tlvar,variableIndex)
	    tkfocus(variableWindow)
	}
	
	EditSelection <- function()
	{	
	    variableIndex <- as.integer(tkcurselection(tlvar))

	   	variableslist <- variableMAT$Name
	   	variableselected <- as.character(variableslist[variableIndex+1])
	    	
	   	addvariableWindow <- tktoplevel()
	   	title <- "ÁrvoRe - Variáveis"
	   	tkwm.title(addvariableWindow,title)
	    	
	   	frameOverall <- tkframe(addvariableWindow)
	   	frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=2)
	   	frameLower <- tkframe(frameOverall, borderwidth=2)
	    	
	   	tkgrid(tklabel(frameOverall,text="Propriedades da Variável"))
	    
	   	variableMATnames <- names(variableMAT)
	   	Data <- subset(variableMAT, Name == variableselected, select = variableMATnames)
	    
	   	Namevar <- tclVar(Data$Name)
	   	Fixvar <- tclVar(Data$Fix.Value)
	   	Minvar <- tclVar(Data$Min.Value)
	   	Maxvar <- tclVar(Data$Max.Value)
	   	Notesvar <- tclVar(Data$Notes)
	    
	   	campowidth <- 25
		Name.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Namevar)
		tkgrid(tklabel(frameUpper,text="Nome da variável"), sticky = "n")
		tkgrid(Name.var.Value, sticky = "n")
		
		Fix.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Fixvar)
		tkgrid(tklabel(frameUpper,text="Valor padrão da variável"), sticky = "n")
		tkgrid(Fix.var.Value, sticky = "n")
	
		Min.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Minvar)
		tkgrid(tklabel(frameUpper,text="Valor mínimo da variável"), sticky = "n")
		tkgrid(Min.var.Value, sticky = "n")
		
		Max.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Maxvar)
		tkgrid(tklabel(frameUpper,text="Valor máximo da variável"), sticky = "n")
		tkgrid(Max.var.Value, sticky = "n")

		Notes.var.Value  <- tkentry(frameUpper, width=campowidth,textvariable=Notesvar)
		tkgrid(tklabel(frameUpper,text="Notas"), sticky = "n")
		tkgrid(Notes.var.Value, sticky = "n")
		
		OnOkAdd <- function() {
			Allok <- TRUE
			NameVal <- as.character(tclvalue(Namevar))
			FixVal <- as.integer(tclvalue(Fixvar))
			MinVal <- as.integer(tclvalue(Minvar))
			MaxVal <- as.integer(tclvalue(Maxvar))
			NotesVal <- as.character(tclvalue(Notesvar))
				
			if((nchar(NameVal) <= 0)&& Allok) {
				Allok <- FALSE
				msg <- "Este não é um nome válido para uma variável."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if((!is.numeric(FixVal))&& Allok) {
				Allok <- FALSE
				msg <- "Este não é um valor fixo válido para uma variável."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if((!is.numeric(MinVal))&& Allok) {
				Allok <- FALSE
				msg <- "Este não é um valor mínimo válido para uma variável."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if((!is.numeric(MaxVal))&& Allok) {
				Allok <- FALSE
				msg <- "Este não é um valor máximo válido para uma variável."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if((MinVal >= MaxVal)&& Allok) {
				Allok <- FALSE
				msg <- "O valor mínimo de uma variável deve ser menor que o valor máximo."
				tkmessageBox(message = msg, icon="error")
				tkfocus(addvariableWindow)
			}
			if(Allok) {
				oldvariable.name <- Data$Name
				newvariableline <- data.frame(Name = NameVal, Fix.Value = FixVal, Min.Value = MinVal,
											Max.Value = MaxVal, Notes = NotesVal)
											
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				if (length(oldvariable.name) == 0) {
					setvariablelist(variableMAT = variableMAT, newvariableline = newvariableline, 
										action = "add")
					oldvariable.name <- " "
				} else {
					setvariablelist(variableMAT = variableMAT, newvariableline = newvariableline, 
										variable.name = oldvariable.name, action = "edit")
				}
				
			    if (oldvariable.name != NameVal) {
				    if (oldvariable.name != " ") tkdelete(tlvar,variableIndex)
				    tkinsert(tlvar,"end",NameVal)
			    }
				tkdestroy(addvariableWindow)
				tkfocus(variableWindow)
			}
				
		}

		OnCanceladd <- function() {
			tkdestroy(addvariableWindow)
			tkfocus(variableWindow)
		}
		
		.Width.but <- 10
		.Height.but <- 1

		OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOkAdd)
		Cancel.but <-tkbutton(frameLower,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCanceladd)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		
		tkgrid(frameUpper,sticky="nwe")
		tkgrid(frameLower,sticky="nwe")
		tkgrid(frameOverall)
		
		tkbind(addvariableWindow, "<Return>",OnOkAdd)
		tkbind(addvariableWindow, "<Escape>",OnCanceladd)
		
		tkfocus(addvariableWindow)
		
	}

	OnOK <- function()
	{
		tkdestroy(variableWindow)
		tkfocus(tt)
	}
		
	.Width.but <- 10
	.Height.but <- 1
	
	OK.but <-tkbutton(frameOverall,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
	Edit.but <-tkbutton(frameUpperLeft,text="Editar", width=.Width.but, height=.Height.but,command=EditSelection)
	Add.but <-tkbutton(frameUpperLeft,text="Nova", width=.Width.but, height=.Height.but,command=AddSelection)
	Delete.but <-tkbutton(frameUpperLeft,text="Apagar", width=.Width.but, height=.Height.but,command=DeleteSelection)

	tkbind(variableWindow, "<Return>",OnOK)
	tkbind(variableWindow, "<Escape>",OnOK)
		
	tkgrid(OK.but, sticky = "s", padx = 5, pady = 5)
	tkgrid(Add.but, sticky = "s", padx = 5, pady = 5)
	tkgrid(Delete.but, sticky = "s", padx = 5, pady = 5)
	tkgrid(Edit.but, sticky = "s", padx = 5, pady = 5)
	
	tkgrid(frameOverall)
	
	posiciona.janela.no.mouse(variableWindow, 250, 160)
					
	tkfocus(variableWindow)
}

