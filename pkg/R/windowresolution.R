# FUNCTION :: windowresolution				# Criada em July 02, 2008 02:33:36 PM 
#		This function is the GUI to do something.				
#
#
# Revision : 	xxxx 		- xxx
#				
#
# Parameters

# Esta função faz alguma coisa

windowresolution <- function() {
		# Criação da janela
		win.main.resWindow <- tktoplevel()
		title.window <- "ÁrvoRe - Tamanho da Janela"	# Título da janela
		tkwm.title(win.main.resWindow, title.window)

		# Variáveis TclTk
		qualquercoisa.tclvar <- tclVar( 0000 )
		
		# Frames da janela
		FrameOverAll <- tkframe(win.main.resWindow, height = 200, width = 150, borderwidth = 0, relief = "groove")
		FrameButtons <- tkframe(win.main.resWindow, height = 200, width = 150, borderwidth = 0, relief = "groove")
		
		tkpack(FrameOverAll)
		tkpack(FrameButtons)

		# Check bottons
		
		# List box
		
				
		tkgrid(FrameButtons, columnspan = 2, sticky = "s")
		
#------- BOTOES ---------------------------------------------------------------------------------------------
		OnOK <- function() {
			
		}
		
		OnCancel <- function() {
			tkdestroy(win.main.resWindow)
			tkfocus(tt)
		}
		
	   	.Width.but <- 10
		.Height.but <- 1
		
		rotulo <- "OK"
		OK.but <- tkbutton(FrameButtons, text = rotulo, width = .Width.but, height = .Height.but, 
							command = OnOK)
		rotulo <- "Cancelar"
		Cancel.but <- tkbutton(FrameButtons, text = rotulo, width = .Width.but, height = .Height.but, 
							command = OnCancel)
		
		tkgrid(OK.but, Cancel.but, columnspan = 2, sticky = "s", padx = 5, pady = 5)
#------- TECLAS ATALHO --------------------------------------------------------------------------------------
		tkbind(win.main.resWindow, "<Return>", OnOK)
		tkbind(win.main.resWindow, "<Escape>", OnCancel)
#------- MONTAGEM FINAL -------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
		tkfocus(win.main.resWindow)


}
