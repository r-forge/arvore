`displayInTable` <-
function(matrix1,title="",height=-1,width=-1,nrow=-1,ncol=-1, 
							titlerows = FALSE, titlecols = FALSE, editable = FALSE,
							returntt = TRUE) {
	require(tcltk)
	
	Original.Dada <- matrix1
	
	num.lin <- dim(matrix1)[1]
	num.col <- dim(matrix1)[2]

	if (titlecols && (!titlerows)) {
		TitleCols <- colnames(matrix1)
		if (is.null(colnames(matrix1))) TitleCols <- paste("Col ", 1:num.col, sep="")
		matrix1 <- rbind(TitleCols, matrix1)
		nrow <- nrow + 1
	}
	
	if ( titlerows && (!titlecols) ) {
		TitleRows <- rownames(matrix1)
		if (is.null(rownames(matrix1))) TitleRows <- paste("Row ", 1:num.lin, sep="")
		matrix1 <- cbind(TitleRows, matrix1)
		ncol <- ncol + 1
	} else {
		if ( titlerows && titlecols ) {
			TitleCols <- colnames(matrix1)
			if (is.null(colnames(matrix1))) TitleCols <- paste("Col ", 1:num.col, sep="")
			matrix1 <- rbind(TitleCols, matrix1)
			TitleRows <- rownames(matrix1)
			if (is.null(rownames(matrix1))) TitleRows <- paste("Row ", 1:num.lin, sep="")
			TitleRows <- c(" ", TitleRows)
			matrix1 <- cbind(TitleRows, matrix1)
			ncol <- ncol + 1
			nrow <- nrow + 1
		}
	}
	
	num.lin <- dim(matrix1)[1]
	num.col <- dim(matrix1)[2]

# remover se nao funcionar
	matrix1 <- matrix(as.character(matrix1), num.lin, num.col)
#---------------------------

# 	tamanhocoluna <- max(nchar(matrix1))
	
	tclarray <- tclArray()
	for (i in (1:num.lin))
	  for (j in (1:num.col))
	    tclarray[[i-1,j-1]] <- matrix1[i,j]

	if( editable ) {
		editable <- "normal"
	} else {
		editable <- "disabled"
	}
	
	displayInTableWindow <- tktoplevel()
	tclRequire("Tktable")
	tkwm.title(displayInTableWindow,title)
	
	table1 <- tkwidget(displayInTableWindow,"table",rows=nrow,cols=ncol,
			titlerows = sum(titlecols), titlecols = sum(titlerows),
			height=height+1,width=width+1,
			xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...),
			state = editable,
			colstretchmode = "all")
# 			colwidth = tamanhocoluna)
	xscr <-tkscrollbar(displayInTableWindow,orient="horizontal", command=function(...)tkxview(table1,...))
	yscr <- tkscrollbar(displayInTableWindow,command=function(...)tkyview(table1,...))
	
	tkgrid(table1, yscr, columnspan = 2)
	
	tkgrid.configure(yscr, sticky="nsw")
	tkgrid.configure(table1, sticky="nswe")
	
	tkgrid(xscr, sticky="new", columnspan = 2)
	
	tkconfigure(table1,variable=tclarray,background="white",selectmode="extended")
	
	OnExport <- function(Original.Dada) {
		filetypeWindow <- tktoplevel()
		title <- "ÁrvoRe - Exportar"
		tkwm.title(filetypeWindow,title)
		
	    frameOverall <- tkframe(filetypeWindow)
	    frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=2)
	    frameLower <- tkframe(frameOverall, borderwidth=2)
	
		tkgrid(tklabel(frameUpper,text="Selecione o tipo de arquivo:"))
		filetypes <- c("CSV (separado por vírgulas)","TXT (texto separado por tabulações)","Todos arquivos")
		fileextensions <- c(".csv", ".txt", " ")
			
		widthcombo <- max( nchar(filetypes) )
			
		comboBox <- tkwidget(frameUpper,"ComboBox", width = widthcombo, editable = FALSE, values = filetypes)
		tkgrid(comboBox)
		
		OnOK <- function() {
	   		filetypeChoice <- filetypes[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
	   		fileextChoice <- fileextensions[as.numeric(tclvalue(tcl(comboBox,"getvalue")))+1]
	   		tkdestroy(filetypeWindow)
	   		filetypes <- paste("{{ ", filetypeChoice, "}", " {", fileextChoice, "}}", sep = "")
	   		fileName <- tclvalue(tkgetSaveFile(filetypes=filetypes))
	   		
			if (!nchar(fileName))
				tkfocus(filetypeWindow)
			else {
				
				ans <- substr(fileName,nchar(fileName)-3,nchar(fileName))
				if ( fileextChoice == ".csv" ) {
					if (ans == ".csv") {
						write.csv2(Original.Dada, file = fileName, row.names = FALSE)
					} else {
						fileName <- paste(fileName, ".csv", sep = "")
						write.csv2(Original.Dada, file = fileName, row.names = FALSE)
					}
				}
				if ( fileextChoice == ".txt" ) {
					if (ans == ".txt") {
						write.table(Original.Dada, file = fileName, sep = "\t")
					} else {
						fileName <- paste(fileName, ".txt", sep = "")
						write.table(Original.Dada, file = fileName, sep = "\t")
					}
				}
				if ( fileextChoice == " " ) {
					if (ans == ".txt") {
						write.table(Original.Dada, file = fileName, sep = "\t")
					} else {
						fileName <- paste(fileName, ".txt", sep = "")
						write.table(Original.Dada, file = fileName, sep = "\t")
					}
				}	
				tkfocus(displayInTableWindow)
			}	
		    	}
	   	
	   	OnCancel <- function() {
	    	tkdestroy(filetypeWindow)
	    	tkfocus(displayInTableWindow)
	   	}
	   	
	   	.Width.but <- 10
		.Height.but <- 1
		
		OK.but <-tkbutton(frameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <-tkbutton(frameLower,text="Cancelar", width=.Width.but, height=.Height.but, command=OnCancel)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		
		tkgrid(frameUpper,sticky="nwe")
		tkgrid(frameLower,sticky="nwe")
		tkgrid(frameOverall)
		tkbind(filetypeWindow, "<Return>",OnOK)
		tkbind(filetypeWindow, "<Escape>",OnOK)
			
		tkfocus(filetypeWindow)
	}	
	
	OnOK <- function() {
		tkdestroy(displayInTableWindow)
		if (returntt) {
			tkwm.deiconify(tt)
			tkfocus(tt)
		}
    }
		
  	.Width.but <- 10
	.Height.but <- 1
		
	OK.but <-tkbutton(displayInTableWindow,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
	Export.but <-tkbutton(displayInTableWindow,text="Exportar", width=.Width.but, height=.Height.but, command=function() {OnExport(Original.Dada)})
		
	tkgrid(OK.but, Export.but, sticky = "s", padx = 5, pady = 5)
	tkbind(displayInTableWindow, "<Return>",OnOK)
	tkbind(displayInTableWindow, "<Escape>",OnOK)
	
	tkfocus(displayInTableWindow)
}

