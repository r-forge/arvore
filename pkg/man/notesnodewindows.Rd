\name{notesnodewindows}
\alias{notesnodewindows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
notesnodewindows(...)
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
	nodeSec <- nodoselecionado()
		if ( nodeSec[1] == " ") {
			msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
			tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
			tkfocus(tt)
		} else {
			node.number <- as.numeric(nodeSec[3])
			column <- as.numeric(nodeSec[2])
			
			notesWindow <- tktoplevel()
			title <- "ÁrvoRe - Comentários do Nodo"
			tkwm.title(notesWindow,title)
			
			position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
			
			Notesvar <- tclVar(TheTree$Note[position])
			entry.Value  <- tkentry(notesWindow, width="20", textvariable=Notesvar)
			tkgrid(tklabel(notesWindow,text="Nota"))
			tkgrid(entry.Value)
			
			OnOK <- function()
			{
				NotesVal <- as.character(tclvalue(Notesvar))
				tkdestroy(notesWindow)
				safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
				setnotesnode(TheTree, column = column, node.number = node.number, nodo.note = NotesVal, .EnvironmentArvoRe)
				refreshF5()
				tkfocus(tt)
			}
			OK.but <-tkbutton(notesWindow, text="   OK   ", command=OnOK)
			tkbind(entry.Value, "<Return>", OnOK)
			
			OnCancel <- function()
			{
				tkdestroy(notesWindow)
				tkfocus(tt)
			}

			Cancel.but <-tkbutton(notesWindow, text=" Cancel ", command=OnCancel)
			tkbind(notesWindow, "<Escape>", OnCancel)

			tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)

			posiciona.janela.no.mouse(notesWindow, 200, 100)
			tkfocus(notesWindow)
		}	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
