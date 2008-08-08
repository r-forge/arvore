\name{set.value}
\alias{set.value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set.value(TheTree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
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
function(TheTree) {
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		node.type <- TheTree$Type[position]
		node.name <- TheTree$Node.name[position]
		
		setvalueWindow <- tktoplevel()
		title <- "ÁrvoRe - Propriedades"
		tkwm.title(setvalueWindow,title)
		
		# Create frames
		FrameOverall <- tkframe(setvalueWindow, borderwidth = 0, relief = "groove")
		FrameLeft <- tkframe(FrameOverall, borderwidth = 0, relief = "groove")
		FrameRight <- tkframe(FrameOverall, borderwidth = 0, relief = "groove")
		FrameButton <- tkframe(FrameRight, borderwidth = 2, relief = "groove")
		FrameMenuButton <- tkframe(FrameLeft, borderwidth = 2, relief = "groove")
		FrameLower <- tkframe(FrameOverall, borderwidth = 0, relief = "groove")
		
		# Node label
		text.to.label <- paste("Nodo : ", node.name, sep = "")
		node.tk.label <- tklabel(FrameLeft, text = text.to.label)
		if (node.type == "C") node.type.label <- "Chance"
		else if (node.type == "T") node.type.label <- "Terminal"
		else if (node.type == "M") node.type.label <- "Markov"
		else if (node.type == "D") node.type.label <- "Decision"
		else node.type.label <- "Unknow"
		
		text.to.label <- paste("Tipo : ", node.type.label, sep = "")
		node.tk.type <- tklabel(FrameLeft, text = text.to.label)
		
		tkgrid(node.tk.label, sticky = "nw", columnspan = 2)
		tkgrid(node.tk.type, sticky = "nw", columnspan = 2)
		
		# The menubutton width
		menubutton.width <- 15
		
######### O menubutton
		Operators <- tkmenubutton(FrameMenuButton, text = "Operadores", direction = "below", 
							borderwidth = 1, relief = "raised", indicatoron = TRUE,
							width = menubutton.width)	
######### O menu associado ao menubutton
		menuOperatorsChild <- tkmenu(Operators, tearoff=FALSE)
			# Os ítens do ítem "Botão de menu"
			tkadd(menuOperatorsChild,"command",label=">",command=function() {})
			tkadd(menuOperatorsChild,"command",label="<",command=function() {})
			tkadd(menuOperatorsChild,"command",label=">=",command=function() {})
			tkadd(menuOperatorsChild,"command",label="<=",command=function() {})
			tkadd(menuOperatorsChild,"command",label="==",command=function() {})
			tkadd(menuOperatorsChild,"separator")
			tkadd(menuOperatorsChild,"command",label="&&",command=function() {})
			tkadd(menuOperatorsChild,"command",label="||",command=function() {})
			tkadd(menuOperatorsChild,"separator")
			tkadd(menuOperatorsChild,"command",label="(",command=function() {})
			tkadd(menuOperatorsChild,"command",label=")",command=function() {})			
			tkadd(menuOperatorsChild,"separator")
			tkadd(menuOperatorsChild,"separator")
			tkadd(menuOperatorsChild,"command",label="Sair",command=function() tkdestroy(setvalueWindow))
		# Ajusta que o menu associado ao menubutton é menufilho
		tkconfigure(Operators, menu = menuOperatorsChild)
		# Monta o rótulo e o checkbutton
		
		
######### O menubutton
		Functions <- tkmenubutton(FrameMenuButton, text = "Funções", direction = "below", 
							borderwidth = 1, relief = "raised", indicatoron = TRUE,
							width = menubutton.width)
######### O menu associado ao menubutton
		menuFunctionsChild <- tkmenu(Functions, tearoff = FALSE)
			# Os ítens do ítem "Botão de menu"
			tkadd(menuFunctionsChild,"command",label="X",command=function() {})
			tkadd(menuFunctionsChild,"command",label="XX",command=function() {})
			tkadd(menuFunctionsChild,"separator")
			tkadd(menuFunctionsChild,"command",label="XXX",command=function() {})
			tkadd(menuFunctionsChild,"command",label="XXXX",command=function() {})
		# Ajusta que o menu associado ao menubutton é menufilho
		tkconfigure(Functions, menu = menuFunctionsChild)
		
######### O menubutton
		Keywords <- tkmenubutton(FrameMenuButton, text = "Palavra chave", direction = "below", 
							borderwidth = 1, relief = "raised", indicatoron = TRUE,
							width = menubutton.width)
######### O menu associado ao menubutton
		menuKeywordsChild <- tkmenu(Keywords, tearoff = FALSE)
			# Os ítens do ítem "Botão de menu"
			tkadd(menuKeywordsChild,"command",label=".stage",command=function() {})
			tkadd(menuKeywordsChild,"command",label=".stage.cost",command=function() {})
			tkadd(menuKeywordsChild,"command",label=".stage.eff",command=function() {})
			tkadd(menuKeywordsChild,"command",label=".stage.reward",command=function() {})
			tkadd(menuKeywordsChild,"separator")
			tkadd(menuKeywordsChild,"command",label=".total.cost",command=function() {})
			tkadd(menuKeywordsChild,"command",label=".total.eff",command=function() {})
			tkadd(menuKeywordsChild,"command",label=".total.reward",command=function() {})
			tkadd(menuKeywordsChild,"command",label="NONE",command=function() {})
		# Ajusta que o menu associado ao menubutton é menufilho
		tkconfigure(Keywords, menu = menuKeywordsChild)

		# Monta os menubuttons
		tkgrid(Operators, Functions, Keywords, sticky = "n", padx = 5, pady = 5)

		Text.space <- tktext(FrameLeft, borderwidth = 2, relief = "sunken",
								height = 5, width = 30, wrap = "word")
		
		
		
		OnOK <- function()
		{
			
		}
		
		OnCancel <- function()
		{
			tkdestroy(setvalueWindow)
			tkfocus(tt)
		}
		
    	.Width.but <- 10
		.Height.but <- 1
		
		OK.but <-tkbutton(FrameLower,text="OK", width=.Width.but, height=.Height.but, command=OnOK)
		Cancel.but <-tkbutton(FrameLower,text="Cancel", width=.Width.but, height=.Height.but, command=OnCancel)
		
		tkbind(setvalueWindow, "<Escape>",OnCancel)
		
		tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
		
		tkgrid(FrameButton, sticky = "nwe")
		tkgrid(FrameMenuButton, sticky = "nwe")
		tkgrid(Text.space, sticky = "swe", padx = 5, pady = 5)
		tkgrid(FrameLeft, FrameRight, sticky = "nwe")
		tkgrid(FrameLower, sticky = "swe")
		tkgrid(FrameOverall)
		
		tkfocus(setvalueWindow)
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
