\name{markov.nodes.properties}
\alias{markov.nodes.properties}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
markov.nodes.properties(TheTree, .EnvironmentArvoRe)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
  \item{.EnvironmentArvoRe}{ ~~Describe \code{.EnvironmentArvoRe} here~~ }
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
function(TheTree, .EnvironmentArvoRe) {
	exist.var <- exists("markov.propertiesMAT", envir = .EnvironmentArvoRe) 
	if (!exist.var) {
		markov.propertiesMAT <- data.frame(	"Level" = array(,0),
											"Node.N" = array(,0),
											"Node.name" = array(,0),
											"Father" = array(,0),
											"Father.Name" = array(,0),
											"Initial.cost" = array(,0), 
											"Incremental.cost" = array(,0), 
											"Final.cost" = array(,0),
											"Initial.effectiveness" = array(,0), 
											"Incremental.effectiveness" = array(,0), 
											"Final.effectiveness" = array(,0))
	} else {	
		markov.propertiesMAT <- get("markov.propertiesMAT", .EnvironmentArvoRe)
	}
	
	nodeSec <- nodoselecionado()
	if ( nodeSec[1] == " ") {
		msg <- paste("Nenhum nodo selecionado. Selecione um nodo da árvore e tente novamente.")
		tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
		tkfocus(tt)
	} else {
		node.number <- as.numeric(nodeSec[3])
		column <- as.numeric(nodeSec[2])
		position <- intersect(which((TheTree$Level == column)),which(TheTree$Node.N == node.number))
		position.father <- intersect(which((TheTree$Level == (column-1))),which(TheTree$Node.N == TheTree$Father[position]))
		
		if ( (TheTree$Type[position.father] == "M") &&
			((TheTree$Type[position] == "T") || (TheTree$Type[position] == "C")) ) {
			markovnodeWindow <- tktoplevel()
			title <- "ÁrvoRe - Propriedades"
			tkwm.title(markovnodeWindow,title)
			
			position.markov <- intersect(which((markov.propertiesMAT$Level == column)),
									which(markov.propertiesMAT$Node.N == node.number))
			
			if ( length(position.markov) != 0) {
				Initial.costvar <- tclVar(markov.propertiesMAT$Initial.cost[position.markov])
				Incremental.costvar <- tclVar(markov.propertiesMAT$Incremental.cost[position.markov])
				Final.costvar <- tclVar(markov.propertiesMAT$Final.cost[position.markov])
				Initial.effectivenessvar <- tclVar(markov.propertiesMAT$Initial.effectiveness[position.markov])
				Incremental.effectivenessvar <- tclVar(markov.propertiesMAT$Incremental.effectiveness[position.markov])
				Final.effectivenessvar <- tclVar(markov.propertiesMAT$Final.effectiveness[position.markov])
			} else {
				Initial.costvar <- tclVar(0)
				Incremental.costvar <- tclVar(0)
				Final.costvar <- tclVar(0)
				Initial.effectivenessvar <- tclVar(0)
				Incremental.effectivenessvar <- tclVar(0)
				Final.effectivenessvar <- tclVar(0)
			}
			
			entry.Value  <- tkentry(markovnodeWindow,width="20",textvariable=Initial.costvar)
			tkgrid(tklabel(markovnodeWindow,text="Custo Inicial (ciclo zero)"), sticky = "nw")
			tkgrid(entry.Value, sticky = "n")
			
			entry.Value2  <- tkentry(markovnodeWindow,width="20",textvariable=Incremental.costvar)
			tkgrid(tklabel(markovnodeWindow,text="Custo Adicional (por ciclo)"), sticky = "nw")
			tkgrid(entry.Value2, sticky = "n")
			
			entry.Value3  <- tkentry(markovnodeWindow,width="20",textvariable=Final.costvar)
			tkgrid(tklabel(markovnodeWindow,text="Custo Final (após o final)"), sticky = "nw")
			tkgrid(entry.Value3, sticky = "n")
			
			entry.Value4  <- tkentry(markovnodeWindow,width="20",textvariable=Initial.effectivenessvar)
			tkgrid(tklabel(markovnodeWindow,text="Efetividade Inicial (ciclo zero)"), sticky = "nw")
			tkgrid(entry.Value4, sticky = "n")
	
			entry.Value5  <- tkentry(markovnodeWindow,width="20",textvariable=Incremental.effectivenessvar)
			tkgrid(tklabel(markovnodeWindow,text="Efetividade Adicional (por ciclo)"), sticky = "nw")
			tkgrid(entry.Value5, sticky = "n")
			
			entry.Value6  <- tkentry(markovnodeWindow,width="20",textvariable=Final.effectivenessvar)
			tkgrid(tklabel(markovnodeWindow,text="Efetividade Final (após o final)"), sticky = "nw")
			tkgrid(entry.Value6, sticky = "n")
	
			OnOK <- function()
			{
				Initial.costVal <- as.character(tclvalue(Initial.costvar))
				Incremental.costVal <- as.character(tclvalue(Incremental.costvar))
				Final.costVal <- as.character(tclvalue(Final.costvar))
				Initial.effectivenessVal <- as.character(tclvalue(Initial.effectivenessvar))
				Incremental.effectivenessVal <- as.character(tclvalue(Incremental.effectivenessvar))
				Final.effectivenessVal <- as.character(tclvalue(Final.effectivenessvar))
								
				if ( (!is.na(Initial.costVal)) && (nchar(Initial.costVal) > 0) &&
					(!is.na(Incremental.costVal)) && (nchar(Incremental.costVal) > 0) &&
					(!is.na(Final.costVal)) && (nchar(Final.costVal) > 0) &&
					(!is.na(Initial.effectivenessVal)) && (nchar(Initial.effectivenessVal) > 0) &&
					(!is.na(Incremental.effectivenessVal)) && (nchar(Incremental.effectivenessVal) > 0) &&
					(!is.na(Final.effectivenessVal)) && (nchar(Final.effectivenessVal) > 0)
				 ) {
					tkdestroy(markovnodeWindow)
					safedofunction(TheTree, .EnvironmentArvoRe, .modeltypeArvore)
					set.markov.nodes.properties(TheTree, markov.propertiesMAT, column = column, node.number = node.number, 
						Initial.rwd.cost = Initial.costVal, 
						Incremental.rwd.cost = Incremental.costVal, 
						Final.rwd.cost = Final.costVal,
						Initial.rwd.effectiveness = Initial.effectivenessVal, 
						Incremental.rwd.effectiveness = Incremental.effectivenessVal, 
						Final.rwd.effectiveness = Final.effectivenessVal)
	 				refreshF5()
					tkfocus(tt)
				} else {
					msg <- paste("Os valores definidos não são válidos.")
					tkmessageBox(message = msg, icon="error", title = "ÁrvoRe - AVISO")
					tkfocus(markovnodeWindow)
				}
			}
			
		  	.Width.but <- 10
			.Height.but <- 1
			
			OK.but <-tkbutton(markovnodeWindow, width=.Width.but, height=.Height.but,text="OK",command=OnOK)
			tkbind(markovnodeWindow, "<Return>",OnOK)
			
			OnCancel <- function()
			{
				tkdestroy(markovnodeWindow)
				tkfocus(tt)
			}
			
			Cancel.but <-tkbutton(markovnodeWindow, width=.Width.but, height=.Height.but, text="Cancel", command=OnCancel)
			
			tkbind(markovnodeWindow, "<Escape>",OnCancel)
	
			tkgrid(OK.but, Cancel.but, sticky = "s", padx = 5, pady = 5)
	
			tkfocus(markovnodeWindow)
# 			posiciona.janela.no.mouse(markovnodeWindow, 230, 280)
		} else {
			msg <- paste("O nodo selecionado não é ramificação de um nodo Markov \n ou é de tipo inválido.")
			tkmessageBox(message = msg, icon="warning", title = "ÁrvoRe - AVISO")
			tkfocus(tt)
		}
	}

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
