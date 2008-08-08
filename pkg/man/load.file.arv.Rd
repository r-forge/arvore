\name{load.file.arv}
\alias{load.file.arv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load.file.arv(...)
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
	if (.workstatus != "saved") {
		ans <- tkmessageBox(message="Deseja salvar a árvore atual?",icon="question",type="yesnocancel",default="yes")
		ans <- tclvalue(ans)
		if (ans == "yes") {
			save.file.arv()
		}
	}
	fileName <- tclvalue(tkgetOpenFile(filetypes="{{ArvoRe Files} {.arv}} {{All files} *}"))
	if (!nchar(fileName))
		tkfocus(tt)
	else {
		clearTreeTkArvore(TheTree)
		load(fileName, envir = .EnvironmentArvoRe)
		load(fileName)
		theTreeTkArvore(TheTree)
		atualiza.grafico()
	}	
	assign(".workstatus", "saved", .EnvironmentArvoRe)
	assign(".opennedfile", fileName, .EnvironmentArvoRe)
	titletext <- paste("ÁrvoRe - Janela Principal - [", fileName, "]", sep = "")
	tkwm.title(tt, titletext)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
