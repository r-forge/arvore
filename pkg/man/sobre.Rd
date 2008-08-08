\name{sobre}
\alias{sobre}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sobre(versionarvore, versiondate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{versionarvore}{ ~~Describe \code{versionarvore} here~~ }
  \item{versiondate}{ ~~Describe \code{versiondate} here~~ }
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
function(versionarvore, versiondate) {
	.Mensagem <- paste(
" ________________________________________ \n\n",
" ArvoRe - Análise de Custo Efetividade no R              \n",
" (Simulação de primeira ordem MCMC)                      \n\n",
paste("Versão : ", versionarvore, "                                     \n", sep=""),
paste("Versão : ", versiondate, "                                     \n", sep=""),
" ________________________________________ \n\n",
" Autor:                                                  \n",
" Isaías V. Prestes                                       \n",
" IM - Departamento de Estatística                        \n",
" Universidade Federal do Rio Grande do Sul,              \n",
" Av. Bento Gonçalves, 9500, Porto Alegre, Brasil         \n",
" E-mail: isaias.prestes@ufrgs.br                         \n",
" URL: http://www.mat.ufrgs.br/~camey/                    \n",
" ________________________________________ \n",
" \n", sep = "")
	sobre.wm.title <- "Sobre o Programa"
	ReturnVal <- tkmessageBox(title = sobre.wm.title,
    message = .Mensagem, icon = "info", type = "ok")
    tkfocus(tt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
