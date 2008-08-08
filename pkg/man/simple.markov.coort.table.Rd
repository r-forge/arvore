\name{simple.markov.coort.table}
\alias{simple.markov.coort.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simple.markov.coort.table(TheTree, trials = 10000, seed = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TheTree}{ ~~Describe \code{TheTree} here~~ }
  \item{trials}{ ~~Describe \code{trials} here~~ }
  \item{seed}{ ~~Describe \code{seed} here~~ }
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
function(TheTree, trials = 10000, seed = FALSE) {
	# ajusta a semente escolhida pelo usuário
	if (seed != FALSE) {
		set.seed(seed)	
	}
	
	# Convert the tree to matrix format
	MatrixTheTree <- convert2matrix(TheTree)
# 	print(MatrixTheTree)
	x <- MatrixTheTree$x								# Structure matrix
	y <- MatrixTheTree$y								# Node name matrix
	typeMAT <- MatrixTheTree$typeMAT					# Node type matrix
	utilityMAT <- MatrixTheTree$utilityMAT				# Node Cost matrix 
	effectivenessMAT <- MatrixTheTree$effectivenessMAT	# Node effectiveness matrix
	probMAT <- MatrixTheTree$probMAT					# Node probability matrix

	num.col.x <- dim(x)[2]
	num.lin.x <- dim(x)[1]

	probMAT[,1] <- 1.0	# Agora o nodo raiz recebe prob = 1.
	typeMAT[,1] <- "D"	# Agora o nodo raiz recebe "D".
	
	# ajusta elementos para matriz... pois com vetor não funciona
# 	utilityMAT <- matrix(utilityMAT, num.lin.x, num.col.x)
# 	effectivenessMAT <- matrix(utilityMAT, num.lin.x, num.col.x)
# 	probMAT <- matrix(utilityMAT, num.lin.x, num.col.x)
	
	# ajusta custo e efetividade: serão acumulados através dos nodos.
	if (num.lin.x > 1) {
		utilityMAT <- apply(utilityMAT, 1, sum)
		effectivenessMAT <- apply(effectivenessMAT, 1, sum)
	} else {
		utilityMAT <- sum(utilityMAT)
		effectivenessMAT <- sum(effectivenessMAT)
	}
	# cria a tabela que comportará os individuos
# 	Coorte.Ind <- matrix(0, 1, trials)	# Matriz com cada individuo
# 	Coorte.Cost <- matrix(0, 1, trials)	# Matriz com custo de cada individuo
# 	Coorte.Effec <- matrix(0, 1, trials)	# Matriz com a efetividade de cada individuo
	
	# A simulação em si. Choose your destiny!
	sorteado <- runif(trials,0,1)
	linprobs <- cumsum(apply(probMAT, 1, prod)) # observa a probabilidade de cada ramo acontecer numa runif
	valn <- length(linprobs)
	linprobs.Matrix <- matrix(linprobs, trials, valn, byrow = TRUE) # podemos ter problema de memória aqui!!!
	resultado <- valn - apply(sorteado <= linprobs.Matrix, 1, sum) + 1
#  	ans.dest <- destinos[resultado]	# quantos vão para cada categoria
	ans.cost <- utilityMAT[resultado]
	ans.effectiveness <- effectivenessMAT[resultado]
	
 	Coorte.Ind <- matrix(resultado, 1, trials)				# Matriz com cada individuo
	Coorte.Cost <- matrix(ans.cost, 1, trials)				# Matriz com custo de cada individuo
	Coorte.Effec <- matrix(ans.effectiveness, 1, trials)	# Matriz com a efetividade de cada individuo
	
	ans <- list(Path = Coorte.Ind, Cost = Coorte.Cost, Effectiveness = Coorte.Effec)
	return(ans)	# And return the result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
