\name{theTreeTkArvore}
\alias{theTreeTkArvore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
theTreeTkArvore(TheTree)
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
	
	num.lin <- dim(TheTree)[1]
	num.levels <- max(TheTree$Level)
	
	for (i in 1:length(.libPaths())) {
		SubDataSet <- subset(TheTree, Level == 1)
		osnodos <- SubDataSet$Node.N
		osnodosnomes <- SubDataSet$Node.name
		osnodostipos <- SubDataSet$Type
		osnodos <- paste(i,".",osnodos,sep="")

		icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/", osnodostipos,".png",sep=""))
		if (file.exists(icon.but)) {
			icn <- tkimage.create("photo", file=icon.but)
			tkinsert(treeWidget,"end","root","1.1",text=osnodosnomes, image = icn)
		} else {
			tkinsert(treeWidget,"end","root","1.1",text=osnodosnomes)
		}
	}
	
	if (num.lin > 1) {
		for (i in 2:num.levels) {
			SubDataSet <- subset(TheTree, Level == i)
			osnodos <- SubDataSet$Node.N
			paisnodos <- SubDataSet$Father
			osnodosnomes <- SubDataSet$Node.name
			osnodostipos <- SubDataSet$Type
# 	  		cat("DEBUG : Criei os nodos \n ", osnodos, " cujos pais são ", paisnodos, "\n")
				
			osnodos <- paste(i,".",osnodos,sep="")
			paisnodos <- paste((i-1),".",paisnodos,sep="")
				
			for (j in 1:length(osnodos)) {
					tipofilename <- osnodostipos[j]
					for (i in 1:length(.libPaths())) {
						icon.but <- file.path(paste(.libPaths()[i],"/arvoRe/icons/", tipofilename,".png",sep=""))
						if (file.exists(icon.but)) {
							icn <- tkimage.create("photo", file=icon.but)
							tkinsert(treeWidget,"end",paisnodos[j],osnodos[j],text=osnodosnomes[j], image = icn)
						} else {
							tkinsert(treeWidget,"end",paisnodos[j],osnodos[j],text=osnodosnomes[j])
						}
					}
			}
		}
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
