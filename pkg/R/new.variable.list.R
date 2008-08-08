`new.variable.list` <-
function() {
	Variable <- array(" ",0)
	StdValue <- array(0,0)
	MinValue <- array(0,0)
	MaxValue <- array(0,0)
	Notes <- array(" ",0)
	
	ans <- data.frame("Name" = Variable, "Fix.Value" = StdValue, "Min.Value" = MinValue, 
						"Max.Value" = MaxValue,	"Notes" = Notes)
	ans <- as.data.frame(ans)
	assign("variableMAT", ans, .EnvironmentArvoRe)
}

