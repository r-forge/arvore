`exec.text` <-
function(x) {
	ans <- try( eval(parse(text = x)) )
	return(ans)
}

