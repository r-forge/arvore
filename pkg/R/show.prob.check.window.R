`show.prob.check.window` <-
function(TheTree) {
		msg <- probability.check(TheTree)
		icon="error"
		if (msg[2] == "0") {
			icon="warning"
		}
		tkmessageBox(title = "�rvoRe - Verifica��o das Probabilidades", message=msg[1], icon = icon, type = "ok")
		tkfocus(tt)
}

