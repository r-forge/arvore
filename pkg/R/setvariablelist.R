`setvariablelist` <-
function(variableMAT, newvariableline = " ", variable.name = " ", action = "edit") {
	if (action == "delete") {
		variables <- names(variableMAT)
		ans <- subset(variableMAT, Name != variable.name, select = variables)
	}
	if (action == "add") {
		require(abind)
		ans <- abind(variableMAT, newvariableline, along=1)
	}
	if (action == "edit") {
		variables <- names(variableMAT)
		ans <- subset(variableMAT, Name != variable.name, select = variables)
		
		require(abind)
		ans <- abind(ans, newvariableline, along=1)
	}

	ans <- as.data.frame(ans)
	ans$Name <- as.character(ans$Name)
	ans$Fix.Value <- as.numeric(as.character(ans$Fix.Value))
	ans$Min.Value <- as.numeric(as.character(ans$Min.Value))
	ans$Max.Value <- as.numeric(as.character(ans$Max.Value))
	ans$Notes <- as.character(ans$Notes)
	assign("variableMAT", ans, envir = .EnvironmentArvoRe)
	assign(".workstatus", "unsaved", .EnvironmentArvoRe)
	
}

