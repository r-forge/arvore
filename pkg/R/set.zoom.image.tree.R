`set.zoom.image.tree` <-
function(imgHeight, imgWidth, scalarfac = 1) {
	imgHeight <- imgHeight * scalarfac
	imgWidth <- imgWidth * scalarfac
	assign("imgHeight", imgHeight, .EnvironmentArvoRe)
	assign("imgWidth", imgWidth, .EnvironmentArvoRe)
}

