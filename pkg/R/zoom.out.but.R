`zoom.out.but` <-
function(imgHeight) {
	if (imgHeight >= 320) {
		imgHeight <- round(imgHeight / 1.1, digits = 0)
		imgWidth <- round((4/3) * imgHeight, digits = 0)
		set.zoom.image.tree(imgHeight, imgWidth)
	}
	refreshF5()
}

