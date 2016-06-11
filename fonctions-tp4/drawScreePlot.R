drawScreePlot <- function(princompResult, title, cex=1, round=2){
	eigenValues <- princompResult^2
	eigenValuesSum <- sum(eigenValues)
	n <- length(eigenValues)
	xLab <- as.vector(interaction('U',1:n, sep=""))
	ones <- matrix(1, nrow=1, ncol=n)
	eigenValuesSum <- eigenValuesSum %*% ones
	screePlotData <- (eigenValues / eigenValuesSum) * 100
	brplt <- barplot(screePlotData, main=title, names.arg=xLab, ylim=c(0, 100), ylab= "Pourcentage d'inertie expliqué par chaque axe")
	text(x=brplt, y=screePlotData+3, labels=as.character(round(screePlotData, round)), xpd=TRUE, cex=cex)
}
