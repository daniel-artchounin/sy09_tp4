getData <- function (fileName){
	data <- read.table(fileName, header=T); # Here, we get the data
	X <- data[, 1:2]; # Attributes
	z <- data[, 3]; # Labels
	results <- list();
	results[[1]] <- X;
	results[[2]] <- z;
	results;
}