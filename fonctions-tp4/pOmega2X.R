pOmega2X <- function(X, omega){
	n <- dim(X)[1];
	omega <- omega;
	matrix(1, n, 1) / (matrix(1, n, 1) + exp(X %*% omega));
}