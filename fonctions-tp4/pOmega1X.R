pOmega1X <- function(X, omega){
	n <- dim(X)[1];
	omega <- omega;
	exp(X %*% omega) / matrix(1, n, 1) + exp(X %*% omega);
}