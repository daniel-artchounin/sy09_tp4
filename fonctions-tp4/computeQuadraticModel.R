computeQuadraticModel <- function(X){
	n <- dim(X)[1];
	p <- dim(X)[2];
	XNew <- matrix(0, n, (p*(p+1))/2);
	iterator <- 0;
	for(i in 1:(p-1)){
		for(j in (i+1):p){
			iterator <- iterator + 1;
			XNew[, iterator] <- X[, i] * X[, j]; 
		}
	}
	for(i in 1:p){
		iterator <- iterator + 1;
		XNew[, iterator] <- X[, i] * X[, i]; 
	}
 	cbind(X, XNew);
}