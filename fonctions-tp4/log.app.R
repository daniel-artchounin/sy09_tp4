library(MASS)

log.app  <- function(Xapp, zapp, intr, epsi){
	beta <- NULL; # Estimation of parameters 			
	niter <- 0; # Number of iterations
	logL <- NULL; # Log vraisemblance
	Xapp <- as.matrix(Xapp);
	n <- dim(Xapp)[1]; # Number of individuals
	p <- dim(Xapp)[2]; # Number of caracteristics
	t <- cleanZ(zapp);	
	minusH <- NULL;
	betaNew <- matrix(0, p, 1);
	betaOld <- matrix(1, p, 1);
	if(intr){ # We should add an intercept
		Xapp <-cbind(matrix(1, n, 1), Xapp);
		betaOld <- matrix(0, p+1, 1);
		betaNew <- matrix(1, p+1, 1);
	}	
	while(norm(betaNew - betaOld, type="2") > epsi){
		niter <- niter + 1;
		betaOld <- betaNew;
		# print('no') # Test
		# print(betaOld) # Test
		# print(pOmega1X(Xapp, betaOld)) # Test
		# print('no') # Test
		minusH <- t(Xapp) %*% diag( diag( pOmega1X(Xapp, betaOld) %*% t(pOmega2X(Xapp, betaOld)) ) )  %*% Xapp;
		betaNew <- betaOld + solve(minusH) %*% t(Xapp) %*% (t - pOmega1X(Xapp, betaOld));
		# betaNew <- betaOld + ginv(minusH) %*% t(Xapp) %*% (t - pOmega1X(Xapp, betaOld)); # Test
		print(norm(betaNew - betaOld, type="2")); # Test
	}
	results <- list()
	results$beta <- betaNew;
	results$niter <- niter;
	results$logL <- t(Xapp) %*% (t - pOmega1X(Xapp, betaNew));
	results;
}