# Optimisable pour éviter la boucle sum
nba.app <- function(Xapp, zapp){
	g <- max(zapp); # The number of classes
	parameters <- list();
	n <- dim(Xk)[1];
	temptotsigma <- 0;
	for (k in 1:g){
		# K class management
		Xk <- Xapp[which( zapp==k ), ]; # We get the indivuals of this class
		nk <- dim(Xk)[1]; # Number of individuals of this classn

		pik <- nk/n; # Proportion of individuals in this class
		muk <- apply(Xk, 2, mean); # Estimation of mu for this class
		temptotsigma <- 0;
		for (i in 1:nk){
			temptotsigma <- temptotsigma + (Xk[i] - muk) %*% t(Xk[i] - muk) 
		}
		sigmak <- (1/nk) * temptotsigma # Estimation of Sigma for this class
		# Here, we store the parameters of the k class
		classK <- list()
		classK$class <- k;
		classK$nk <- nk;
		classK$pik <- pik;
		classK$muk <- muk;
		classK$sigmak <- sigmak;
		parameters[[k]] <- classK;
	}
	parameters;
}