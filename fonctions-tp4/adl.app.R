adl.app <- function(Xapp, zapp){
	g <- max(zapp); # The number of classes
	parameters <- list();
	n <- dim(Xk)[1];
	p <- dim(Xk)[2];
	sumVk <- matrix(0, nrow=p, ncol=p);
	for (k in 1:g){
		# K class management
		Xk <- Xapp[which( zapp==k ), ]; # We get the indivuals of this class
		nk <- dim(Xk)[1]; # Number of individuals of this classn

		pik <- nk/n; # Proportion of individuals in this class
		muk <- apply(Xk, 2, mean); # Estimation of mu for this class
		sumVk <- sumVk + (nk - 1) * cov.wt(Xk, method='unbiased');
		# Here, we store the parameters of the k class
		classK <- list()
		classK$class <- k;
		classK$nk <- nk;
		classK$pik <- pik;
		classK$muk <- muk;
		parameters[[k+1]] <- classK;
	}
	parameters[[1]] <- sumVk/(n-g);
	parameters;
}