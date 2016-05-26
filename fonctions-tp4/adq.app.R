adq.app <- function(Xapp, zapp){
	g <- max(zapp); # The number of classes
	parameters <- list();
	n <- dim(Xapp)[1];
	for (k in 1:g){
		# K class management
		Xk <- Xapp[which( zapp==k ), ]; # We get the indivuals of this class
		nk <- dim(Xk)[1]; # Number of individuals of this classn

		pik <- nk/n; # Proportion of individuals in this class
		muk <- apply(Xk, 2, mean); # Estimation of mu for this class
		sigmak <- cov.wt(Xk, method='ML')$cov; # Estimation of Sigma for this class

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