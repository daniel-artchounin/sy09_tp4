estimationOfMusAndSigmas <- function(X, z){
	# print(X); # Test
	# print(z); # Test
	n <- dim(X)[1]; # Size of data set
	g <- max(z); # The number of classes
	results <- list();
	for (k in 1:g){
		# K class management
		Xk <- X[which( z == k ), ]; # We get the indivuals of this class
		nk <- dim(Xk)[1]; # Number of individuals of this class
		pik <- nk/n; # Proportion of individuals in this class
		muk <- apply(Xk, 2, mean); # Estimation of mu for this class
		sigmak <- cov.wt(Xk, method='ML'); # Estimation of Sigma for this class
		# print(pik); # Test
		# print(muk); # Test
		# print(sigmak); # Test

		# Here, we store the results of the k class
		XkResults <- list()
		XkResults$class <- k;
		XkResults$nk <- nk;
		XkResults$pik <- pik;
		XkResults$muk <- muk;
		XkResults$sigmak <- sigmak;
		results <- c(results, XkResults);
	}
	results;
}