# source('./mvdnorm.r')

ad.val <- function(parameters, Xtst){
	n <- dim(Xtst)[1];
	g <- length(parameters);
	densities <- list();
	sumDensities <- rep(0, n);
	aPosterioriProbabilities <- NULL;
	for(k in 1:g){
		densities[[k]] <- mvdnorm(Xtst, parameters[[k]]$muk, parameters[[k]]$sigmak);
		sumDensities <- sumDensities + parameters[[k]]$pik * densities[[k]];
	}
	for(k in 1:g){
		if(k == 1){
			aPosterioriProbabilities <- as.matrix((parameters[[k]]$pik * densities[[k]])/sumDensities);
		}else{
			aPosterioriProbabilities <- cbind(aPosterioriProbabilities, as.matrix((parameters[[k]]$pik * densities[[k]])/sumDensities));
		}
	}
	predictions <- apply(aPosterioriProbabilities, 1, which.max);
	predictionsList <- list();
	predictionsList$prob <- aPosterioriProbabilities;
	predictionsList$predictions <- predictions;
	predictionsList;
}