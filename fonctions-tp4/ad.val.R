# source('./mvdnorm.r')

ad.val <- function(parameters, Xtst){
	n <- dim(Xtst)[1];
	g <- length(parameters);
	densities <- list();
	sumDensities <- rep(0, n);
	aPosterioriProbabilities <- NULL;
	for(k in 1:g){
		densities[[k]] <- mvdnorm(Xtst, parameters[[k]]$muk, parameters[[k]]$sigmak);
		sumDensities <- sumDensities + densities[[k]];
	}
	for(k in 1:g){
		if(k == 1){
			aPosterioriProbabilities <- as.matrix((parameters[[k]]$pik * densities[[k]])/sumDensities);
		}else{
			aPosterioriProbabilities <- cbind(aPosterioriProbabilities, as.matrix((parameters[[k]]$pik * densities[[k]])/sumDensities));
		}
	}
	predictions <- apply(aPosterioriProbabilities, 1, which.max);
	probas <- apply(aPosterioriProbabilities, 1, max);
	predictionsList <- list();
	predictionsList$prob <- cbind(probas, as.matrix(predictions));
	# print(predictionsList$prob)
	# predictionsList;
}

# source('./adq.app.R')
# source('./adl.app.R')
# source('./nba.app.R')
# data <- read.table('../donnees-tp4/yo_100.txt', header=F);
# X <- data[, 1:2];
# z <- data[, 3];
# plot(ad.val(adq.app(X, z), X))