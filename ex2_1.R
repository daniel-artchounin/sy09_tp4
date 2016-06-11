library(tree);
library(MASS);

source('./fonctions-tp4/getData.R');
source('./fonctions-tp4/ad.val.R');
source('./fonctions-tp4/adl.app.R');
source('./fonctions-tp4/adq.app.R');
source('./fonctions-tp4/nba.app.R');
source('./fonctions-tp4/separ1.R');
source('./fonctions-tp4/mvdnorm.r');
source('./fonctions-tp4/errorRate.R');
source('./fonctions-tp4/log.app.R');
source('./fonctions-tp4/log.val.R');
source('./fonctions-tp4/log.quad.app.R');
source('./fonctions-tp4/log.quad.val.R');
source('./fonctions-tp4/cleanZ.R');
source('./fonctions-tp4/pOmega1X.R');
source('./fonctions-tp4/pOmega2X.R');
source('./fonctions-tp4/post.pr.R');
source('./fonctions-tp4/estimationOfMusAndSigmas.R');
source('./fonctions-tp4/prob.ad.R');
source('./fonctions-tp4/prob.log.R');
source('./fonctions-tp4/prob.log2.R');

alpha <- 0.05;
alphaDivBy2 <- alpha/2;
oneMinusAlphaDivBy2 <- 1 - alphaDivBy2
filesNames <- c("donnees-tp4/Synth1-1000.txt", "donnees-tp4/Synth2-1000.txt", "donnees-tp4/Synth3-1000.txt");
methods <- c("adq", "adl", "nba", "logWithoutIntercept", "logWithIntercept", "quadLog", "binTree");
results <- list();
i <- 1;
N <- 20;
iterator <- 0;
errorRatesList <- NULL;
for(fileName in filesNames){
	print(fileName);
	data <- getData(fileName);
	X <- data[[1]];
	z <- data[[2]];

	print("*** Synthi data set ***");
	print("X summary:")
	print(summary(X))
	print("z summary:")
	print(table(z))
	print("Data set size:")
	print(dim(X)[1])
	print("Number of explicative variables:")
	print(dim(X)[2])
	
	# Plot of each dataset
	imageName <- paste ("images/ex2/synth_", i, "_1000.pdf", sep="");
	pdf(file = imageName);
	plot(X, col=c("darkorchid","firebrick","darkcyan","darkolivegreen","orange")[z]);
	dev.off();	

	tmpList <- NULL
	adqTmpList <- NULL
	adlTmpList <- NULL
	nbaTmpList <- NULL
	errorRatesList <- NULL;
	errorRates <- c()
	adqErrorRatesTst <- c();
	adlErrorRatesTst <- c();
	nbaErrorRatesTst <- c();
	logWithoutInterceptErrorRatesTst <- c();
	logWithInterceptErrorRatesTst <- c();
	quadLogErrorRatesTst <- c();
	binTreeErrorRatesTst <- c();
	for(j in 1:N){
		donn.sep <- separ1(X, z);
		Xapp <- donn.sep$Xapp;
		zapp <- donn.sep$zapp;
		Xtst <- donn.sep$Xtst;
		ztst <- donn.sep$ztst;

		# Parameters of the models
		paramsAppAdq <- adq.app(Xapp, zapp);
		paramsAppAdl <- adl.app(Xapp, zapp);
		paramsAppNba <- nba.app(Xapp, zapp);
		paramsAppLogWithoutIntercept <- log.app(Xapp, zapp, FALSE, 1e-5);
		paramsAppLogWithIntercept <- log.app(Xapp, zapp, TRUE, 1e-5);
		paramsAppQuadLog <- log.quad.app(Xapp, zapp, 1e-5);
		
		binTree <- tree(factor(zapp) ~ ., data=cbind(Xapp, zapp), control=tree.control(nobs=dim( Xapp )[1], mindev = 0.0001));
		cvModel <- cv.tree(binTree);
		bestSize <- cvModel$size[which(cvModel$dev==min(cvModel$dev))];
		binTree2 <- prune.misclass(binTree, best=bestSize[length(bestSize)]);

		predictionsAdq <- ad.val(paramsAppAdq, Xtst)$predictions;
		predictionsAdl <- ad.val(paramsAppAdl, Xtst)$predictions;
		predictionsNba <- ad.val(paramsAppNba, Xtst)$predictions;
		predictionsLogWithoutIntercept <- log.val(paramsAppLogWithoutIntercept$beta, Xtst)$predictions;
		predictionsLogWithIntercept <- log.val(paramsAppLogWithIntercept$beta, Xtst)$predictions;
		predictionsQuadLog <- log.quad.val(paramsAppQuadLog$beta, Xtst)$predictions;
		predictionsBinTree <- predict(binTree2, Xtst, type = "class");
		
		adqErrorRatesTst[j] <- errorRate(predictionsAdq, ztst);
		adlErrorRatesTst[j] <- errorRate(predictionsAdl, ztst);
		nbaErrorRatesTst[j] <- errorRate(predictionsNba, ztst);
		logWithoutInterceptErrorRatesTst[j] <- errorRate(predictionsLogWithoutIntercept, ztst);
		logWithInterceptErrorRatesTst[j] <- errorRate(predictionsLogWithIntercept, ztst);
		quadLogErrorRatesTst[j] <- errorRate(predictionsQuadLog, ztst)
		binTreeErrorRatesTst[j] <- errorRate(predictionsBinTree, ztst);
		if(j == 1){
			imageName <- paste ("images/ex2/synth_", i, "_adq_1000.pdf", sep="");
			pdf(file = imageName);
			prob.ad(paramsAppAdq, Xtst, ztst, c(0.5));
			dev.off();	

			imageName <- paste ("images/ex2/synth_", i, "_adl_1000.pdf", sep="");
			pdf(file = imageName);
			prob.ad(paramsAppAdl, Xtst, ztst, c(0.5));
			dev.off();	

			imageName <- paste ("images/ex2/synth_", i, "_nba_1000.pdf", sep="");
			pdf(file = imageName);
			prob.ad(paramsAppNba, Xtst, ztst, c(0.5));
			dev.off();	

			imageName <- paste ("images/ex2/synth_", i, "_log_without_intercept_1000.pdf", sep="");
			pdf(file = imageName);
			prob.log(paramsAppLogWithoutIntercept$beta, Xtst, ztst, c(0.5));
			dev.off();

			imageName <- paste ("images/ex2/synth_", i, "_log_with_intercept_1000.pdf", sep="");
			pdf(file = imageName);
			prob.log(paramsAppLogWithIntercept$beta, Xtst, ztst, c(0.5));
			dev.off();

			imageName <- paste ("images/ex2/synth_", i, "_quad_log_1000.pdf", sep="");
			pdf(file = imageName);
			prob.log2(paramsAppQuadLog$beta, Xtst, ztst, c(0.5));
			dev.off();

			imageName <- paste ("images/ex2/synth_", i, "_bin_tree_1000.pdf", sep="");
			pdf(file = imageName);
			plot(binTree2);
			text(binTree2, cex=0.75);
			dev.off();

			imageName <- paste ("images/ex2/synth_", i, "_bin_partition_1000.pdf", sep="");
			pdf(file = imageName);
			plot(Xtst, col=c("darkorchid","firebrick","darkcyan","darkolivegreen","orange")[ztst])
			partition.tree(binTree2, ordvars=c("V1","V2"), add=TRUE)			
			dev.off();
		}
	}
	errorRatesList[[1]] <- adqErrorRatesTst;
	errorRatesList[[2]] <- adlErrorRatesTst;
	errorRatesList[[3]] <- nbaErrorRatesTst;
	errorRatesList[[4]] <- logWithoutInterceptErrorRatesTst;
	errorRatesList[[5]] <- logWithInterceptErrorRatesTst;
	errorRatesList[[6]] <- quadLogErrorRatesTst;
	errorRatesList[[7]] <- binTreeErrorRatesTst;
	iterator <- 0;
	results[[i]] <- list();
	results[[i]]$param <- estimationOfMusAndSigmas(X, z);
	for(methodName in methods){
		iterator <- iterator + 1;
		cILengthDivByTwoTst <- (qnorm(oneMinusAlphaDivBy2)* sd(errorRatesList[[iterator]]))/sqrt(N);	
		tmpList$errorRatesTst <- errorRatesList[[iterator]]
		tmpList$estimatorErrorRateTst <- mean(errorRatesList[[iterator]]);methodName
		tmpList$cILengthTst <- cILengthDivByTwoTst + cILengthDivByTwoTst;
		tmpList$cILeftBoundTst <- tmpList$estimatorErrorRateTst - cILengthDivByTwoTst;
		tmpList$cIRightBoundTst <- tmpList$estimatorErrorRateTst + cILengthDivByTwoTst;
		results[[i]][[methodName]] <- tmpList;
	}
	i <- i + 1;
}
print(results); # Test