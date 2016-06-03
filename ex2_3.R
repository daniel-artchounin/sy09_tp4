library(tree)


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


# Should we also estimate pik, muk and sigmak ???


alpha <- 0.05;
alphaDivBy2 <- alpha/2;
oneMinusAlphaDivBy2 <- 1 - alphaDivBy2
methods <- c("adq", "adl", "nba", "log", "binTree");
results <- list();
i <- 1;
N <- 100;
iterator <- 0;
errorRatesList <- NULL;

Donn <- read.csv("./donnees-tp4/bcw.csv", header=T);
X <- Donn[,1:9];
z <- Donn[,10];
tmpList <- NULL
adqTmpList <- NULL
adlTmpList <- NULL
nbaTmpList <- NULL
errorRatesList <- NULL;
errorRates <- c()
adqErrorRatesTst <- c();
adlErrorRatesTst <- c();
nbaErrorRatesTst <- c();
logErrorRatesTst <- c();
binTreeErrorRatesTst <- c();

# Plot of the dataset
pdf(file = "images/ex2/bcw.pdf");
plot(X, col=c("darkorchid","firebrick","darkcyan","darkolivegreen","orange","black","red")[z]);
dev.off();

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
	paramsAppLog <- log.app(Xapp, zapp, TRUE, 1e-5);
	binTree <- tree(factor(zapp) ~ ., data=cbind(Xapp, zapp), control=tree.control(nobs=dim( Xapp )[1], mindev = 0.0001));
	cvModel <- cv.tree(binTree);
	bestSize <- cvModel$size[which(cvModel$dev==min(cvModel$dev))];
	binTree2 <- prune.misclass(binTree, best=bestSize[length(bestSize)]);

	predictionsAdq <- ad.val(paramsAppAdq, Xtst)$predictions;
	predictionsAdl <- ad.val(paramsAppAdl, Xtst)$predictions;
	predictionsNba <- ad.val(paramsAppNba, Xtst)$predictions;
	predictionsLog <- log.val(paramsAppLog$beta, Xtst)$predictions;
	predictionsBinTree <- predict(binTree2, Xtst, type = "class");

	adqErrorRatesTst[j] <- errorRate(predictionsAdq, ztst);
	adlErrorRatesTst[j] <- errorRate(predictionsAdl, ztst);
	nbaErrorRatesTst[j] <- errorRate(predictionsNba, ztst);
	logErrorRatesTst[j] <- errorRate(predictionsLog, ztst);
	binTreeErrorRatesTst[j] <- errorRate(predictionsBinTree, ztst);

	if(j == 1){
		imageName <- paste ("images/ex2/bcw_bin_tree.pdf", sep="");
		pdf(file = imageName);
		plot(binTree2);
		text(binTree2, cex=0.75);
		dev.off();
	}
}
errorRatesList[[1]] <- adqErrorRatesTst;
errorRatesList[[2]] <- adlErrorRatesTst;
errorRatesList[[3]] <- nbaErrorRatesTst;
errorRatesList[[4]] <- logErrorRatesTst;
errorRatesList[[5]] <- binTreeErrorRatesTst;
iterator <- 0;
for(methodName in methods){
	iterator <- iterator + 1;
	cILengthDivByTwoTst <- (qnorm(oneMinusAlphaDivBy2)* sd(errorRatesList[[iterator]]))/sqrt(N);	
	tmpList$errorRatesTst <- errorRatesList[[iterator]]
	tmpList$estimatorErrorRateTst <- mean(errorRatesList[[iterator]]);methodName
	tmpList$cILengthTst <- cILengthDivByTwoTst + cILengthDivByTwoTst;
	tmpList$cILeftBoundTst <- tmpList$estimatorErrorRateTst - cILengthDivByTwoTst;
	tmpList$cIRightBoundTst <- tmpList$estimatorErrorRateTst + cILengthDivByTwoTst;
	results[[methodName]] <- tmpList;
}
print(results);