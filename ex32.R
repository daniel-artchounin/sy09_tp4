library(tree);
library(sm) # Chargement de sm


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
source('./fonctions-tp4/drawScreePlot.R');
source('./fonctions-tp4/drawCumulativeScreePlot.R');


alpha <- 0.05;
alphaDivBy2 <- alpha/2;
oneMinusAlphaDivBy2 <- 1 - alphaDivBy2
methods <- c("adl", "logWithIntercept", "binTree");
results <- list();
i <- 1;
N <- 30;
XAll <- NULL;
zAll <- NULL;
iterator <- 0;
errorRatesList <- NULL;

print('yo'); # Test

Donn <- read.csv("./donnees-tp4/spam.csv", header=T);
X <- Donn[, 2:58]
z <- Donn[, 59]

print("*** spam data set ***");
print("X summary:")
print(summary(X))
print("z summary:")
print(table(z))
print("Data set size:")
print(dim(X)[1])
print("Number of explicative variables:")
print(dim(X)[2])


# print(z) # Test

print('yo'); # Test

tmpList <- NULL
adqTmpList <- NULL
adlTmpList <- NULL
nbaTmpList <- NULL
errorRatesList <- NULL;
adlErrorRatesTst <- c();
logWithoutInterceptErrorRatesTst <- c();
logWithInterceptErrorRatesTst <- c();
binTreeErrorRatesTst <- c();


for(j in 1:N){
	donn.sep <- separ1(X, z);
	Xapp <- donn.sep$Xapp;
	zapp <- donn.sep$zapp;
	Xtst <- donn.sep$Xtst;
	ztst <- donn.sep$ztst;
	
	# Parameters of the models
	print('yo'); # Test
	paramsAppAdl <- adl.app(Xapp, zapp);
	paramsAppLogWithIntercept <- log.app(Xapp, zapp, TRUE, 1e-5, TRUE);
	binTree <- tree(factor(zapp) ~ ., data=as.data.frame(cbind(Xapp, zapp)), control=tree.control(nobs=dim( Xapp )[1], mindev = 0.0001));
	cvModel <- cv.tree(binTree);
	bestSize <- cvModel$size[which(cvModel$dev==min(cvModel$dev))];
	binTree2 <- prune.misclass(binTree, best=bestSize[length(bestSize)]);

	print('yo'); # Test
	predictionsAdl <- ad.val(paramsAppAdl, Xtst)$predictions;
	predictionsLogWithIntercept <- log.val(paramsAppLogWithIntercept$beta, Xtst)$predictions;	
	predictionsBinTree <- predict(binTree2, as.data.frame(Xtst), type = "class");

	print('yo'); # Test
	adlErrorRatesTst[j] <- errorRate(predictionsAdl, ztst);
	logWithInterceptErrorRatesTst[j] <- errorRate(predictionsLogWithIntercept, ztst);
	binTreeErrorRatesTst[j] <- errorRate(predictionsBinTree, ztst);
	print('yo'); # Test

	if(j == 1){
		imageName <- paste ("images/ex2/spam_bin_tree.pdf", sep="");
		pdf(file = imageName);
		plot(binTree2);
		text(binTree2, cex=0.75);
		dev.off();
	}
}

errorRatesList[[1]] <- adlErrorRatesTst;
errorRatesList[[2]] <- logWithInterceptErrorRatesTst;
errorRatesList[[3]] <- binTreeErrorRatesTst;

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


# -> Results
# $log$estimatorErrorRateTst
# [1] 0.08323549