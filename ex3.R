library(tree);


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
methods <- c("adq", "adl", "nba", "logWithoutIntercept", "logWithIntercept", "quadLog", "binTree");
results <- list();
i <- 1;
N <- 10;
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
adqErrorRatesTst <- c();
adlErrorRatesTst <- c();
nbaErrorRatesTst <- c();
logWithoutInterceptErrorRatesTst <- c();
logWithInterceptErrorRatesTst <- c();
quadLogErrorRatesTst <- c();
binTreeErrorRatesTst <- c();

res <- princomp(X) # Calcul de l'ACP
print(summary(res))
# print((res$sdev)^2) # L
# print(res$loadings) # Vecteurs propres
# print(res$scores) # Composantes
# print(res$sdev) # Valeurs propres

X <- res$scores[, 1:35];

print("*** spam data set (after PCA) ***");
print("X summary:")
print(summary(X))
print("z summary:")
print(table(z))
print("Data set size:")
print(dim(X)[1])
print("Number of explicative variables:")
print(dim(X)[2])
	
# Diagramme en bâtons des valeurs propres
pdf("./images/ex2/spam_batons_valeurs_propres.pdf");
plot(res, main='Diagramme en bâtons des valeurs propres');
dev.off()

for(j in 1:N){
	donn.sep <- separ1(X, z);
	Xapp <- donn.sep$Xapp;
	zapp <- donn.sep$zapp;
	Xtst <- donn.sep$Xtst;
	ztst <- donn.sep$ztst;
	
	# Parameters of the models
	print('yo'); # Test
	paramsAppAdq <- adq.app(Xapp, zapp);
	paramsAppAdl <- adl.app(Xapp, zapp);
	paramsAppNba <- nba.app(Xapp, zapp);
	paramsAppLogWithoutIntercept <- log.app(Xapp, zapp, FALSE, 1e-5, TRUE);
	paramsAppLogWithIntercept <- log.app(Xapp, zapp, TRUE, 1e-5);
	paramsAppQuadLog <- log.quad.app(Xapp, zapp, 1e-3, TRUE);
	binTree <- tree(factor(zapp) ~ ., data=as.data.frame(cbind(Xapp, zapp)), control=tree.control(nobs=dim( Xapp )[1], mindev = 0.0001));
	cvModel <- cv.tree(binTree);
	bestSize <- cvModel$size[which(cvModel$dev==min(cvModel$dev))];
	binTree2 <- prune.misclass(binTree, best=bestSize[length(bestSize)]);

	print('yo'); # Test
	predictionsAdq <- ad.val(paramsAppAdq, Xtst)$predictions;
	predictionsAdl <- ad.val(paramsAppAdl, Xtst)$predictions;
	predictionsNba <- ad.val(paramsAppNba, Xtst)$predictions;
	predictionsLogWithoutIntercept <- log.val(paramsAppLogWithoutIntercept$beta, Xtst)$predictions;
	predictionsLogWithIntercept <- log.val(paramsAppLogWithIntercept$beta, Xtst)$predictions;	
	predictionsQuadLog <- log.quad.val(paramsAppQuadLog$beta, Xtst)$predictions;
	predictionsBinTree <- predict(binTree2, as.data.frame(Xtst), type = "class");

	print('yo'); # Test
	adqErrorRatesTst[j] <- errorRate(predictionsAdq, ztst);
	adlErrorRatesTst[j] <- errorRate(predictionsAdl, ztst);
	nbaErrorRatesTst[j] <- errorRate(predictionsNba, ztst);
	logWithoutInterceptErrorRatesTst[j] <- errorRate(predictionsLogWithoutIntercept, ztst);
	logWithInterceptErrorRatesTst[j] <- errorRate(predictionsLogWithIntercept, ztst);
	quadLogErrorRatesTst[j] <- errorRate(predictionsQuadLog, ztst);
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

errorRatesList[[1]] <- adqErrorRatesTst;
errorRatesList[[2]] <- adlErrorRatesTst;
errorRatesList[[3]] <- nbaErrorRatesTst;
errorRatesList[[4]] <- logWithoutInterceptErrorRatesTst;
errorRatesList[[5]] <- logWithInterceptErrorRatesTst;
errorRatesList[[6]] <- quadLogErrorRatesTst;
errorRatesList[[7]] <- binTreeErrorRatesTst;

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