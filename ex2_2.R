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
# TO DO: implement decision trees !!!
# TO DO: implement quadratic logistic regression !!!
# TO DO: implement logistic regression !!!

alpha <- 0.05;
alphaDivBy2 <- alpha/2;
oneMinusAlphaDivBy2 <- 1 - alphaDivBy2
# methods <- c("adq", "adl", "nba", "log", "quadLog");
methods <- c("adq", "adl", "nba");
results <- list();
i <- 1;
N <- 100;
XAll <- NULL;
zAll <- NULL;
iterator <- 0;
errorRatesList <- NULL;

Donn <- read.csv("./donnees-tp4/Pima.csv", header=T);
X <- Donn[,1:7];
z <- Donn[,8];
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
quadLogErrorRatesTst <- c();
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
	# paramsAppLog <- log.app(Xapp, zapp, TRUE, 1e-5);
	# paramsAppQuadLog <- log.quad.app(Xapp, zapp, 1e-5);

	predictionsAdq <- ad.val(paramsAppAdq, Xtst)$predictions;
	predictionsAdl <- ad.val(paramsAppAdl, Xtst)$predictions;
	predictionsNba <- ad.val(paramsAppNba, Xtst)$predictions;
	# predictionsLog <- log.val(paramsAppLog$beta, Xtst)$predictions;
	# predictionsQuadLog <- log.quad.val(paramsAppQuadLog$beta, Xtst)$predictions;

	adqErrorRatesTst[j] <- errorRate(predictionsAdq, ztst);
	adlErrorRatesTst[j] <- errorRate(predictionsAdl, ztst);
	nbaErrorRatesTst[j] <- errorRate(predictionsNba, ztst);
	# logErrorRatesTst[j] <- errorRate(predictionsLog, ztst);
	# quadLogErrorRatesTst[j] <- errorRate(predictionsQuadLog, ztst);
}
errorRatesList[[1]] <- adqErrorRatesTst;
errorRatesList[[2]] <- adlErrorRatesTst;
errorRatesList[[3]] <- nbaErrorRatesTst;
# errorRatesList[[4]] <- logErrorRatesTst;
# errorRatesList[[5]] <- quadLogErrorRatesTst;
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