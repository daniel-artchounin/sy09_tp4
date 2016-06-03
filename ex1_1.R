source('./fonctions-tp4/mvdnorm.r');
source('./fonctions-tp4/prob.ad.R');
source('./fonctions-tp4/ad.val.R');
source('./fonctions-tp4/adl.app.R');
source('./fonctions-tp4/adq.app.R');
source('./fonctions-tp4/nba.app.R');
source('./fonctions-tp4/separ1.R');
library(MASS)


# Data loading
data <- read.table('./donnees-tp4/yo_100.txt', header=F);
X <- data[, 1:2];
z <- data[, 3];
data <- separ1(X, z)
Xapp <- data$Xapp;
Xtst <- data$Xtst;
zapp <- data$zapp;
ztst <- data$ztst;

# Parameters of the models
paramsAppAdq <- adq.app(Xapp, zapp)
paramsAppAdl <- adl.app(Xapp, zapp)
paramsAppNba <- nba.app(Xapp, zapp)

# Predictions
probTstAdq <- ad.val(paramsAppAdq, Xtst)
pdf(file = "./images/ex1/probTstAdq.pdf");
plot(probTstAdq$predictions);
dev.off();

probTstAdl <- ad.val(paramsAppAdl, Xtst)
pdf(file = "./images/ex1/probTstAdl.pdf");
plot(probTstAdl$predictions);
dev.off();

probTstNba <- ad.val(paramsAppNba, Xtst)
pdf(file = "./images/ex1/probTstNba.pdf");
plot(probTstNba$predictions);
dev.off();

# Decision boundaries
pdf(file = "./images/ex1/ctstAdq.pdf");
prob.ad(paramsAppAdq, Xtst, ztst, c(0.5));
dev.off();

pdf(file = "./images/ex1/ctstAdl.pdf");
prob.ad(paramsAppAdl, Xtst, ztst, c(0.5));
dev.off();

pdf(file = "./images/ex1/ctstNba.pdf");
prob.ad(paramsAppNba, Xtst, ztst, c(0.5));
dev.off();