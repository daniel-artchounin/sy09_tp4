source('./fonctions-tp4/mvdnorm.r')
source('./fonctions-tp4/prob.ad.R')
source('./fonctions-tp4/ad.val.R')
source('./fonctions-tp4/adl.app.R')
source('./fonctions-tp4/adq.app.R')
source('./fonctions-tp4/nba.app.R')
library(MASS)

# adq.app <- function(Xapp, zapp)
# adl.app <- function(Xapp, zapp)
# nba.app <- function(Xapp, zapp)

# ad.val <- function(parameters, Xtst)

# prob.ad <- function(param, X, z, niveaux)

data <- read.table('./donnees-tp4/yo-100.txt', header=F);
X <- data[, 1:2];
z <- data[, 3];
Xapp <- X[c(1:30, 50:70),];
zapp <- z[c(1:30, 50:70)];
Xtst <- X[c(31:49, 71:100),];
ztst <- z[c(31:49, 71:100)];

# Les paramètres du modèle

MuAppAdq <- adq.app(Xapp, zapp)
MuAppAdl <- adl.app(Xapp, zapp)
# MuAppNba <- nba.app(Xapp, zapp)

# Les probabilités a posteriori

# probTstAdq <- ad.val(MuAppAdq, Xtst)
# pdf(file = "./images/ex1/probTstAdq.pdf");
# plot(probTstAdq);
# dev.off();

# probTstAdl <- ad.val(MuAppAdl, Xtst)
# pdf(file = "./images/ex1/probTstAdl.pdf");
# plot(probTstAdl);
# dev.off();

# probTstNba <- ad.val(MuAppNba, Xtst)
# pdf(file = "/mages/ex1/probTstNba.pdf");
# plot(probTstNba);
# dev.off();

# les courbes de niveau des probabilités a posteriorib P(w1|x) estimées

pdf(file = "./images/ex1/ctstAdq.pdf");
prob.ad(MuAppAdq, Xtst, ztst, c(0.005, 0.5));
dev.off();

pdf(file = "./images/ex1/ctstAdl.pdf");
prob.ad(MuAppAdl, Xtst, ztst, c(0.005, 0.5));
dev.off();
# ctstNba <- prob.ad(MuAppNba, Xtst, ztst, 0.5)
# pdf(file = "./images/ex1/ctstNba.pdf");
# plot(ctstNba);
# dev.off();