source('./fonctions-tp4/cleanZ.R')
source('./fonctions-tp4/pOmega1X.R')
source('./fonctions-tp4/pOmega2X.R')
source('./fonctions-tp4/post.pr.R')
source('./fonctions-tp4/errorRate.R')
source('./fonctions-tp4/log.app.R')
source('./fonctions-tp4/log.val.R')
source('./fonctions-tp4/separ1.R')

# Test
data <- read.table('./donnees-tp4/yo_100.txt', header=F);
X <- data[, 1:2];
z <- data[, 3];
data <- separ1(X, z)
Xapp <- data$Xapp;
Xtst <- data$Xtst;
zapp <- data$zapp;
ztst <- data$ztst;
results <- log.app(Xapp, zapp, TRUE, 1e-5)
print(results);
results2 <- log.val(Xtst, results$beta);
print(results2);
print(ztst);
print(errorRate(results2$predictions, ztst));