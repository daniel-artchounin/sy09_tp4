source('./fonctions-tp4/cleanZ.R');
source('./fonctions-tp4/pOmega1X.R');
source('./fonctions-tp4/pOmega2X.R');
source('./fonctions-tp4/post.pr.R');
source('./fonctions-tp4/errorRate.R');
source('./fonctions-tp4/log.app.R');
source('./fonctions-tp4/log.val.R');
source('./fonctions-tp4/log.quad.app.R');
source('./fonctions-tp4/log.quad.val.R');
source('./fonctions-tp4/separ1.R');
source('./fonctions-tp4/prob.log.R');
source('./fonctions-tp4/prob.log2.R');


# Data loading
data <- read.table('./donnees-tp4/yo_100.txt', header=F);
X <- data[, 1:2];
z <- data[, 3];
data <- separ1(X, z)
Xapp <- data$Xapp;
Xtst <- data$Xtst;
zapp <- data$zapp;
ztst <- data$ztst;


print('* Test 1 *');
results <- log.app(Xapp, zapp, TRUE, 1e-5)
print(results);
results2 <- log.val(results$beta, Xtst);
print(results2);
print(ztst);
print(errorRate(results2$predictions, ztst));

# Decision boundary
pdf(file = "./images/ex1/logReg.pdf");
prob.log(results$beta, Xtst, ztst, c(0.5));
dev.off();


print('* Test 2 *');
results <- log.quad.app(Xapp, zapp, 1e-5)
print(results);
results2 <- log.quad.val(results$beta, Xtst);
print(results2);
print(ztst);
print(errorRate(results2$predictions, ztst));

# Decision boundary
pdf(file = "./images/ex1/quadLogReg.pdf");
prob.log2(results$beta, Xtst, ztst, c(0.5));
dev.off();