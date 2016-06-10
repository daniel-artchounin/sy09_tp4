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
print('- Régression linéaire (sans intercept) -');
results1 <- log.app(Xapp, zapp, FALSE, 1e-5, TRUE)
print(results1);
results2 <- log.val(results1$beta, Xtst);
print(results2);
print(ztst);
print(errorRate(results2$predictions, ztst));

# Decision boundary
pdf(file = "./images/ex1/logRegWithoutIntercept.pdf");
prob.log(results1$beta, Xtst, ztst, c(0.5));
dev.off();


print('- Régression linéaire (avec intercept) -');
results3 <- log.app(Xapp, zapp, TRUE, 1e-5)
print(results3);
results4 <- log.val(results3$beta, Xtst);
print(results4);
print(ztst);
print(errorRate(results4$predictions, ztst));

# Decision boundary
pdf(file = "./images/ex1/logRegWithIntercept.pdf");
prob.log(results3$beta, Xtst, ztst, c(0.5));
dev.off();


print('* Test 2 *');
print('- Régression quadratique -');
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