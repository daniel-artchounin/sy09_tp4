source('./fonctions-tp4/pOmega1X.R')
source('./fonctions-tp4/pOmega2X.R')

post.pr <- function(X, omega){
	probabilities <- cbind(pOmega1X(X, omega), pOmega2X(X, omega));  
	probabilities;
}