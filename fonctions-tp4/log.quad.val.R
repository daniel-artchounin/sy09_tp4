source('./fonctions-tp4/computeQuadraticModel.R');

log.quad.val <- function(beta, Xtst){
	Xtst <- as.matrix(Xtst);
	Xtst <- computeQuadraticModel(Xtst);
	log.val(beta, Xtst);
}