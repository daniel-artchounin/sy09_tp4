source('./fonctions-tp4/computeQuadraticModel.R');

log.quad.val <- function(Xtst, beta){
	Xtst <- as.matrix(Xtst);
	Xtst <- computeQuadraticModel(Xtst);
	log.val(Xtst, beta);
}