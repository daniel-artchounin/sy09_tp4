source('./fonctions-tp4/computeQuadraticModel.R');

log.quad.app  <- function(Xapp, zapp, epsi, pseudoInv=FALSE){
	Xapp <- as.matrix(Xapp);
	Xapp <- computeQuadraticModel(Xapp);
	log.app(Xapp, zapp, FALSE, epsi, pseudoInv);
}