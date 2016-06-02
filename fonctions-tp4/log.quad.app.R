source('./fonctions-tp4/computeQuadraticModel.R');

log.quad.app  <- function(Xapp, zapp, epsi){
	Xapp <- as.matrix(Xapp);
	Xapp <- computeQuadraticModel(Xapp);
	log.app(Xapp, zapp, FALSE, epsi);
}