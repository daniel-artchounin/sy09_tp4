cleanZ <- function (zapp){
	n <- length(zapp);
	t <- matrix(1, n, 1);
	t[which( zapp==2 )] = 0;
	t[which( zapp==1 )] = 1;
	t;
}