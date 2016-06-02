log.val <- function(beta, Xtst){
	Xtst <- as.matrix(Xtst);
	n <- dim(Xtst)[1];
	p <- dim(Xtst)[2];
	pBeta <- dim(beta)[1];
	if(p != pBeta){ 
		Xtst <-cbind(matrix(1, n, 1), Xtst);
	}
 	results <- list();
 	prob <- post.pr(Xtst, beta);
 	predictions <- apply(prob, 1, which.max);
 	results$prob <- prob;
 	results$predictions <- predictions;
 	results;
}