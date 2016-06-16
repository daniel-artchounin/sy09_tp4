prob.logistic.regression <- function(params, X, z, niveaux){
    lineWidth <- 3;
    discretisation <- 50;
    N <- 3;
    colors <- c("seagreen", "cornsilk4", "darkgoldenrod3");
    predictions <- list();
    deltaX <- (max(X[,1]) -min(X[,1]))/discretisation
    deltaY <- (max(X[,2]) -min(X[,2]))/discretisation
    minX <- min(X[,1])-deltaX
    maxX <- max(X[,1])+deltaX
    minY <- min(X[,2])-deltaY
    maxY <- max(X[,2])+deltaY
    
    # Grille d'affichage 
    grilleX <- seq(from=minX,to=maxX,by=deltaX)
    naffX <- length(grilleX)
    grilleY <- seq(from=minY,to=maxY,by=deltaY)
    naffY <- length(grilleY)
    grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
    grille2 <- cbind(grille, grille[,1]*grille[,2], grille[,1]^2, grille[,2]^2)

    # Cast
    grille <- as.matrix(grille)
    grille2 <- as.matrix(grille2)   

    # Calcul des valeurs de la fonction 
    predictions[[1]] <- log.val(params[[1]], grille)$prob[,1];
    predictions[[2]] <- log.val(params[[2]], grille)$prob[,1];
    predictions[[3]] <- log.val(params[[3]], grille2)$prob[,1];

    plot(X, col=c("darkorchid","firebrick","darkcyan","darkolivegreen","orange")[z])

    for(i in 1:N){
        contour(
            grilleX, 
            grilleY, 
            matrix(predictions[[i]],nrow=naffX,byrow=T), 
            add=T, 
            drawlabels=FALSE, 
            levels=niveaux, 
            col=colors[i], 
            lwd=lineWidth
        );
    }
    legend(
        "bottomright", 
        200,
        legend=c("RLCSI", "RLCAI", "RLQ"), 
        col = c("seagreen", "cornsilk4", "darkgoldenrod3"), 
        pch = 21, 
        inset=0
    );
}