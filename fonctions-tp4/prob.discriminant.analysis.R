prob.discriminant.analysis <- function(params, X, z, niveaux){
    lineWidth <- 3;
    discretisation <- 50;
    N <- 3;
    colors <- c("cornflowerblue", "chartreuse4", "coral4");
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

    # Cast
    grille <- as.matrix(grille)

    # Calcul des valeurs de la fonction 
    predictions[[1]] <- ad.val(params[[1]], grille)$prob[,1];
    predictions[[2]] <- ad.val(params[[2]], grille)$prob[,1];
    predictions[[3]] <- ad.val(params[[3]], grille)$prob[,1];

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
        legend=c("ADQ", "ADL", "NBA"), 
        col = c("cornflowerblue", "chartreuse4", "coral4"), 
        pch = 21, 
        inset=0
    );
}