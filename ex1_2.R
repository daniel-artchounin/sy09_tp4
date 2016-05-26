source('/fonctions-tp4/mvdnorm.r')
source('/fonctions-tp4/prob.ad.R')
source('/fonctions-tp4/prob.log')
source('/fonctions-tp4/prob.log2')
source('/ex1_1.R')

# On souhaite implémenter le modèle logistique binaire. On programmera tout d’abord deux fonctions,
# l’une permettant de faire l’apprentissage du modèle (on utilisera l’algorithme de Newton-Raphson
# présenté en cours), l’autre permettant d’appliquer le modèle obtenu sur un ensemble de données.
# Apprentissage. La fonction log.app, permettant d’apprendre les paramètres du modèles, prendra
# comme arguments d’entrée le tableau de données Xapp, le vecteur zapp des étiquettes associées, ainsi
# qu’une variable binaire intr indiquant s’il faut ou non ajouter une ordonnée à l’origine (intercept) à
# la matrice d’exemples et un scalaire epsi correspondant au seuil " en-deçà duquel on considère que
# l’algorithme d’apprentissage a convergé.
# Elle devra retourner la matrice beta correspondant à l’estimateur du maximum de vraisemblance
# des paramètres, de dimensions p  1 (ou (p + 1)  1 si une ordonnée à l’origine a été ajoutée),
# le nombre niter d’itérations effectuées par l’algorithme de Newton-Raphson, et la valeur logL de la
# vraisemblance à l’optimum.
# On pourra utiliser comme matrice de paramètres initiale (0) = (0; : : : ; 0)t. La convergence sera
# testée en comparant la norme de la différence entre deux estimations successives (q) et (q+1) au
# seuil " (on pourra choisir " = 1e ?? 5).

log.app  <- function(Xapp, zapp, intr, epsi){
}

log.val{
}