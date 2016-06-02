# This function computes the error rate of a supervised learning using two parameters:
# 	- ptst: The predicted vector of labels for the current set
# 	- ztst: The real vector of labels for the current set
errorRate <- function(ptst, ztst){
	rate <- length( which( ptst != ztst ) ) / length(ztst);
}