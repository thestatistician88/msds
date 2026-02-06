
#' Linear Combination of Variables
#'
#' Performs linear combinations using a data matrix y.
#'
#' @param y A data frame or numeric matrix.
#' @param C Matrix of weights.
#'
#'
#' @returns A data frame of the linear combinations:
#' \item{LRT.Layers}{A data frame with the Lilkelihood Ratio Test Statistic for each layer of the tree.}
#' \item{Overall}{The overall Lilkelihood Ratio Test Statistics and p-value.}
#'
#' @details Generates a new data matrix that are linear combinations of the data matrix y.
#' @export
#' @examples
#'y<-data.frame(Y1=rnorm(10),Y2=rnorm(10),Y3=rnorm(10))
#'C1<-data.frame(Z1=c(1,1,1),Z2=c(1,0,0),Z3=c(5,2,4))
#'lc.trans(y,C)
#'
#'
#'
lc.trans<-function(y,C){
  if( !(is.data.frame(X) || is.data.frame(C))) {errorCondition("X or C is not a dataframe.")}
  if( !all(apply(X,2,is.numeric)) ) {errorCondition("Nonnumeric variables in X exists.")}
  dim.x<-dim(X)
  dim.c<-dim(C)
  if(dim.x[2]!=dim.c[1]){errorCondition("X and C are nonconformable")}
  X.mat<-as.matrix(X)
  C.mat<-as.matrix(C)
  Z=X.mat %*% C.mat
  Z=data.frame(Z)
  names(Z)<-colnames(C)
  return(Z)
}



#####################################################################################################

