
#' Linear Combination of Variables
#'
#' Performs linear combinations using a data matrix y.
#'
#' @param y A data frame or numeric matrix.
#' @param C A dataframe containg the weights.
#'
#'
#' @returns A data frame of the linear combinations specified in C.
#'
#' @details Generates a new data matrix that are linear combinations of the data matrix y. The weights for the linear combinations
#' are provided as columns in C. The variables names within C will be used as the new variables of the return matrix.
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
#' Scree Plots Derived From PCA
#'
#' Produces scree plots from user provided eigenvalues or an object created by [stats::princomp] or [stats:prcomp].
#'
#' @param x An object containing a `sdev` component, such as that returned by [stats::princomp] or [stats:prcomp] .
#' @param eig.vals Vector of eigenvalues from a PCA decomposition.  Alternative to `x`.
#' @param ref Horizontal reference line for the cumulative proportion plot.
#' @param label.size Text size for x and y axis labels.
#'
#'
#' @returns A 1x3 panel of scree plots using the eigenvalues, proportion of variance explained, and cumulative proportion of variance explained.
#'
#' @details The user may specify eigenvalues for plotting rather than an object created by one of the common PCA functions. User should specify `x=NULL` when
#' using the alternative option.
#' @export
#' @examples
#'faculty.pca<-prcomp(economics_reduced)
#'pca.scree(facult.pca,ref=.9,label.size=14)
#'
#'
#'
#'
pca.scree <- function(x, eig.vals = NULL, ref = 0.8, label.size = 11) {
  # Load required package
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required but not installed.")
  }

  # Check inputs
  if (all(names(x) != "sdev") & is.null(eig.vals)) {
    return(cat("x does not contain an sdev variable (sqrt(eigenvalues))"))
  }
  if (any(names(x) == "sdev")) {
    eig.vals <- (x$sdev)^2
  }

  # Prepare data
  n_components <- length(eig.vals)
  prop_var <- eig.vals / sum(eig.vals)
  cumulative_prop <- cumsum(prop_var)

  plot_data <- data.frame(
    PC = 1:n_components,
    EigenValues = eig.vals,
    PropVariance = prop_var,
    CumulativeProp = cumulative_prop
  )

  # Plot 1: Eigenvalues
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC, y = EigenValues)) +
    ggplot2::geom_point(size = 4, shape = 18) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::labs(x = "Principal Comp.", y = "Eigen Values") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = label.size),
      axis.text = ggplot2::element_text(size = label.size * 0.9)
    )

  # Plot 2: Proportion of variance
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC, y = PropVariance)) +
    ggplot2::geom_point(size = 4, shape = 18) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::labs(x = "Principal Comp.", y = "Prop. of Total Variance") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = label.size),
      axis.text = ggplot2::element_text(size = label.size * 0.9)
    )

  # Plot 3: Cumulative proportion with reference line
  p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC, y = CumulativeProp)) +
    ggplot2::geom_hline(yintercept = ref, linetype = "dashed", linewidth = 0.75) +
    ggplot2::geom_point(size = 4, shape = 18) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::labs(x = "Principal Comp.", y = "Cumul. Prop.") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = label.size),
      axis.text = ggplot2::element_text(size = label.size * 0.9)
    )

  # Combine plots
  gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
}
