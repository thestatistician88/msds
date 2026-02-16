
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


#' Visualize Principal Axes
#'
#' This function plots the two dimensional data matrix `x` along with an estimate of the principal axes obtained via
#' spectral decomposition of the data's covariance matrix.
#'
#'
#' @param data A data frame with exactly two numeric columns (x and y)
#' @param axis_label_size Numeric value controlling the font size of axis labels (default = 12)
#' @param point_size Numeric value controlling the size of data points (default = 3)
#' @param main Character string for the title (default is empty)
#' @param xlab Character string for the x axis variable (default is variable name)
#' @param ylab Character string for the y axis variable (default is variable)
#' @param arrow_scale Numeric value controlling the length of PC arrows.
#'   If NULL (default), arrows are scaled by 2.5 times the square root of the eigenvalues of the sample covariance amtrix.
#'   If numeric, uses the same scale factor for both PC arrows.
#'
#' @return A list containing:
#'   \item{plot}{ggplot2 object of the visualization}
#'   \item{pca_result}{prcomp object with PCA results}
#'   \item{variance_explained}{Vector of variance explained when converting the data to principal components.}
#'
#' @examples
#' # Create sample data
#' set.seed(123)
#' sample_data <- data.frame(x = rnorm(50, 5, 2), y = rnorm(50, 10, 5))
#' sample_data$y <- sample_data$x + rnorm(50, 0, 1)
#'
#' # Run PCA visualization with automatic arrow scaling
#' result <- plot.praxes(sample_data, axis_label_size = 14)
#' print(result$plot)
#'
#' # Run with custom arrow scale
#' result <- plot.praxes(sample_data, arrow_scale = 5)
#' print(result$plot)
#'
#' @export
plot_praxes <- function(data,
                              axis_label_size = 12,
                              point_size = 3,
                              main="",
                              xlab=NULL,
                              ylab=NULL,
                              arrow_scale = NULL
                              ) {

  # Load required library
  if (!require("ggplot2")) {
    stop("ggplot2 package is required. Please install it using: install.packages('ggplot2')")
  }

  # Input validation
  if (!is.data.frame(data)) {
    data=data.frame(data)
  }

  if (ncol(data) != 2) {
    stop("Data frame must have exactly 2 columns")
  }

  if (!all(sapply(data, is.numeric))) {
    stop("All columns must be numeric")
  }

  if (axis_label_size <= 0) {
    stop("axis_label_size must be positive")
  }

  if (!is.null(arrow_scale) && arrow_scale <= 0) {
    stop("arrow_scale must be positive or NULL")
  }
  #Hardcoding PCA on Covariance Matrix.  User should standardize data to view principal axes when using
  #correlation matrix.
  center_data = TRUE
  scale_data = FALSE
  # Perform PCA
  pca_result <- prcomp(data, center = center_data, scale. = scale_data)

  # Calculate center of data
  center <- colMeans(data)

  # Extract rotation matrix (eigenvectors)
  rotation <- pca_result$rotation

  # Calculate variance explained
  variance_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

  # Determine arrow scales for each PC
  if (is.null(arrow_scale)) {
    pc1_scale <- 2.5 * pca_result$sdev[1]
    pc2_scale <- 2.5 * pca_result$sdev[2]
  } else {
    pc1_scale <- arrow_scale
    pc2_scale <- arrow_scale
  }

  # Create arrow endpoints for PC1 and PC2
  pc1_arrow <- data.frame(
    x = center[1],
    y = center[2],
    xend = center[1] + rotation[1, 1] * pc1_scale,
    yend = center[2] + rotation[2, 1] * pc1_scale,
    PC = "PC1"
  )

  pc2_arrow <- data.frame(
    x = center[1],
    y = center[2],
    xend = center[1] + rotation[1, 2] * pc2_scale,
    yend = center[2] + rotation[2, 2] * pc2_scale,
    PC = "PC2"
  )

  # Combine arrows
  arrows_df <- rbind(pc1_arrow, pc2_arrow)

  # Get column names for labeling
  col_names <- colnames(data)
  if(is.null(xlab)){xname=col_names[1]}
  if(!is.null(xlab)){xname=xlab}
  if(is.null(ylab)){yname=col_names[2]}
  if(!is.null(ylab)){yname=ylab}
  # Create ggplot
  p <- ggplot(data, aes(x = .data[[col_names[1]]], y = .data[[col_names[2]]])) +
    geom_point(color = "gray40", size = point_size, alpha = 0.7) +
    geom_segment(data = arrows_df,
                 aes(x = x, y = y, xend = xend, yend = yend, color = PC),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 linewidth = 1.2) +
    scale_color_manual(values = c("PC1" = "red", "PC2" = "blue"),
                       labels = c(paste0("PC1 (", round(variance_explained[1], 1), "%)"),
                                  paste0("PC2 (", round(variance_explained[2], 1), "%)"))) +
    #coord_fixed(ratio = 1) +
    labs(title = main,
         x = xname,
         y = yname,
         color = "Principal\nComponent") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = axis_label_size),
      axis.title = element_text(size = axis_label_size + 2, face = "bold"),
      plot.title = element_text(size = axis_label_size + 4, face = "bold", hjust = 0.5),
      legend.text = element_text(size = axis_label_size - 2),
      legend.title = element_text(size = axis_label_size, face = "bold"),
      panel.grid.minor = element_blank()
    )
  print(p)
  # Return results
  return(list(
    plot = p,
    pca_result = pca_result,
    variance_explained = variance_explained
  ))
}







