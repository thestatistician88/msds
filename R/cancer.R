#' Wisconsin Breast Cancer Data set
#'
#' A dataset containing 30 measurements computed from digitized images of a fine need aspirate (FNA) of a breast mass.
#' The measurements describe various characteristics of the cell nuclei that are present in the image.  Each image is also labeled
#' as being benign (B) or malginant (M) via the diagnosis variable.  The data set was obtained through the UCI data data base archives
#' \insertCite{cancer_data}{msds}.
#'
#' @format A data frame with 569 observations on the following 10 characeristics each summarized by the mean, standard error, and "worst":
#' \describe{
#'   \item{\code{diagnosis}}{diagnosis: Benign (B) , malignant (M)}
#'   \item{\code{radius}}{radius: distances from center to points on the perimeter}
#'   \item{\code{texture}}{a grey scale value from the image itself}
#'   \item{\code{perimeter}}{NA }
#'   \item{\code{area}}{NA }
#'   \item{\code{smoothness}}{local variation in radius lengths}
#'   \item{\code{compactness}}{perimeter^2/area -1}
#'   \item{\code{concavity}}{severity of concave portions of the contour}
#'   \item{\code{concave points}}{number of concave portions of the contour}
#'   \item{\code{symmetry}}{NA}
#'   \item{\code{fractal dimensions}}{"coastline approximation" - 1}
#' }
#'
#' @details
#' The dataset includes 30 numerical aggregates obtained from imaging of 569 breast mass tissue samples. There are 10 well definied characteristics that are
#' measured for each nuclei in the image.  Since multiple nuclei are present in each image the numeric values are aggreated by taking the mean, computing the standard error,
#' or taking the worst measuremtn (max or min). \insertCite{cancer_data}{msds}.
#'
#' @source \insertRef{cancer_data}{MSDS}
#' \url{https://doi.org/10.24432/C5DW2B}
"cancer"
