#' @importFrom stats rnorm rgamma
#' @importFrom Rcpp evalCpp
#' @import knitr
#' @import ggplot2
#' @import DAAG
#' @import boot
#' @import bootstrap
#' @import coda
#' @import dplyr
#' @import graphics
#' @import microbenchmark
#' @useDynLib SA23204194
NULL

#' @title A function for implementing k-means clustering algorithm
#' @description Divide a set of observations into k clusters, so that each observation belongs to the nearest cluster center.
#' @param data dataset
#' @param k the number of clusters
#' @param max.iter Maximum number of iterations
#' @return A list containing two elements : centers and clusters
#' @examples
#' \dontrun{
#'     set.seed(194)
#'     data <- matrix(rnorm(100),ncol = 2)
#'     result <- kmeansR(data,3)
#' }
#' @export

kmeansR <- function(data, k, max.iter = 100) {
  #   Initialize cluster center
  centers <- data[sample(nrow(data), k), ]
  #  Initialize cluster allocation
  clusters <- numeric(nrow(data))
  
  for (iter in 1:max.iter) {
    # Assign data points to the nearest cluster center
    for (i in 1:nrow(data)) {
      min.dist <- Inf
      for (j in 1:k) {
        dist <- sum((data[i, ] - centers[j, ])^2)
        if (dist < min.dist) {
          min.dist <- dist
          clusters[i] <- j
        }
      }
    }
    
# Update cluster center
 new.centers <- centers
for (j in 1:k) {
cluster.data <- data[clusters == j, ]
if (nrow(cluster.data) > 0) {
new.centers[j, ] <- colMeans(cluster.data)
}
}
    
 #  Check convergence
if (all(new.centers == centers)) {
break
}
centers <- new.centers
}
  
list(centers = centers, clusters = clusters)
}

#' @title Using Newton's method to find the root of a function
#' @description  Implement a function optimized using Newton's method to solve the roots of nonlinear equations
#' @param f Function that need to solve roots
#' @param df The derivative of function f
#' @param x0 Estimating the initial value of roots
#' @param maxiter Maximum number of iterations, default to 100
#' @param tol Error tolerance, used to determine the accuracy of the solution, defaults to 1e-6
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' f <- function(x) x^3 - x^2 - x - 1
#' df <- function(x) 3*x^2 - 2*x - 1
#' root <- newtonMethodC(f, df, x0 = 1.5)
#' print(root)
#' }
#' @export

newtonMethodR <- function(f, df, x0, maxiter = 100, tol = 1e-6) {
  x <- x0
  for (iter in 1:maxiter) {
    fx <- f(x)
    dfx <- df(x)
    
    if (abs(dfx) < .Machine$double.eps) {
      stop("Derivative is zero. No solution found.")
    }
    
    x_new <- x - fx / dfx
    if (abs(x_new - x) < tol) {
      return(x_new)
    }
    
    x <- x_new
  }
  stop("Maximum iterations reached. No solution found.")
}

