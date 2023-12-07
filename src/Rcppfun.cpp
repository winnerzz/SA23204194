#include <Rcpp.h>
using namespace Rcpp;

//' @title Using Newton's method to find the root of a function
//' @description  Implement a function optimized using Newton's method to solve the roots of nonlinear equations
//' @param f Function that need to solve roots
//' @param df The derivative of function f
//' @param x0 Estimating the initial value of roots
//' @param maxiter Maximum number of iterations, default to 100
//' @param tol Error tolerance, used to determine the accuracy of the solution, defaults to 1e-6
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' f <- function(x) x^3 - x^2 - x - 1
//' df <- function(x) 3*x^2 - 2*x - 1
//' root <- newtonMethodC(f, df, x0 = 1.5)
//' print(root)
//' }
//' @export
// [[Rcpp::export]]
double newtonMethodC(Function f, Function df, double x0, int maxiter = 100, double tol = 1e-6) {
  double x = x0;
  double fx = Rcpp::as<double>(f(x));
  double dfx;
  int iter = 0;
  
  while (std::abs(fx) > tol && iter < maxiter) {
    dfx = Rcpp::as<double>(df(x));
    if (dfx == 0) {
      stop("Derivative is zero. No solution found.");
    }
    
    x = x - fx / dfx;
    fx = Rcpp::as<double>(f(x));
    iter++;
  }
  
  if (std::abs(fx) > tol) {
    stop("Maximum iterations reached. No solution found.");
  }
  
  return x;
}



//' @title A function for implementing k-means clustering algorithm
//' @description Divide a set of observations into k clusters, so that each observation belongs to the nearest cluster center.
//' @param data dataset
//' @param k the number of clusters
//' @param maxIter Maximum number of iterations
//' @return A list containing two elements : centers and clusters
//' @examples
//' \dontrun{
//'     set.seed(194)
//'     data <- matrix(rnorm(100),ncol = 2)
//'     result <- kmeans_customR(data,3)
//' }
//' @export
// [[Rcpp::export]]
 List kmeansC(NumericMatrix data, int k, int maxIter = 100) {
   int n = data.nrow(), d = data.ncol();
   NumericMatrix centers(k, d);
   IntegerVector clusters(n, 0);
   bool change = true;
   int iter = 0;
   
   // Initialize cluster center
   IntegerVector center_indices = Rcpp::sample(n, k, false) - 1; // 减1以适应C++的0索引
   for (int i = 0; i < k; ++i) {
     centers(i, _) = data(center_indices[i], _);
   }
   
   // Iteration until convergence or maximum number of iterations reached
   while (change && iter < maxIter) {
     change = false;
     ++iter;
     
     // Assign data points to the nearest center point
     for (int i = 0; i < n; ++i) {
       double minDist = R_PosInf;
       int bestCluster = 0;
       for (int j = 0; j < k; ++j) {
         double dist = sqrt(sum(pow(data(i,_) - centers(j,_), 2)));
         if (dist < minDist) {
           minDist = dist;
           bestCluster = j;
         }
       }
       if (clusters[i] != bestCluster) {
         clusters[i] = bestCluster;
         change = true;
       }
     }
     
     //  Update center point
     if (change) {
       NumericMatrix newCenters(k, d);
       IntegerVector counts(k, 0);
       for (int i = 0; i < n; ++i) {
         int cluster = clusters[i];
         counts[cluster]++;
         for (int j = 0; j < d; ++j) {
           newCenters(cluster, j) += data(i, j);  // Accumulate all dimensions
       }
       for (int j = 0; j < k; ++j) {
         if (counts[j] > 0) {
           for (int col = 0; col < d; ++col) {
             centers(j, col) = newCenters(j, col) / counts[j];
           }
         }
       }
     }
    }
  }
   return List::create(Named("centers") = centers, Named("clusters") = clusters);
}   
