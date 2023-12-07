## ----eval=FALSE---------------------------------------------------------------
#   function(data, k, max.iter = 100) {
#    #   Initialize cluster center
#    centers <- data[sample(nrow(data), k), ]
#    #  Initialize cluster allocation
#    clusters <- numeric(nrow(data))
#  
#    for (iter in 1:max.iter) {
#      # Assign data points to the nearest cluster center
#      for (i in 1:nrow(data)) {
#        min.dist <- Inf
#        for (j in 1:k) {
#          dist <- sum((data[i, ] - centers[j, ])^2)
#          if (dist < min.dist) {
#            min.dist <- dist
#            clusters[i] <- j
#          }
#        }
#      }
#  # Update cluster center
#   new.centers <- centers
#  for (j in 1:k) {
#  cluster.data <- data[clusters == j, ]
#  if (nrow(cluster.data) > 0) {
#  new.centers[j, ] <- colMeans(cluster.data)
#  }
#  }
#   #  Check convergence
#  if (all(new.centers == centers)) {
#  break
#  }
#  centers <- new.centers
#  }
#  list(centers = centers, clusters = clusters)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  List kmeansC(NumericMatrix data, int k, int maxIter = 100) {
#     int n = data.nrow(), d = data.ncol();
#     NumericMatrix centers(k, d);
#     IntegerVector clusters(n, 0);
#     bool change = true;
#     int iter = 0;
#  
#     // Initialize cluster center
#     IntegerVector center_indices = Rcpp::sample(n, k, false) - 1; // 减1以适应C++的0索引
#     for (int i = 0; i < k; ++i) {
#       centers(i, _) = data(center_indices[i], _);
#     }
#  
#     // Iteration until convergence or maximum number of iterations reached
#     while (change && iter < maxIter) {
#       change = false;
#       ++iter;
#  
#       // Assign data points to the nearest center point
#       for (int i = 0; i < n; ++i) {
#         double minDist = R_PosInf;
#         int bestCluster = 0;
#         for (int j = 0; j < k; ++j) {
#           double dist = sqrt(sum(pow(data(i,_) - centers(j,_), 2)));
#           if (dist < minDist) {
#             minDist = dist;
#             bestCluster = j;
#           }
#         }
#         if (clusters[i] != bestCluster) {
#           clusters[i] = bestCluster;
#           change = true;
#         }
#       }
#  
#       //  Update center point
#       if (change) {
#         NumericMatrix newCenters(k, d);
#         IntegerVector counts(k, 0);
#         for (int i = 0; i < n; ++i) {
#           int cluster = clusters[i];
#           counts[cluster]++;
#           for (int j = 0; j < d; ++j) {
#             newCenters(cluster, j) += data(i, j);  // Accumulate all dimensions
#         }
#         for (int j = 0; j < k; ++j) {
#           if (counts[j] > 0) {
#             for (int col = 0; col < d; ++col) {
#               centers(j, col) = newCenters(j, col) / counts[j];
#             }
#           }
#         }
#       }
#  
#     return List::create(Named("centers") = centers, Named("clusters") = clusters);
#   }
#  }

## ----eval=FALSE---------------------------------------------------------------
#  library(SA23204194)
#  library(microbenchmark)
#  set.seed(123)
#  data <- matrix(rnorm(100), ncol = 2)
#   kmeans_custom(data, 3)
#   tm <- microbenchmark(
#    R = kmeansR(data,3),
#    Rcpp = kmeansC(data,3)
#    times = 1000
#  )

## ----eval=FALSE---------------------------------------------------------------
#  function(f, df, x0, maxiter = 100, tol = 1e-6) {
#    x <- x0
#    for (iter in 1:maxiter) {
#      fx <- f(x)
#      dfx <- df(x)
#  
#      if (abs(dfx) < .Machine$double.eps) {
#        stop("Derivative is zero. No solution found.")
#      }
#  
#      x_new <- x - fx / dfx
#      if (abs(x_new - x) < tol) {
#        return(x_new)
#      }
#  
#      x <- x_new
#    }
#    stop("Maximum iterations reached. No solution found.")
#  }

## ----eval=FALSE---------------------------------------------------------------
#  double newtonMethodC(Function f, Function df, double x0, int maxiter = 100, double tol = 1e-6) {
#    double x = x0;
#    double fx = Rcpp::as<double>(f(x));
#    double dfx;
#    int iter = 0;
#  
#    while (std::abs(fx) > tol && iter < maxiter) {
#      dfx = Rcpp::as<double>(df(x));
#      if (dfx == 0) {
#        stop("Derivative is zero. No solution found.");
#      }
#  
#      x = x - fx / dfx;
#      fx = Rcpp::as<double>(f(x));
#      iter++;
#    }
#  
#    if (std::abs(fx) > tol) {
#      stop("Maximum iterations reached. No solution found.");
#    }
#  
#    return x;
#  }

## ----eval=FALSE---------------------------------------------------------------
#  library(SA23204194)
#  library(microbenchmark)
#   kmeans_custom(data, 3)
#   ts <- microbenchmark(
#    R = newtonMethodR(f,df,x0=1.5),
#    Rcpp = newtonMethodC(f,df,x0=1.5)
#    times = 1000
#  )

