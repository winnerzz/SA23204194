% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{kmeansC}
\alias{kmeansC}
\title{A function for implementing k-means clustering algorithm}
\usage{
kmeansC(data, k, maxIter = 100L)
}
\arguments{
\item{data}{dataset}

\item{k}{the number of clusters}

\item{maxIter}{Maximum number of iterations}
}
\value{
A list containing two elements : centers and clusters
}
\description{
Divide a set of observations into k clusters, so that each observation belongs to the nearest cluster center.
}
\examples{
\dontrun{
    set.seed(194)
    data <- matrix(rnorm(100),ncol = 2)
    result <- kmeans_customR(data,3)
}
}
