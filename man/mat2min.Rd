\name{mat2min}
\alias{mat2min}
\title{mat2min}
\usage{
mat2min(mat, tabMat, scale = TRUE, byrow = TRUE, what = c("dist", "idx", "both"))
}
\arguments{
  \item{mat, tabMat}{Numerical matrices.}
  \item{scale}{Boolean, whether or not to standardize the matrix columns
    before calculating distances.}
  \item{byrow}{Whether or not to provide answers for each row of the
    first argument (the default) or for the matrix as a whole.}
  \item{what}{Argument determining what exactly is returned: the minimum
    value, the row in \code{tabMat} that is closest to a row in \code{mat},
    or both.
  }
}
\description{
  Function mat2min takes two matrices, and calculates all Euclidean
  distances between the rows of these matrices. The optional scale argument
  allows the three variables to be equally important: note that scaling is
  performed using the mean values and standard deviations of the
  \code{tabMat} argument.
}
\value{The function returns either the
  shortest distance found, the row of the \code{tabMat} argument that
  corresponds to the shortest distance, or both.}
\author{Ron Wehrens}
