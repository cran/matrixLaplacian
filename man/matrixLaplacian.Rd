\name{matrixLaplacian}
\alias{matrixLaplacian}
\title{
Normalized Laplacian Matrix and Laplacian Map
}
\description{
Constructs the normalized Laplacian matrix of a square matrix, returns the eigenvectors (singular vectors) and visualization of normalized Laplacian map.
}
\usage{
matrixLaplacian(A, plot2D=TRUE, plot3D=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{A}{
  	a numeric or complex matrix whose normalized Laplacian matrix is to be computed
}
  \item{plot2D}{
		a logical value indicating whether a 2-D map should be plotted
}
  \item{plot3D}{
		a logical value indicating whether a 3-D map should be plotted
}
}
\value{
\item{LaplacianMatrix}{
		the symmetric normalized Laplacian matrix
}
\item{eigenvector}{
		the eigenvectors of normalized Laplacian matrix, which are same as singular vectors
}
}
\examples{
#Create a square matrix
A <- matrix(c(1:16), 4, 4)
#Construct normalized Laplacian matrix and plot map
m <- matrixLaplacian(A, plot2D=TRUE, plot3D=TRUE)
#See the eigenvectors
vector<-m$eigenvector
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.