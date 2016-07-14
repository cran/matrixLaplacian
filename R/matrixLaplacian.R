
matrixLaplacian<-function(A, plot2D=TRUE, plot3D=TRUE)
{
# Make a symmetric adjacency matrix B
B <- A + t(A)

# Normalizer D
D <- matrix(0,nrow=dim(A)[1],ncol=dim(A)[1])
diag(D) <- B %*% rep(1,dim(A)[1])
diag(D)<-1/sqrt(diag(D))

# D^(-1/2)BD^(1/2)
Q <- D %*% B %*% D

# Normalized Laplacian Matrix
N <- diag(1,dim(Q)[1])-Q

# Eigenvectors
Eigen <- eigen(N)$vectors
  
# 2D Plot
if(plot2D==TRUE){plot(Eigen[,1],Eigen[,1],xlab="",ylab="")}

# 3D Plot
if(plot3D==TRUE){scatterplot3d(Eigen[,1],Eigen[,2],Eigen[,3],xlab="",ylab="",zlab="")}

# Return Normalized Laplacian Matrix and Eigenvectors
object<-list(LaplacianMatrix=N, eigenvector=Eigen)
object
}
