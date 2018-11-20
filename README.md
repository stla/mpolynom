# mpolynom

To get the normals of an implicit surface, one needs the gradient of the 
implicit equation. Many known implicit surfaces are algebraic: the implicit 
equation is defined by a multivariate polynomial.

Instead of entering the gradient manually, I was looking for a way to derive it 
with R in the case of an algebraic surface. I have been able to do so with the 
`multipol` package. Unfortunately, the evaluation of a multivariate polynomial 
with the `multipol` package (function `as.function`) is very slow.

This is why I created the `mpolynom` package. It depends on `multipol`, and it 
enhances it by providing a fast evaluation of a multivariate polynom. 
This package is built on a C++ library written by John Burkardt, which is ported 
to R with the help of the `Rcpp` package.

```r
# define a polynomial function ####
phi <- (1+sqrt(5))/2
f0 <- function(x,y,z){
  4*(phi^2*x^2-y^2)*(phi^2*y^2-z^2)*(phi^2*z^2-x^2) - 
    (1+2*phi)*(x^2+y^2+z^2-1)^2*1
}
f <- function(x,y,z){
  ifelse(x*x+y*y+z*z < 3, f0(x,y,z), NaN)
}
library(mpolynom)
P <- f0(x., y., z.)

# run the marching cubes algorithm ####
nx <- 120; ny <- 120; nz <- 120
x <- seq(-1.8, 1.8, length=nx) 
y <- seq(-1.8, 1.8, length=ny)
z <- seq(-1.8, 1.8, length=nz) 
g <- expand.grid(x=x, y=y, z=z)
voxel <- array(with(g, f(x,y,z)), c(nx,ny,nz))
library(misc3d)
surf <- computeContour3d(voxel, maxvol=max(voxel[!is.nan(voxel)]), level=0, 
                         x=x, y=y, z=z)

# build the rgl mesh ####
fx <- differentiate(P, c(1,0,0))
fy <- differentiate(P, c(0,1,0))
fz <- differentiate(P, c(0,0,1))
gradient <- function(xyz){
  cbind(evalPoly(fx,xyz), evalPoly(fy,xyz), evalPoly(fz,xyz))
}
library(rgl)
mesh <- tmesh3d(vertices = t(surf),
                indices = matrix(1:nrow(surf), nrow=3),
                homogeneous = FALSE,
                normals = -gradient(surf))
mesh$normals <- rbind(mesh$normals,1)

# plot ####
open3d(windowRect = c(50, 50, 550, 550))
bg3d(rgb(54, 57, 64, maxColorValue = 255))
shade3d(mesh, color=rgb(1,0,1))
```

[![](https://thumbs.gfycat.com/GraciousRevolvingFlatfish-size_restricted.gif)](https://gfycat.com/GraciousRevolvingFlatfish)