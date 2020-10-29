library(MASS)
library(RColorBrewer)
coul1 <- brewer.pal(4,"YlGnBu")
coul1 <- colorRampPalette(coul1)(16)
coul1 <- rev(coul1)
coul2 <- brewer.pal(4,"OrRd")
coul2 <- colorRampPalette(coul2)(16)
coul2 <- rev(coul2)


getLevelLine <- function(mu,sigma) {  #получение коэффициентов линии уровня (x-mu)^T %*% Sig^-1 %*% (x-mu)
  invSigma <- solve(sigma)
  a <- invSigma[1,1]
  b <- invSigma[1,2]+invSigma[2,1]
  c <- invSigma[2,2]
  d <- -2*mu[1]*invSigma[1,1]-mu[2]*b
  e <- -2*mu[2]*invSigma[2,2]-mu[1]*b
  f <- invSigma[1,1]*mu[1]^2+invSigma[2,2]*mu[2]^2+mu[1]*mu[2]*b
  return (c(a,b,c,d,e,f))
}


levelLine <- function(mu,sigma,x,y) {    #построение линии уровня
  coeffs <- getLevelLine(mu,sigma)
  n <- function(X,Y) {
    return (1.0/(2*pi*sqrt(det(sigma)))*exp(-0.5*(X^2*coeffs[1]+X*Y*coeffs[2]+Y^2*coeffs[3]+X*coeffs[4]+Y*coeffs[5]+coeffs[6])))
  }
  return (outer(x,y,n))
}


drawOne <- function(x,y,z,levelColor,message,deepMap=FALSE) {
  par(pty="s",bty="o")
  if (deepMap==TRUE) {
    filled.contour(
      x,y,z,
      col=coul1,
      main=paste0(message,", спектральная форма"),
      plot.axes={
        axis(1)
        axis(2)
        contour(x,y,z,add=TRUE)
      }
    )
  }
  contour(x,y,z,lwd=1,col=levelColor,asp=1,main=message)
}


drawTwo <- function(x,y,z1,z2,levelColor1,levelColor2,message,deepMap=FALSE) {
  par(pty="s",bty="o")
  if (deepMap==TRUE) {
    z <- z2-z1
    couls <- c(coul1,coul2)
    filled.contour(
      x,y,z,
      col=couls,
      asp=1,
      main=paste0(message,", спектральная форма"),
      plot.axs={
        axis(1)
        axis(2)
        contour(x,y,z,add=TRUE)
      }
    )
  }
  contour(x,y,z1,lwd=1,col=levelColor1,asp=1,main=message)
  contour(x,y,z2,lwd=1,col=levelColor2,asp=1,add=TRUE)
}


main <- function() {
  x <- seq(-5,5,by=0.05)
  y <- x
  z <- levelLine(c(0,0),matrix(c(1,0,0,1),2,2),x,y)
  drawOne(x,y,z,"blue1","Сферическая матрица",TRUE)
  z <- levelLine(c(0,0),matrix(c(4,1,1,2),2,2),x,y)
  drawOne(x,y,z,"blue1","Наклонная эллиптическая матрица",TRUE)
  z <- levelLine(c(0,0),matrix(c(1,0,0,4),2,2),x,y)
  drawOne(x,y,z,"blue1","Эллиптическая матрица",TRUE)
  z2 <- levelLine(c(2,3),matrix(c(1,0,0,1),2,2),x,y)
  drawTwo(x,y,z,z2,"blue1","green2","Два класса",TRUE)
}
main()
