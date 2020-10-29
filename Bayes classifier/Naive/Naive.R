library(MASS)
muHat <- function(xl) {   #считаем значение мю по данным для класса
  n <- dim(xl)[2]
  mu <- array(NA,n)
  for (i in 1:n) {
    mu[i] <- mean(xl[ ,i])   #по каждой координате берём среднее
  }
  return (mu)
}


sigmaHat <- function(xl,mu) {   #считаем зачение сигма по данным для класса и полученному мю
  l <- dim(xl)[1]
  n <- dim(xl)[2]
  sigma <- matrix(0,n,n)
  for (i in 1:n) {
    sigma[i, ] <- sum((xl[ ,i]-mu[i])^2)/l
  }
  return (sigma)
}


naive <- function(mu,sigma,obj,lambda,P) {   #наивный Байес
  n <- length(mu)
  res <- log(lambda*P)   #добавочный коэффициент
  params <- array(NA,n)
  for (i in 1:n) {
    params[i] <- sigma[i,i]   #диагональные элементы матрицы (гипотеза наивного классификатора)
  }
  return (res+sum(log(Pyj(params,obj,mu))))   #значение функции правдоподобия
}


Pyj <- function(sj,xj,muj){    #коэффициент для j-ой компоненты
  return ((1.0/(sj*sqrt(2*pi)))*exp(-((xj-muj)^2)/(2*sj^2)))    #формула нормального наивного Байесовского классификатора
}


quality <- function(xl,mu1,sig1,mu2,sig2,l1,l2) {    #функция качества алгоритма
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  cnt <- table(xl[ ,n+1])
  P <- c(cnt[[1]]/l,cnt[[2]]/l)   #вероятности появления объектов из j-ого класса
  class <- numeric()
  err <- l
  for (i in 1:l) {
    if (naive(mu1,sig1,xl[i,1:n],l1,P[1])>naive(mu2,sig2,xl[i,1:n],l2,P[2]))
      class <- 1
    else
      class <- 2
    if (class!=xl[i,n+1]) {   #если класс не верный
      err <- err-1   #уменьшаем количество правильно классифицируемых объектов выборки
    }
  }
  x <- seq(-5,5,by=0.1)
  y <- x
  N <- function(X,Y) {
    v1 <- naive(mu1,sig1,c(X[arrayindex.x],Y[arrayindex.y]),l1,P[1])
    v2 <- naive(mu2,sig2,c(X[arrayindex.x],Y[arrayindex.y]),l2,P[2])
    return (v1-v2)
  }
  pnt <- array(NA,c(length(x),length(y),2))
  z <- matrix(NA,length(x),length(y))
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      pnt[i,j, ] <- c(x[i],y[j])
      z[i,j] <- naive(mu1,sig1,pnt[i,j, ],l1,P[1])-naive(mu2,sig2,pnt[i,j, ],l2,P[2])
    }
  }
  message <- paste0("Получено ",l-err," ошибок, качество классификатора: ",err/l*100,"%")
  contour(x,y,z,levels=c(0),drawlabels=FALSE,col="purple4",asp=1,lwd=2,main=message)
  points(xl[ ,1],xl[ ,2],pch=21,bg=c("skyblue1","pink1")[xl[ ,3]],asp=1)
  return (err/l*100)   #процентное значение качества классификации
}


main <- function() {
  mu1 <- c(1,2)
  mu2 <- c(-2,-3)
  sig1 <- matrix(c(3,1,1,1),2,2)
  sig2 <- matrix(c(1,0,0,4),2,2)
  xl1 <- mvrnorm(250,mu1,sig1)    #многомерное распределение точек классов
  xl2 <- mvrnorm(250,mu2,sig2)
  xl <- rbind(cbind(xl1,1),cbind(xl2,2))   #объединяем два множества точек, указав классы
  mu1h <- muHat(xl1)
  mu2h <- muHat(xl2)
  sig1h <- sigmaHat(xl1,mu1h)
  sig2h <- sigmaHat(xl2,mu2h)
  q <- quality(xl,mu1h,sig1h,mu2h,sig2h,1.0,1.0)
  print(paste0("Качество классификатора: ",q,"%"))
}
main()