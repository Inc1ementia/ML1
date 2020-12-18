library(MASS)
gPrev <<- numeric()
g <<- numeric()
Pi <<- numeric()


phi <- function(obj,mu,sigma,n=2) {   #вероятность принадлежности объекта паре (mu,sigma)
  mu <- matrix(mu,1,2)
  sigma <- matrix(sigma,2,2)
  return (1.0/sqrt((2*pi)^n*det(sigma))*exp(-0.5*(obj-mu)%*%solve(sigma)%*%t(obj-mu)))
}


muHat <- function(xl,k=1,w=1.0) {   #считаем значение мю по данным для класса
  n <- dim(xl)[2]-1
  mu <- array(NA,n)
  for (i in 1:n) {
    mu[i] <- mean(g[ ,k]*xl[ ,i])   #по каждой координате берём среднее
  }
  return (t(mu/w))
}


sigmaHat <- function(xl,mu,k=1,w=1.0) {   #считаем зачение сигма по данным для класса и полученному мю
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  sigma <- matrix(0,n,n)
  for (i in 1:l) {
    sigma <- sigma+g[i,k]*(t(xl[i,1:n]-mu)%*%(xl[i,1:n]-mu))
  }
  return (sigma/l/w)
}


RBFClassifier <- function(x,theta,lambda) {   #классификация RBF-сетью
  k <- dim(theta)[1]
  s <- length(lambda)   #число классов
  classes <- rep(0.0,s)
  for (j in 1:k) {   #для каждого класса считаем значение второго слоя сети
    mu <- matrix(theta[j,3:4],1,2)
    sigma <- matrix(theta[j,5:8],2,2)
    classes[theta[j,1]] <- classes[theta[j,1]]+phi(x,mu,sigma)*theta[j,2]   #переход от первого слоя сети ко второму
  }
  classes <- classes*lambda*Pi   #третий слой сети
  class <- which.max(classes)   #выбирается argmax
  return (class)
}


RBF <- function(xl,R,m0,delta) {
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  vars <- as.integer(names(table(xl[ ,n+1])))   #список имеющихся классов
  theta <- numeric()
  Pi <<- rep(0,length(vars))
  for (i in 1:length(vars)) {
    xli <- xl[which(xl[ ,n+1]==vars[i],arr.ind=TRUE), ]   #выбираем только точки данного класса
    Pi[i] <<- dim(xli)[1]
    theta <- rbind(theta,cbind(vars[i],EMDinamic(xli,R,m0,delta)))    #для очередного класса запускаем EM-алгоритм
  }
  return (theta)
}


EMDinamic <- function(xl,R,m0,delta) {
  k <- 1
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  gPrev <<- matrix(0.0,l,k)
  g <<- gPrev
  g[ ,1] <<- rep(1.0,l)    #исходно все точки принадлежат первому подмножеству
  theta <- matrix(0.0,k,n*n+n+1)
  mu <- muHat(xl)   #инициализация mu и sigma
  sigma <- sigmaHat(xl,mu)
  theta[1, ] <- c(1.0,c(mu),c(sigma))
  while (k<7) {
    prob <- matrix(0.0,l)
    w <- theta[ ,1]
    mu <- theta[ ,2:(n+1)]
    sigma <- theta[ ,(n+2):(n*n+n+1)]
    if (k==1) {   #на первом шаге нужно ещё не массивная структура - принудительное построение массивов
      w <- matrix(w,1,1)
      mu <- matrix(mu,1,length(mu))
      sigma <- matrix(sigma,1,length(sigma))
    }
    for (j in 1:k) {
      for (i in 1:l) {
        prob[i] <- prob[i]+w[j]*phi(xl[i,1:n],mu[j, ],sigma[j, ])   #подсчёт вероятностей каждого из объектов
      }
    }
    probMax <- max(prob)/R
    U <- which(prob<probMax)   #множество U плоховыделимых объектов
    if (length(U)<m0) break   #если объектов достаточно мало, то можно выходить
    theta <- rbind(theta,rep(0,n*n+n+1))
    k <- k+1
    theta[k,1] <- length(U)/l
    for (i in 1:(k-1)) {
      theta[i,1] <- theta[i,1]*(1-theta[k,1])   #пересчёт весов подмножеств
    }
    g <<- cbind(g,rep(0.0,l))
    g[U, ] <<- rep(0.0,k)
    g[U,k] <<- 1.0
    theta[k,2:(n+1)] <- c(muHat(xl,k,theta[k,1]))   #значение mu для нового подмножества
    theta[k,(n+2):(n*n+n+1)] <- c(sigmaHat(xl,matrix(theta[k,2:(n+1)],1,n),k,theta[k,1]))   #значение sigma для нового подмножества
    theta <- EMfixedK(xl,k,theta,delta)
  }
  return (theta)
}


EMfixedK <- function(xl,k,theta,delta) {
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  steps <- 0
  w <- theta[ ,1]
  mu <- theta[ ,2:(n+1)]
  sigma <- theta[ ,(n+2):(n*n+n+1)]
  while (TRUE) {
    steps <- steps+1
    #E-шаг
    gPrev <<- g   #сохраняется таблица скрытых параметров g
    for (i in 1:l) {
      sm <- 0.0
      for (s in 1:k) {
        sm <- sm+w[s]*phi(xl[i,1:n],mu[s, ],sigma[s, ])   #подсчёт знаменателя
      }
      for (j in 1:k) {
        g[i,j] <<- w[j]*phi(xl[i,1:n],mu[j, ],sigma[j, ])/sm   #пересчёт скрытых коэффициентов g
      }
    }
    #M-шаг
    for (j in 1:k) {
      w[j]<- sum(g[ ,j])/l   #пересчёт весов компонент
      mu[j, ] <- muHat(xl,j,w[j])   #пересчёт mu для каждой компоненты
      ss <- sigmaHat(xl,matrix(mu[j, ],1,2),j,w[j])   #пересчёт sigma для каждой компоненты
      if (det(matrix(ss,2,2))<1e-9) {   #если очень плохая матрица - лучше закончить процесс...
        break;
      }
      sigma[j, ] <- c(ss)
    }
    gg <- max(g-gPrev)   #определение останова алгоритма
    if (gg<=delta) break
  }
  for (j in 1:k) {
    theta[j, ] <- c(w[j],mu[j, ],sigma[j, ])
  }
  return (theta)
}


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


main <- function(objects,lambda) {
  colors <- c("pink1","gold","skyblue1")
  mu1 <- c(5,0)    #построение модельной выборки
  mu2 <- c(5,4)
  mu3 <- c(-1,1)
  mu4 <- c(2,3)
  sigma1 <- matrix(c(4,0,0,1)/10,2,2)
  sigma2 <- matrix(c(1,1,1,4)/10,2,2)
  sigma3 <- matrix(c(2,-1,-1,2)/10,2,2)
  sigma4 <- matrix(c(6,2,2,3)/10,2,2)
  xl1 <- mvrnorm(120,mu1,sigma1)
  xl2 <- mvrnorm(120,mu2,sigma2)
  xl3 <- mvrnorm(40,mu3,sigma3)
  xl4 <- mvrnorm(200,mu4,sigma4)
  xl <- rbind(cbind(xl1,1),cbind(xl2,2),cbind(xl3,2),cbind(xl4,3))
  theta <- RBF(xl,5,5,0.1)
  message <- "Классификация с помощью RBF-сети"
  plot(xl[ ,1],xl[ ,2],pch=21,col=colors[xl[ ,3]],bg=colors[xl[ ,3]],asp=1,main=message,xlab="x",ylab="y")
  k <- dim(theta)[1]
  x <- seq(-10,10,by=0.02)
  y <- x
  for (i in 1:k) {   #для каждого подможества строим линию уровня, увеличивая толщину линии в зависимости от значимости
    mu <- matrix(theta[i,3:4],1,2)
    sigma <- matrix(theta[i,5:8],2,2)
    sick <- 1
    if (theta[i,2]>0.2) sick <- 2
    if (theta[i,2]>0.4) sick <- 3
    if (theta[i,2]>=0.05)
      contour(x,y,levelLine(mu,sigma,x,y),lwd=sick,col=colors[theta[i,1]],asp=1,levels=0.01,drawlabels=FALSE,add=TRUE)
  }
  k <- dim(objects)[1]
  for (i in 1:k) {    #классификация некоторого набора объектов с помощью RBF-сети
    class <- RBFClassifier(objects[i, ],theta,lambda)
    points(objects[i,1],objects[i,2],pch=c(21,22,24)[class],col="black",bg=colors[class],asp=1)
  }
}
main(matrix(c(-2,2,0,4,5,1,3,2,2,1,2,1),6,2),c(1/6,1/6,2/3))