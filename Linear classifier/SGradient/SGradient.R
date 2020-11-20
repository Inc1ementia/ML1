dataNorm <- function(xl) {
  n <- dim(xl)[2]
  for (i in 1:n) {
    mn <- min(xl[ ,i])
    mx <- max(xl[ ,i])
    if (mx>mn) {
      xl[ ,i] <- (xl[ ,i]-mn)/(mx-mn)
    }
  }
  return (xl)
}


randomWeight <- function(n) {
  return (runif(n,min=-0.5/n,max=0.5/n))
}


zeroWeight <- function(n) {
  return (array(0.0,n))
}


adalineL <- function(w,x) {   #L для ADALINE
  n <- length(w)
  return ((c(w%*%x[1:n])*x[n+1]-1)^2)
}


adalineLL <- function(w,x) {   #L' для ADALINE
  n <- length(w)
  return (x[1:n]*(2.0*(c(w%*%x[1:n])*x[n+1]^2-x[n+1])))
}


hebbL <- function(w,x) {   #L для правила Хэбба
  n <- length(w)
  return (max(-c(w%*%x[1:n])*x[n+1],0))
}


hebbLL <- function(w,x) {   #L' для правила Хэбба
  n <- length(w)
  return (-x[1:n]*x[n+1])
}


logisticL <- function(w,x) {   #L для логистической регрессии
  n <- length(w)
  return (log2(1+exp(-c(w%*%x[1:n])*x[n+1])))
}


logisticLL <- function(w,x) {   #L' для логистической регрессии
  n <- length(w)
  return (x[1:n]*x[n+1]*(-1.0/(exp(c(w%*%x[1:n])*x[n+1])*log(2)+log(2))))
}


error <- function(w,objects,func) {   #функция подсчёта ошибки
  res <- 0
  l <- dim(objects)[1]
  for (i in 1:l) {
    res <- res+func(w,objects[i, ])
  }
  return (res)
}


findErrors <- function(w,objects,func) {   #функция поиска всех неправильно классифицируемых объектов для Хэбба
  res <- numeric()
  l <- dim(objects)[1]
  for (i in 1:l) {
    margin <- func(w,objects[i, ])
    if (margin>0 || c(w%*%objects[1:length(w)])==0)   #нулевое значение <w,x> или положительный отступ - ошибка
      res <- c(res,i)
  }
  return (res)
}


step <- function(w,obj,func) {   #градиентный шаг
  res <- func(w,obj)
  return (res)
}


gradient <- function(xl,lambda,funcL,funcLL,rule,weightInit=randomWeight) {
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  w <- weightInit(n)   #инициализируем массив весов
  Q <- error(w,xl,funcL)    #считаем стартовое значение эмперического риска
  Qprev <- array(Q-100.0,10)   #массив для проверки нормализации эмпирического риска
  class <- sample(c(-1,1),1)    #один из случайных классов, для чередования
  steps <- 0
  while (TRUE) {
    steps <- steps+1
    if (rule=="H") {   #для правила Хэбба берём только плохие
      errored <- findErrors(w,xl,funcL)   #список плохих объектов
      if (length(errored)==0) break
      obj <- sample(errored,1)    #выбираем случайный
    } else {
      obj <- sample(which(xl[ ,n+1]==class,arr.ind=TRUE),1)   #выбираем случайный из предложенного класса
    }
    eps <- error(w,matrix(xl[obj, ],1,n+1),funcL)   #считаем ошибку на объекте
    if (rule=="L") {
      nu <- 1/sqrt(sum(xl[obj,1:n]*xl[obj,1:n]))
    } else {
      nu <- 1/steps
    }
    w <- w-nu*step(w,xl[obj, ],funcLL)   #и делаем градиентный спуск
    Q <- (1-lambda)*Q+lambda*eps    #пересчитываем значение эмперического риска
    class <- -class    #меняем класс на противоположный
    if (abs(mean(Qprev)-Q)<1e-3) {   #эмпирический риск стабилизировался
      break
    } else {
      Qprev <- c(Q,Qprev[1:9])
    }
    if (steps==10000) break    #слишком много шагов
  }
  return (w)
}


main <- function() {
  xl <- matrix(0.0,100,4)
  for (i in 1:100) {
    xl[i, ] <- c(iris[i+50,3],iris[i+50,4],-1,ifelse(i>50,1,-1))
  }
  xl <- cbind(dataNorm(xl[ ,1:2]),xl[ ,3:4])
  colors <- c("pink1","skyblue1","limegreen","firebrick1","darkviolet")
  message <- "Линейные классификаторы ADALINE, правило Хэбба и логистическая регрессия"
  plot(xl[ ,1],xl[ ,2],pch=21,bg=ifelse(xl[ ,4]==1,colors[1],colors[2]),col="black",asp=1,main=message)
  weight <- gradient(xl,0.1,adalineL,adalineLL,"A")
  abline(weight[3]/weight[2],-weight[1]/weight[2],col=colors[3],lwd=2,asp=1)
  weight <- gradient(xl,0.1,hebbL,hebbLL,"H")
  abline(weight[3]/weight[2],-weight[1]/weight[2],col=colors[4],lwd=2,asp=1)
  weight <- gradient(xl,0.1,logisticL,logisticLL,"L")
  abline(weight[3]/weight[2],-weight[1]/weight[2],col=colors[5],lwd=2,asp=1)
  legend(0.9,0.4,c("ADALINE","Hebb","LogIt"),pch=c("l","l","l"),col=colors[3:5])
}
main()
