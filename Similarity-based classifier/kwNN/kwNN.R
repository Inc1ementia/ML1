library(lattice)
colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1")
qPow <<- numeric()
optimalK <<- 3
optimalQ <<- 26
maxQ <- 0.9
minQ <- 0.1
parOne <- 3   #чтобы можно было легко поменять параметры
parTwo <- 4
titles <- dimnames(iris)[2]
xname <- titles[[1]][parOne]   #названия для подписей на карте
yname <- titles[[1]][parTwo]


eucDist <- function(u,v) {  #функция расстояния между парой точек
  return (sqrt(sum((u-v)^2)))
}


sortObjbyDist <- function(xl,z,metricFunc=eucDist) {  #функция сортировки массива по расстоянию до z
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  dist <- matrix(NA,l,2)
  for (i in 1:l) {
    dist[i, ] <- c(i, metricFunc(xl[i,1:n],z))  #создание списка пар (номер объекта, расстояние до z)
  }
  orderedXl <- xl[order(dist[ ,2]), ]   #сортировка списка объектов
  return (orderedXl)
}


kwNN <- function(xl,z,optK,optQ) {
  orderedXl <- sortObjbyDist(xl,z)
  n <- dim(orderedXl)[2]
  classes <- orderedXl[1:optK,n]   #получает список классов для ближайших k объектов
  counts <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0)
  for (i in 1:optK) {
    counts[classes[i]] <- counts[classes[i]]+qPow[i,optQ];
  }
  class <- names(which.max(counts))   #выбирает тот класс, у которого больше всего представителей
  return (class)
}


determkwNN_LOO <- function(xl,qCount) {
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  maxK <- l-1
  X <- c(1:maxK)
  Y <- seq(qPow[1,1],qPow[1,qCount],qPow[1,2]-qPow[1,1])
  errorForK <- matrix(0.0,maxK,qCount,dimnames=list(X,Y))
  for (i in 1:l) {  #перебираем каждый элемент
    obj <- xl[i, ]
    newXl <- xl[-i, ]  #убираем его из выборки
    z <- obj[1:n]
    orderedXl <- sortObjbyDist(newXl,z)  #находим расстояния до всех объектов
    print(i)
    for (q in 1:qCount) {   #перебираем значение q
      classes <- c("setosa"=0,"versicolor"=0,"virginica"=0)
      for (k in 1:maxK) {    #перебираем значение k
        classes[orderedXl[k,n+1]] <- classes[orderedXl[k,n+1]]+qPow[k,q]  #добавляем k-ый элемент
        class <- names(which.max(classes))  #выбирает тот вид, у которого вес класса больше
        if (class!=obj[n+1]) {   #если произошла ошибка классификации
          errorForK[k,q]=errorForK[k,q]+1/l   #то увеличиваем для данной пары (k,q) значение ошибки
        }
      }
    }
  }
  print(levelplot(errorForK,col.regions=heat.colors(100),main="LOO(k,q) level plot",xlab="k",ylab="q",xlim=seq(1,maxK,10),asp=1))
  optK <- 1
  optQ <- 1
  for (k in 1:maxK) {
    for (q in 1:qCount) {
      if (errorForK[k,q]<errorForK[optK,optQ]) {
        optK <- k
        optQ <- q
      }
    }
  }
  return (c(optK,optQ,errorForK[optK,optQ]))
}


algoShow <- function(z) {
  k <- optimalK
  q <- optimalQ
  xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
  l <- dim(xl)[1]
  orderedXl <- sortObjbyDist(xl,z)
  n <- dim(orderedXl)[2]-1
  classes <- orderedXl[1:k,n+1]   #получает список классов для всех объектов
  counts <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0)
  for (i in 1:k) {
    counts[classes[i]] <- counts[classes[i]]+qPow[i,q]
  }
  class <- names(which.max(counts))   #выбирает тот класс, у которого вес больше
  remove <- strtoi(dimnames(orderedXl[1:k, ])[[1]])  #получить номера тех, кто является ближайшими соседями
  newXl <- xl[-remove, ]    #убрать ближайших соседей из общего списка
  message <- paste("Point (",z[1],",",z[2],") is of class",class)
  plot(newXl[ ,1:n],pch=21,bg=colors[newXl[ ,n+1]],col=colors[newXl[ ,n+1]],asp=1,main=message,xlab=xname,ylab=yname)
  points(z[1],z[2],pch=22,bg=colors[class],col=colors[class],asp=1)
  points(orderedXl[1:k,1:n],pch=21,bg=colors[orderedXl[1:k,n+1]],col="black",asp=1)
}


main <- function(runLOO=FALSE,runMap=FALSE) {
  errorValue <- 0.04
  optQ <- 0.62
  xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
  xMin <- xl[which.min(xl[ ,1]),1]   #нахождение минимального и максимальнго иксов
  xMax <- xl[which.max(xl[ ,1]),1]
  X <- seq(from=xMin,to=xMax,by=0.05)   #список всех иксов на карте
  xLen <- length(X)
  yMin <- xl[which.min(xl[ ,2]),2]   #нахождение минимального и максимального игриков
  yMax <- xl[which.max(xl[ ,2]),2]
  Y <- seq(from=yMin,to=yMax,by=0.05)   #список всех игриков на карте
  yLen <-length(Y)
  qCount <- 41
  l <- dim(xl)[1]-1
  qPow <<- matrix(NA,l,qCount)
  for (i in 1:qCount) {
    q <- (maxQ-minQ)/(qCount-1)*(i-1)+minQ
    qPow[1,i] <<- q
    for (j in 2:l) {
      qPow[j,i] <<- qPow[j-1,i]*q
    }
  }
  if (runLOO==TRUE) {
    optimals <- determkwNN_LOO(xl,qCount)
    optimalK <<- optimals[1]
    optimalQ <<- optimals[2]
    errorValue <- optimals[3]
    optQ <- (maxQ-minQ)/(qCount-1)*(optimalQ-1)+minQ
  }
  if (runMap==TRUE) {
    flowers <- matrix(NA,xLen,yLen)
    positions <- matrix(NA,xLen*yLen,2)
    cnt <- 1
    for (i in 1:yLen) {
      print(Y[i])
      for (j in 1:xLen) {
        z <- c(X[j],Y[i])
        positions[cnt, ] <- z   #запоминаем координаты объекта
        flowers[j,i] <- kwNN(xl,z,optimalK,optimalQ)   #находим предположительный класс методом kNN
        cnt <- cnt+1
      }
    }
    message <- paste("Map of kwNN for K =",optimalK,"and Q =",optQ,"with error =",errorValue)
    plot(positions,pch=1,bg="white",col=colors[flowers],asp=1,main=message,xlab=xname,ylab=yname)
    points(iris[ ,c(parOne,parTwo)],pch=21,bg=colors[iris$Species],col=colors[iris$Species],asp=1)
  }
}
main(FALSE,FALSE)	#посчитать отдельно LOO и полную карту
algoShow(c(5.2,1.3))	#показать классификацию случайной точки, подсветив её соседей
