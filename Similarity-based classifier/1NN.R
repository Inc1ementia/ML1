colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1")
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


kNN <- function(xl,z) {   #функция выбора класса методом 1NN
  orderedXl <- sortObjbyDist(xl,z)
  n <- dim(orderedXl)[2]-1
  counts <- table(orderedXl[1,n+1])
  class <- names(which.max(counts))   #выбирает класс ближайшего объекта
  return (class)
}


determKNN_LOO <- function(xl) {   #определение оптимального k методом LOO
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  maxK <- l-1
  errorForK <- 0
  for (i in 1:l) {  #перебираем каждый элемент
    obj <- xl[i, ]
    newXl <- xl[-i, ]  #убираем его из выборки
    z <- obj[1:n]
    class <- kNN(newXl,z)
    if (class!=obj[n+1]) {
      errorForK <- errorForK+1/l
    }
  }
  return (errorForK)
}


main <- function() {
  xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
  xMin <- xl[which.min(xl[ ,1]),1]   #нахождение минимального и максимальнго иксов
  xMax <- xl[which.max(xl[ ,1]),1]
  X <- seq(from=xMin,to=xMax,by=0.05)   #список всех иксов на карте
  xLen <- length(X)
  yMin <- xl[which.min(xl[ ,2]),2]   #нахождение минимального и максимального игриков
  yMax <- xl[which.max(xl[ ,2]),2]
  Y <- seq(from=yMin,to=yMax,by=0.05)   #список всех игриков на карте
  yLen <-length(Y)
  flowers <- matrix(NA,xLen,yLen)
  positions <- matrix(NA,xLen*yLen,2)
  optK <- determKNN_LOO(xl)   #находим оптимальное k
  cnt <- 1
  for (i in 1:yLen) {
    print(Y[i])
    for (j in 1:xLen) {
      z <- c(X[j],Y[i])
      positions[cnt, ] <- z   #запоминаем координаты объекта
      flowers[j,i] <- kNN(xl,z)   #находим предположительный класс методом kNN
      cnt <- cnt+1
    }
  }
  title <- paste("Map of 1NN with error =",optK)
  plot(positions,pch=1,bg="white",col=colors[flowers],asp=1,main=title,xlab=xname,ylab=yname)
  points(iris[ ,c(parOne,parTwo)],pch=21,bg=colors[iris$Species],col=colors[iris$Species],asp=1)
}
main()