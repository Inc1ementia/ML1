eucDist <- function(u,v) {  #функция расстояния между парой точек
  return (sqrt(sum((u-v)^2)))
}
colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1")
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
kNN <- function(xl,z,k) {   #функция выбора класса методов kNN
  orderedXl <- sortObjbyDist(xl,z)
  n <- dim(orderedXl)[2]-1
  classes <- orderedXl[1:k,n+1]   #получает список видов для ближайших k объектов
  counts <- table(classes)   #строит из них таблицу количества цветов каждого вида
  class <- names(which.max(counts))   #выбирает тот вид, у которого больше всего представителей
  return (class)
}
determKNN_LOO <- function(xl) {   #определение оптимального k методом LOO
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  maxK <- l-1
  errorForK <- matrix(0.0,maxK,1)
  for (i in 1:l) {  #перебираем каждый элемент
    obj <- xl[i, ]
    newXl <- xl[-i, ]  #убираем его из выборки
    z <- obj[1:n]
    orderedXl <- sortObjbyDist(newXl,z)  #находим расстояния до всех объектов
    classes <- c("setosa"=0,"versicolor"=0,"virginica"=0)
    print(i)
    for (k in 1:maxK) {   #перебираем значение k
      classes[orderedXl[k,n+1]] <- classes[orderedXl[k,n+1]]+1  #добавляем k-ый элемент в свою группу
      class <- names(which.max(classes))  #выбирает тот вид, у которого больше всего представителей
      if (class!=obj[n+1]) {   #если произошла ошибка классификации
        errorForK[k]=errorForK[k]+1   #то увиличиваем для данного k значение ошибки
      }
    }
  }
  LOO <- matrix(NA,maxK,2)
  for (k in 1:maxK) {
    LOO[k, ] <- c(k,errorForK[k])   #строим массив пар (номер, число ошибок)
  }
  optK <- which.min(errorForK)  #выбираем оптимальное k
  plot(LOO,pch=1,type="l",col="blue",xlab="k",ylab="LOO(k)",main="Find optimal value of k for kNN with LOO-algo")
  points(optK,LOO[optK,2],pch=21,bg="red",col="red")
  res <- paste("optK=",optK)   #готовит подпись для графика
  text(optK,LOO[optK,2]+7,labels=res)
  return (optK)
}
main <- function() {
  parOne <- 3   #чтобы можно было легко поменять параметры
  parTwo <- 4
  titles <- dimnames(iris)[2]
  xname <- titles[[1]][parOne]   #названия для подписей на карте
  yname <- titles[[1]][parTwo]
  xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
  xMin <- xl[which.min(xl[ ,1]),1]   #нахождение минимального и максимального иксов
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
      flowers[j,i] <- kNN(xl,z,optK)   #находим предположительный класс методом kNN
      cnt <- cnt+1
    }
  }
  plot(positions,pch=1,bg="white",col=colors[flowers],asp=1,main="Map of kNN for optimal K",xlab=xname,ylab=yname)
  points(iris[ ,c(parOne,parTwo)],pch=21,bg=colors[iris$Species],col=colors[iris$Species],asp=1)
}
main()
