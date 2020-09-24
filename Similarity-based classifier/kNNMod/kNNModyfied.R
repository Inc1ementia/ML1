colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1")
optimalK <<- 2
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


meanDist <- function(xl,z,k,metricFunc=eucDist) {   #
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  dist <- matrix(NA,l,2)
  for (i in 1:l) {
    dist[i, ] <- c(i, metricFunc(xl[i,1:n],z))  #создание списка пар (номер объекта, расстояние до z)
  }
  orderedDist <- dist[order(dist[, 2]), ]   #сортировка списка объектов
  return (orderedDist[1:k,2])
}


subclass <- function(xl,type) {   #выбор определённого класса цветов
  n <- dim(xl)[2]
  return (xl[which(xl[ ,n]==type), ])
}


kNNMod <- function(xl,z,k) {   #функция выбора класса методов kNN
  types <- c("setosa","versicolor","virginica")
  n <- dim(xl)[2]-1
  classDist <- c(0,0,0)
  for (i in 1:3) {
    current <- subclass(xl,types[i])   #для каждого класса получает список цветов
    classDist[i] <- mean(meanDist(current,z,k))   #берёт среднее расстояние до ближайших k представителей класса
  }
  class <- types[which.min(classDist)]     #выбирает тот класс, у которого "средний" объект ближе всего к z
  return (class)
}


determKNNMod_LOO <- function(xl) {   #определение оптимального k методом LOO
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  classes <- xl[ ,n+1]
  counts <- table(classes)
  maxK <- counts[which.min(counts)]-1   #максимальное допустимое k = минимальное число в одном классе
  errorForK <- matrix(0.0,maxK,1)
  for (i in 1:l) {  #перебираем каждый элемент
    obj <- xl[i, ]
    newXl <- xl[-i, ]  #убираем его из выборки
    z <- obj[1:n]
    classes <- c("setosa"=0,"versicolor"=0,"virginica"=0)
    dist <- matrix(NA,3,maxK)
    for (j in 1:3) {
      class <- subclass(newXl,names(classes[j]))   #для каждого класса получает список цветов
      dist[j, ] <- meanDist(class,z,maxK)   #берёт среднее расстояние до всех представителей класса
    }
    print(i)
    for (k in 1:maxK) {   #перебираем значение k
      for (j in 1:3) {
        classes[j] <- classes[j]+dist[j,k]   #в каждый класс добавляется расстояние до k-го объекта класса
      }
      class <- names(which.min(classes))   #выбирается класс с наименьшим расстоянием
      if (class!=obj[n+1]) {   #если произошла ошибка классификации
        errorForK[k]=errorForK[k]+1/l   #то увеличиваем для данного k значение ошибки
      }
    }
  }
  LOO <- matrix(NA,maxK,2)
  for (k in 1:maxK) {
    LOO[k, ] <- c(k,errorForK[k])   #строим массив пар (номер, число ошибок)
  }
  optK <- which.min(errorForK)
  plot(LOO,pch=1,type="l",col="blue",xlab="k",ylab="LOO(k)",main="Find optimal value of k for kNNMod with LOO-algo")
  points(optK,LOO[optK,2],pch=21,bg="red",col="red")
  res <- paste("optK=",optK)   #готовит подпись для графика
  text(optK+1.3,LOO[optK,2]+0.001,labels=res)
  return (c(optK,errorForK[optK]))
}


algoShow <- function(z) {
  k <- optimalK
  types <- c("setosa","versicolor","virginica")
  xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  classDist <- c(0,0,0)
  globalRemove <- matrix(NA,3*k,4)
  pos <- 1
  for (i in 1:3) {
    current <- subclass(xl,types[i])   #для каждого класса получает список цветов
    orderedClass <- sortObjbyDist(current,z)   #берёт ближайших k представителей класса
    classDist[i] <- mean(meanDist(current,z,k))
    remove <- strtoi(dimnames(orderedClass[1:k, ])[[1]])  #получить номера тех, кто является ближайшими соседями
    for (j in 1:k) {   #запоминаем информацию о каждом удаляемом объекте
      globalRemove[pos, ] <- c(remove[j],orderedClass[j,1],orderedClass[j,2],types[i])
      pos <- pos+1
    }
  }
  remove <- strtoi(globalRemove[ ,1])
  newXl <- xl[-remove, ]
  class <- types[which.min(classDist)]     #выбирает тот класс, у которого "средний" объект ближе всего к z
  message <- paste("Point (",z[1],",",z[2],") is of class",class)
  plot(newXl[ ,1:n],pch=21,bg=colors[newXl[ ,n+1]],col=colors[newXl[ ,n+1]],asp=1,main=message,xlab=xname,ylab=yname)
  points(z[1],z[2],pch=22,bg=colors[class],col=colors[class],asp=1)
  points(globalRemove[ ,2:3],pch=21,bg=colors[globalRemove[ ,4]],col="black",asp=1)
}


main <- function(runLOO=FALSE,runMap=FALSE) {
  errorValue <- 0.04
  xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
  xMin <- xl[which.min(xl[ ,1]),1]   #нахождение минимального и максимального иксов
  xMax <- xl[which.max(xl[ ,1]),1]
  X <- seq(from=xMin,to=xMax,by=0.05)   #список всех иксов на карте
  xLen <- length(X)
  yMin <- xl[which.min(xl[ ,2]),2]   #нахождение минимального и максимального игриков
  yMax <- xl[which.max(xl[ ,2]),2]
  Y <- seq(from=yMin,to=yMax,by=0.05)   #список всех игриков на карте
  yLen <-length(Y)
  if (runLOO==TRUE) {
    optimals <- determKNNMod_LOO(xl)   #находим оптимальное k
    optimalK <<- optimals[1]
    errorValue <- optimals[2]
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
        flowers[j,i] <- kNNMod(xl,z,optimalK)   #находим предположительный класс методом kNN
        cnt <- cnt+1
      }
    }
    message <- paste("Map of kNNMod for optimal K =",optimalK,"with error =",errorValue)
    plot(positions,pch=1,bg="white",col=colors[flowers],asp=1,main=message,xlab=xname,ylab=yname)
    points(iris[ ,c(3,4)],pch=21,bg=colors[iris$Species],col=colors[iris$Species],asp=1)
  }
}

main(FALSE,FALSE)	#посчитать отдельно LOO и полную карту
algoShow(c(3.8,2.2))	#показать классификацию случайной точки, подсветив её соседей
