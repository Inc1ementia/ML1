library(scatterplot3d)
colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1")
qPow <<- numeric()
optimalK <<- 6
optimalQ <<- -1
maxQ <- 1.0
minQ <- 0.05
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
    dist[i, ] <- c(i,metricFunc(xl[i,1:n],z))  #создание списка пар (номер объекта, расстояние до z)
  }
  orderedXl <- xl[order(dist[ ,2]), ]   #сортировка списка расстояний
  return (orderedXl)
}


kwNN <- function(xl,z,optK,optQ) {
  orderedXl <- sortObjbyDist(xl,z)   #получает отсортированный список объектов
  n <- dim(orderedXl)[2]
  classes <- orderedXl[1:optK,n]   #выбирает классы ближайших k объектов
  counts <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0)
  for (i in 1:optK) {
    counts[classes[i]] <- counts[classes[i]]+qPow[i,optQ];
  }
  class <- names(which.max(counts))   #выбирает тот класс, у которого больше всего представителей
  return (class)
}


determkwNN_LOO <- function(xl,Q) {
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  maxK <- l-1
  qCount <- length(Q)
  errorForK <- matrix(0.0,maxK,qCount,dimnames=list(c(1:maxK),Q))
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
  Table <- matrix(NA,maxK*qCount,3)
  graphPoints <- matrix(NA,maxK*qCount,3)
  for (i in 1:maxK) {
    pos <- (i-1)*qCount
    for (j in 1:qCount) {
      Table[pos+j,1] <- i
      Table[pos+j,2] <- Q[j]
      Table[pos+j,3] <- errorForK[i,j]
    }
  }
  message <- paste0("Find optimal value of k for kNNMod with LOO-algo, k = ",optK,", error = ",(LOO[optK,2]*l),"/",l," (",round(LOO[optK,2],3),")")
  LOO <- scatterplot3d(
    x = Table[ ,1],
    y = Table[ ,2],
    z = Table[ ,3],
    angle=100,
    col.axis="blue",
    col.grid="lightblue",
    type = "n",
    xlab = "k",
    ylab = "q",
    zlab = "LOO(k,q)",
    main = message
  )
  for (i in 1:qCount) {
    for (j in 1:maxK) {
      for (k in 1:qCount) {
        graphPoints[(j-1)*qCount+k, ] <- c(j,Q[i],errorForK[j,i])
      }
      LOO$points3d(graphPoints[ ,1],graphPoints[ ,2],graphPoints[ ,3],type="l")
    }
  }
  for (i in 1:maxK) {
    for (k in 1:qCount) {
      graphPoints[k, ] <- c(i,Q[k],errorForK[i,k])
    }
    for (j in 2:maxK) {
      for (k in 1:qCount) {
        graphPoints[(j-1)*qCount+k, ] <- graphPoints[qCount, ]
      }
    }
    LOO$points3d(graphPoints[ ,1],graphPoints[ ,2],graphPoints[ ,3],type="l")
  }
  for (i in 1:maxK) {
    for (j in 1:qCount) {
      graphPoints[(i-1)*qCount+j, ] <- c(optK,Q[optQ],errorForK[optK,optQ])
    }
  }
  LOO$points3d(graphPoints[ ,1],graphPoints[ ,2],graphPoints[ ,3],pch=20,col="red");
  message <- paste("k =",optK,", q =",Q[optQ])
  text(LOO$xyz.convert(optK,Q[optQ],errorForK[optK,optQ]+0.05),labels=message,col="red");
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
  errorValue <- 1.0/30.0
  optQ <- 1.0
  ptm <- proc.time()
  xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
  l <- dim(xl)[1]
  xMin <- xl[which.min(xl[ ,1]),1]   #нахождение минимального и максимального иксов
  xMax <- xl[which.max(xl[ ,1]),1]
  X <- seq(from=xMin,to=xMax,by=0.05)   #список всех иксов на карте
  xLen <- length(X)
  yMin <- xl[which.min(xl[ ,2]),2]   #нахождение минимального и максимального игриков
  yMax <- xl[which.max(xl[ ,2]),2]
  Y <- seq(from=yMin,to=yMax,by=0.05)   #список всех игриков на карте
  yLen <-length(Y)
  Q <- seq(from=minQ,to=maxQ,by=0.05)  #список всех значений веса
  qCount <- length(Q)
  qPow <<- matrix(NA,l,qCount)
  for (i in 1:qCount) {
    q <- Q[i]
    if (q<=1.0) optimalQ <<- i
    qPow[1,i] <<- q
    for (j in 2:l) {
      qPow[j,i] <<- qPow[j-1,i]*q
    }
  }
  if (runLOO==TRUE) {
    optimals <- determkwNN_LOO(xl,Q)
    print("LOO algorithm")
    btm <- proc.time()
    print(btm-ptm)
    optimalK <<- optimals[1]
    optimalQ <<- optimals[2]
    errorValue <- optimals[3]
    optQ <- Q[optimalQ]
  }
  if (runMap==TRUE) {
    ptm <- proc.time()
    flowers <- matrix(NA,xLen,yLen)
    positions <- matrix(NA,xLen*yLen,2)
    cnt <- 1
    for (i in 1:yLen) {
      print(Y[i])
      for (j in 1:xLen) {
        z <- c(X[j],Y[i])
        positions[cnt, ] <- z   #запоминаем координаты объекта
        flowers[j,i] <- kwNN(xl,z,optimalK,optimalQ)   #находим предположительный класс методом kwNN
        cnt <- cnt+1
      }
    }
    message <- paste0("Map of kwNN for K = ",optimalK," and Q = ",optQ," with error = ",(errorValue*l),"/",l," (",round(errorValue,3),")")
    plot(positions,pch=1,bg="white",col=colors[flowers],asp=1,main=message,xlab=xname,ylab=yname)
    points(iris[ ,c(parOne,parTwo)],pch=21,bg=colors[iris$Species],col=colors[iris$Species],asp=1)
    print("Map build")
    btm <- proc.time()
    print(btm-ptm)
  }
}


main(FALSE,FALSE)	#посчитать отдельно LOO и полную карту
algoShow(c(5.2,1.3))	#показать классификацию случайной точки, подсветив её соседей
