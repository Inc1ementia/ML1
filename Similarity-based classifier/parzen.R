colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1","unknown"="azure4")
optimalH <<- numeric()
parOne <- 3   #чтобы можно было легко поменять параметры
parTwo <- 4
titles <- dimnames(iris)[2]
xname <- titles[[1]][parOne]   #названия для подписей на карте
yname <- titles[[1]][parTwo]


eucDist <- function(u,v) {  #функция расстояния между парой точек
  return (sqrt(sum((u-v)^2)))
}


distSort <- function(xl,z,metricFunc=eucDist) {  #функция сортировки массива по расстоянию до z
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  dist <- matrix(NA,l,3)
  for (i in 1:l) {
    dist[i, ] <- c(i, metricFunc(xl[i,1:n],z),xl[i,n+1])  #создание списка пар (номер объекта, расстояние до z)
  }
  orderedXl <- dist[order(dist[ ,2]), ]   #сортировка списка объектов
  return (orderedXl)
}


#LIST OF KERNELS - START
triangleKernel <- function(dist,h) {
  if (dist<=h) {
    return ((h-dist)/h)   #ядро нормированное: 0<=K(x)<=1
  } else {
    return (0.0)
  }
}


rectKernel <- function(dist,h) {
  if (dist<=h) {
    return (0.5)
  } else {
    return (0.0)
  }
}


EpanechnikovKernel <- function(dist,h) {
  if (dist<=h) {
    return (0.75*(1.0-dist^2))
  } else {
    return (0.0)
  }
}


GaussianKernel <- function(dist,h) {
  return (exp(-0.5*(dist*h)^2)*sqrt(h/pi*0.5))
}
#LIST OF KERNELS - END


kernelList <<- c("Triangle"=triangleKernel,"Rectangle"=rectKernel,"Epanechnikov"=EpanechnikovKernel,"Gaussian"=GaussianKernel)


parzen <- function(xl,z,h,kernelType) {		#функция выбора класса методом парзеновского окна
  l <- dim(xl)[1]
  orderedXl <- distSort(xl,z)    #получаем отсортированные расстояния до объектов
  classes <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0,"unknown"=1e-7)   #три класса и неопределённость
  for (i in 1:l) {   #для каждого объекта определяем значение ядровой функции и прибавляем к весу класса
    classes[orderedXl[i,3]] <- classes[orderedXl[i,3]]+kernelType(orderedXl[i,2],h)
  }
  class <- names(which.max(classes))  #относим к тому классу, чей вес больше
  return (class)
}


parzenMulty <- function(xl,z,h,kernels) {		#оптимизированная функция, считает точку сразу по всем ядрам
  l <- dim(xl)[1]
  kLen <- length(h)
  orderedXl <- distSort(xl,z)    #получаем отсортированные расстояния до объектов
  res <- c(1:kLen)
  for (k in 1:kLen) {
    curKernel <- kernels[[k]]   #выбераем k-ое ядро из списка
    H <- h[k]
    classes <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0,"unknown"=1e-7)   #три класса и неопределённость
    for (i in 1:l) {   #для каждого объекта определяем значение ядровой функции и прибавляем к весу класса
      classes[orderedXl[i,3]] <- classes[orderedXl[i,3]]+curKernel(orderedXl[i,2],H)
    }
    res[k] <- names(which.max(classes))  #относим к тому классу, чей вес больше
  }
  return (res)
}


determParzenH_LOO <- function(xl,kernels,hSeq) {	#определение оптимального h методом LOO
  l <- dim(xl)[1]
  ml <- l-1
  n <- dim(xl)[2]-1
  hLen <- length(hSeq)
  kLen <- length(kernels)
  errorForH <- matrix(0.0,hLen,kLen)
  for (i in 1:l) {  #перебираем каждый элемент
    obj <- xl[i, ]
    newXl <- xl[-i, ]  #убираем его из выборки
    z <- obj[1:n]
    orderedXl <- distSort(newXl,z)  #находим расстояния до всех объектов
    print(i)
    for (h in 1:hLen) {   #перебираем значение h
      for (k in 1:kLen) {
        curKernel <- kernels[[k]]
        classes <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0)
        for (j in 1:ml) {
          bonus <- curKernel(orderedXl[j,2],hSeq[h])
          classes[orderedXl[j,3]] <- classes[orderedXl[j,3]]+bonus
        }
        class <- names(which.max(classes))
        if (class!=obj[n+1]) {   #если произошла ошибка классификации
          errorForH[h,k]=errorForH[h,k]+1/l   #то увеличиваем для данного k значение ошибки
        }
      }
    }
  }
  answer <- matrix(NA,kLen,2)
  for (k in 1:kLen) {
    LOO <- matrix(NA,hLen,2)
    for (h in 1:hLen) {
      LOO[h, ] <- c(hSeq[h],errorForH[h,k])   #строим массив пар (номер, число ошибок)
    }
    optH <- which.min(errorForH[ ,k])  #выбираем оптимальное h
    message <- paste0("Find optimal value of h = ",hSeq[optH]," for ",names(kernels[k])," kernel for Parzen with LOO-algo, error = ",errorForH[optH,k]*l,"/",l," (",round(errorForH[optH,k],3),")")
    plot(LOO,pch=1,type="l",col="blue",xlab="h",ylab="LOO(h)",main=message)
    points(hSeq[optH],LOO[optH,2],pch=21,bg="red",col="red")
    res <- paste("optH=",hSeq[optH])   #готовит подпись для графика
    text(hSeq[optH]+0.03,LOO[optH,2]+errorForH[which.max(errorForH[ ,k]),k]/0.3*0.01,labels=res)
    answer[k, ] <- c(optH,errorForH[optH,k])
  }
  return (answer)
}


main <- function() {
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
  h <- seq(0.1,eucDist(c(xMin,yMin),c(xMax,yMax)),0.1)
  optimals <- determParzenH_LOO(xl,kernelList,h)   #находим оптимальное k
  kLen <- length(kernelList)
  print("LOO algorithm")
  btm <- proc.time()
  print(btm-ptm)
  btm <- ptm
  flowers <- array(NA,c(kLen,xLen,yLen))
  positions <- matrix(NA,xLen*yLen,2)
  optH <- h[optimals[ ,1]]
  cnt <- 1
  for (i in 1:yLen) {
    print(Y[i])
    for (j in 1:xLen) {
      z <- c(X[j],Y[i])
      positions[cnt, ] <- z
      flowers[ ,j,i] <- parzenMulty(xl,z,optH,kernelList)
      cnt <- cnt+1
    }
  }
  for (k in 1:kLen) {
    message <- paste0("Parzen with ",names(kernelList[k])," kernel and h = ",optH[k]," error = ",optimals[k,2]*l,"/",l," (",round(optimals[k,2],3),")")
    plot(positions,pch=1,bg="white",col=colors[flowers[k, , ]],asp=1,main=message,xlab=xname,ylab=yname)
    points(iris[ ,c(parOne,parTwo)],pch=21,bg=colors[iris$Species],col=colors[iris$Species],asp=1)
  }
  print("Map build")
  btm <- proc.time()
  print(btm-ptm)
}
main()