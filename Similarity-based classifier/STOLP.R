library(ggplot2)
colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1")
parOne <- 3   #чтобы можно было легко поменять параметры
parTwo <- 4
titles <- dimnames(iris)[2]
xname <- titles[[1]][parOne]   #названия для подписей на карте
yname <- titles[[1]][parTwo]
distMap <<- numeric()


eucDist <- function(u,v) {  #функция расстояния между парой точек
  return (sqrt(sum((u-v)^2)))
}


#ядра нужны для kNNMod, так как в оригинальном алгоритме выбирается класс с минимальным расстоянием,
#а для алгоритма STOLP необъодимо, чтобы у лучшего класса значение было максимальным


exponentialKernel <- function(dist,h=10) {
  return (exp(-dist*h))
}


GaussianKernel <- function(dist,h=10) {
  return (exp(-0.5*(dist*h)^2)*sqrt(h/pi*0.5))
}


NN <- function(xl,items,zIndex,par=FALSE) {   #метод ближайшего соседа
  res <- c("setosa"=0,"versicolor"=0,"virginica"=0)
  if (par==FALSE) {    #при обучении расстояния уже просчитаны
    n <- dim(xl)[2]
    obj <- items[which.min(distMap[zIndex,items])]   #выбираем самый близкий объект
    res[xl[obj,n]] <- 1   #и устанавливаем ему 1
    return (res)
  } else {
    l <- length(items)
    n <- dim(xl)[2]-1
    dist <- array(0.0,l)   #массив расстояний до всех допустимых объектов
    for (i in 1:l) {
      dist[i] <- eucDist(xl[items[i],1:n],zIndex)   #считаем расстояние от объекта до z
    }
    obj <- items[which.min(dist)]  #выбираем самый близкий объект
    res[xl[obj,n+1]] <- 1   #и устанавливаем ему 1
    return (names(res)[which.max(res)])
  }
}


kNNMod <- function(xl,items,zIndex,par=FALSE,kOpt=10,ker=exponentialKernel) {   #модифицированный алгоритм kNN, берёт по k ближайших соседей из каждого класса
  res <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0)
  clCount <- length(res)    #число классов
  if (par==FALSE) {    #при обучении расстояния уже просчитаны
    n <- dim(xl)[2]
    for (i in 1:clCount) {
      class <- names(res)[i]   #узнаём текущий класс
      subClass <- items[which(xl[items,n]==class,arr.ind=TRUE)]   #выбираем номера всех имеющихся элементов текущего класса
      k <- min(length(subClass),kOpt)   #объектов может быть меньше чем k
      subClass <- subClass[order(distMap[zIndex,subClass])]   #сортируем объекты по расстоянию до z
      res[class] <- ker(mean(distMap[zIndex,subClass[1:k]]))   #берём ядровую функцию от среднего расстояния до объектов
    }
    return (res)
  } else {
    l <- length(items)
    n <- dim(xl)[2]-1
    for (i in 1:clCount) {
      class <- names(res)[i]   #узнаём текущий класс
      subClass <- items[which(xl[items,n+1]==class,arr.ind=TRUE)]   #выбираем номера всех имеющихся элементов текущего класса
      k <- min(length(subClass),kOpt)   #объектов может быть меньше чем k
      dist <- array(0.0,l)
      for (i in 1:l) {
        dist[i] <- eucDist(xl[subClass,1:n],zIndex)    #считаем расстояние от каждого из объектов до z
      }
      subClass <- subClass[order(dist)]   #сортируем объекты по расстоянию до z
      res[class] <- ker(mean(dist[1:k]))   #берём ядровую функцию от среднего расстояния до объектов
    }
    return (names(res)[which.max(res)])
  }
}


addMax <- function(xl,range,margin,name) {
  class <- range[which(xl[range]==name,arr.ind=TRUE)]   #выбираем все доступные элементы указанного класса
  m <- which.max(margin[class])    #находим объект, максимальный по отступу
  return (class[m])
}


STOLP <- function(xl,algo,l0,delta) {
  l <- dim(xl)[1]
  n <- dim(xl)[2]
  items <- c(1:l)   #список всех предметов выборки
  classes <- names(colors)   #названия всех классов
  clCount <- length(classes)   #количество классов
  margin <- array(0.0,l)    #массив для отступов
  for (i in 1:l) {
    res <- algo(xl,items,i)   #получаем массив параметров
    class <- which(classes==xl[i,n],arr.ind=TRUE)   #выбираем класс самого объекта
    margin[i] <- res[class]-max(res[-class])   #отступ: свой класс - лучший чужой
  }
  data <- data.frame(x=1:l,y=margin[order(margin)])   #отсортированный массив отступов
  items <- which(margin>=delta,arr.ind=TRUE)    #остальные объекты
  omega <- numeric()
  for (i in 1:clCount) {
    omega <- c(omega,addMax(xl[ ,n],items,margin,classes[i]))   #выбираем по одному самому мощному элементу из каждого класса
  }
  for (i in 1:length(omega)) {
    items <- items[which(items!=omega[i],arr.ind=TRUE)]   #убираем из нераспознанных элементов эталоны
  }
  while (length(items)>0) {
     E <- numeric()
     for (i in items) {
       res <- algo(xl,items,i)
       class <- which(classes==xl[i,n],arr.ind=TRUE)
       margin[i] <- res[class]-max(res[-class])
       if (margin[i]<0) {
         E <- c(E,i)   #если отступ отрицательный, то объект - претендент на то, чтобы стать эталоном
       }
     }
     if (length(E)<l0) break   #если объектов меньше l0, то конец алгоритма
     obj <- which.min(margin[E])   #выбираем объект с минимальным отступом
     omega <- c(omega,E[obj])    #и добавляем в список эталонов
     items <- items[which(items!=E[obj],arr.ind=TRUE)]   #после чего убираем его из списка проверяемых объектов
  }
  items <- c(1:l)
  items <- items[-omega]   #число ошибок будем считать по выборке без объектов
  error <- numeric()
  for (i in items) {
    res <- algo(xl,omega,i)    
    class <- classes[which.max(res)]
    if (class!=xl[i,n]) {
      error <- c(error,i)   #если неправильно классифицорвали, то добавляем в список ошибок
    }
  }
  data$col <- ifelse(
    data$x<=length(error),"firebrick2",
    ifelse(
      data$y<delta,"lightpink2",
      ifelse(
        data$y<0.1,"palegoldenrod",
        ifelse(
          data$x>l-length(omega),"seagreen4",
          "palegreen1"
        )
      )
    )
  )
  toDraw <- ggplot(data,aes(x=x, y=y)) +
    geom_segment(aes(x=x,y=0,xend=x,yend=y,colour=col),size=1.3,alpha=0.9) +
    scale_colour_identity() +
    theme(legend.position="none",panel.border=element_blank()) +
    xlab("") +
    ylab("Margin")
  print(toDraw)
  return (omega)   #возвращаем список эталонов
}


main <- function(algo,runMap=FALSE) {
  ptm <- proc.time()
  classes <- names(colors)
  xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  xMin <- xl[which.min(xl[ ,1]),1]   #нахождение минимального и максимального иксов
  xMax <- xl[which.max(xl[ ,1]),1]
  X <- seq(from=xMin,to=xMax,by=0.05)   #список всех иксов на карте
  xLen <- length(X)
  yMin <- xl[which.min(xl[ ,2]),2]   #нахождение минимального и максимального игриков
  yMax <- xl[which.max(xl[ ,2]),2]
  Y <- seq(from=yMin,to=yMax,by=0.05)   #список всех игриков на карте
  yLen <-length(Y)
  distMap <<- matrix(0.0,l,l)   #расстояния между всеми парами объектов в исходной выборке
  for (i in 1:l) {
    for (j in i:l) {
      distMap[i,j] <<- eucDist(xl[i,1:n],xl[j,1:n])
      distMap[j,i] <<- distMap[i,j]
    }
    distMap[i,i] <<- 1e9   #расстояние до самого себя должно быть очень большим, чтобы сам объект не был первым соседом
  }
  omega <- STOLP(xl,algo,1,0)   #запускаем STOLP
  print(omega)
  error <- numeric()
  items <- c(1:l)
  items <- items[-omega]   #число ошибок будем считать по выборке без объектов
  for (i in items) {
    res <- algo(xl,omega,i)    
    class <- classes[which.max(res)]
    if (class!=xl[i,n+1]) {
      error <- c(error,i)   #если неправильно классифицорвали, то добавляем в список ошибок
    }
  }
  goodItems <- items
  for (i in 1:length(error)) {
    goodItems <- goodItems[which(goodItems!=error[i],arr.ind=TRUE)]   #хорошие объекты - не плохие!
  }
  message <- paste0(length(error),"/",length(items)," objects get wrong class, err=",round(length(error)/length(items),3))
  plot(c(xMin,xMax),c(yMin,yMax),pch=1,bg="white",col="white",asp=1,main=message,xlab=xname,ylab=yname)
  points(iris[goodItems,c(parOne,parTwo)],pch=1,col=colors[iris[goodItems,5]],asp=1)
  points(iris[error,c(parOne,parTwo)],pch=22,bg=colors[iris[error,5]],col="gray",asp=1)
  points(iris[omega,c(parOne,parTwo)],pch=21,bg=colors[iris[omega,5]],col="black",asp=1)
  print("STOLP select")
  btm <- proc.time()
  print(btm-ptm)
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
        flowers[j,i] <- algo(xl,omega,z,TRUE)   #находим предположительный класс методом kNN
        cnt <- cnt+1
      }
    }
    plot(positions,pch=1,bg="white",col=colors[flowers],asp=1,main="Map for STOLP",xlab=xname,ylab=yname)
    points(iris[omega,c(parOne,parTwo)],pch=21,bg=colors[iris[omega,5]],col=colors[iris[omega,5]],asp=1)
    print("MAP build")
    btm <- proc.time()
    print(btm-ptm)
  }
}
main(kNNMod,TRUE)