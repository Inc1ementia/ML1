library(lattice)
library(RColorBrewer)
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


NN <- function(omega,obj) {   #метод ближайшего соседа
  return (which.min(distMap[obj,omega]))
}


weight <- function(u,x,z) {   #функция S(u,x|x')
  return ((distMap[u,z]-distMap[u,x])/(distMap[u,z]+distMap[u,x]))
}


findEtalon <- function(xl,yClass,omega,lambda,subArray) {   #для указанного класса ищет эталон
  l <- dim(xl)[1]
  n <- dim(xl)[2]
  xly <- which(xl[subArray,n]==yClass,arr.ind=TRUE)   #выделяем X_y
  xly <- subArray[xly]
  ly <- length(xly)
  if (ly<=1) return (NA)   #если элементов слишком мало, то ничего не делаем
  neighbor <- array(NA,l)
  for (i in subArray) {
    neighbor[i] <- NN(omega,i)   #находим ближайшего соседа для каждого объекта из X_y
  }
  effective <- array(0,ly)
  for (x in 1:ly) {
    defence <- 1.0/(ly-1)
    tolerance <- 1.0/(length(subArray)-ly)
    sum <- 0.0
    for (u in 1:ly) {
      if (u!=x && is.na(neighbor[xly[u]])==FALSE) {
        sum <- sum+weight(xly[u],xly[x],neighbor[xly[u]])   #считаем "защищённость"
      }
    }
    defence <- defence*sum
    sum <- 0.0
    for (v in subArray) {
      if (v!=xly[x] && is.na(neighbor[v])==FALSE) {
        sum <- sum+weight(v,xly[x],neighbor[v])   #считаем "толерантность"
      }
    }
    tolerance <- tolerance*sum
    effective[x] <- lambda*defence + (1-lambda)*tolerance   #эффективность текущего объекта
  }
  return (xly[which.max(effective)])   #выбираем лучший объект
}


FRiSSTOLP <- function(xl,lambda,theta) {   #FRis-STOLP для выбранных lambda и theta
  classes <- names(colors)
  clCount <- length(classes)
  l <- dim(xl)[1]
  n <- dim(xl)[2]
  omegaCoor <- matrix(0,clCount,4)   #указатели на индексы в массиве omega:
  #первые два столбца - текущий набор эталонов по классам, 3-4 - новые наборы, которые потом заменят текущие
  omega <- numeric()   #массив эталонов, содержит эталоны каждого из классов
  pos <- 1
  items <- c(1:l)
  for (i in 1:clCount) {
    other <- which(xl[ ,n]!=classes[i],arr.ind=TRUE)   #все объекты чужих классов
    etalon <- findEtalon(xl,classes[i],other,lambda,items)   #определяем эталон в своём классе
    omegaCoor[i,3] <- pos
    omegaCoor[i,4] <- pos
    pos <- pos+1
    omega <- c(omega,etalon)
  }
  for (i in 1:clCount) {   #заменяем индексы текущих эталонов на новые
    omegaCoor[i,1:2] <- omegaCoor[i,3:4]
  }
  omega <- omega[omegaCoor[1,1]:omegaCoor[clCount,2]]   #заменяем текущие эталоны на новые
  val <- omegaCoor[1,1]-1   #сдвигаем координаты, чтобы индексы были верными
  for (i in 1:clCount) {
    omegaCoor[i,1] <- omegaCoor[i,1]-val
    omegaCoor[i,2] <- omegaCoor[i,2]-val
  }
  pos <- length(omega)+1   #индекс следующей свободной ячейки в массиве эталонов
  for (i in 1:clCount) {
    part <- numeric()
    for (j in 1:clCount) {
      if (i!=j) {    #берём эталоны из чужих классов
        part <- c(part,omega[omegaCoor[j,1]:omegaCoor[j,2]])
      }
    }
    etalon <- findEtalon(xl,classes[i],part,lambda,items)   #пересчитываем эталон текущего класса
    omegaCoor[i,3] <- pos
    omegaCoor[i,4] <- pos
    pos <- pos+1
    omega <- c(omega,etalon)
  }
  for (i in 1:clCount) {   #заменяем индексы текущих эталонов на новые
    omegaCoor[i,1:2] <- omegaCoor[i,3:4]
  }
  omega <- omega[omegaCoor[1,1]:omegaCoor[clCount,2]]   #заменяем текущие эталоны на новые
  val <- omegaCoor[1,1]-1   #сдвигаем координаты, чтобы индексы были верными
  for (i in 1:clCount) {
    omegaCoor[i,1] <- omegaCoor[i,1]-val
    omegaCoor[i,2] <- omegaCoor[i,2]-val
  }
  pos <- length(omega)   #индекс текущей последней ячейки в массиве эталонов
  while (length(items)>0) {   #цикл добавления эталонов
    u <- numeric()
    for (i in items) {
      class <- which(classes==xl[i,n],arr.ind=TRUE)   #узнаём класс объекта
      friend <- NN(omega[omegaCoor[class,1]:omegaCoor[class,2]],i)   #находим ближайший свой эталон
      friend <- omega[omegaCoor[class,1]+friend-1]
      part <- numeric()
      for (j in 1:clCount) {
        if (j!=class) {
          part <- c(part,omega[omegaCoor[j,1]:omegaCoor[j,2]])   #список чужих эталонов
        }
      }
      enemy <- NN(part,i)   #находим ближайшего чужого
      enemy <- part[enemy]
      w <- weight(i,friend,enemy)   #считаем функцию S
      if (w>theta) {   #если она больше theta, то объект классифицирован достаточно правильно
        u <- c(u,i)
      }
    }
    if (length(u)>0) {   #если есть новые хорошие объекты
      for (i in 1:length(u)) {
        items <- items[which(items!=u[i],arr.ind=TRUE)]   #то их надо исключить из дальнейшего рассмотрения
      }
    } else {
      break   #дальше начнётся стогнация, можно сразу заканчивать цикл
    }
    for (i in 1:pos) {
      val <- which(u==omega[i],arr.ind=TRUE)   #если среди эталонов есть тот, кого можно классифицировать
      if (length(val)>0) {
        omega[i] <- NA   #то удаляем его из списка эталонов
      }
    }
    pos2 <- pos+1
    for (i in 1:clCount) {   #для каждого класса сдвигаем массив эталонов с учётом исключённых объектов
      etalon <- which(is.na(omega[omegaCoor[i,1]:omegaCoor[i,2]])==FALSE,arr.ind=TRUE)
      omegaCoor[i,3] <- pos2
      pos2 <- pos2+length(etalon)
      if (pos2==omegaCoor[i,3]) {
        omegaCoor[i,3] <- NA
        omegaCoor[i,4] <- NA
      } else {
        omegaCoor[i,4] <- pos2-1
      }
      omega <- c(omega,omega[omegaCoor[i,1]-1+etalon])
    }
    for (i in 1:clCount) {   #заменяем индексы текущих эталонов на новые
      omegaCoor[i,1:2] <- omegaCoor[i,3:4]
    }
    omega <- omega[omegaCoor[1,1]:omegaCoor[clCount,2]]   #заменяем текущие эталоны на новые
    val <- omegaCoor[1,1]-1   #сдвигаем координаты, чтобы индексы были верными
    for (i in 1:clCount) {
      omegaCoor[i,1] <- omegaCoor[i,1]-val
      omegaCoor[i,2] <- omegaCoor[i,2]-val
    }
    pos <- length(omega)+1   #индекс следующей свободной ячейки в массиве эталонов
    for (i in 1:clCount) {
      part <- numeric()
      for (j in 1:clCount) {
        if (i!=j && is.na(omegaCoor[j,1])==FALSE) {
          part <- c(part,omega[omegaCoor[j,1]:omegaCoor[j,2]])   #выбираем эталоны чужих классов
        }
      }
      etalon <- findEtalon(xl,classes[i],part,lambda,items)   #и считаем новый эталон текущего класса
      omegaCoor[i,3] <- pos
      bonus <- numeric()
      if (is.na(omegaCoor[i,1])==FALSE) {   #если в списке есть эталоны
        pos <- pos+omegaCoor[i,2]-omegaCoor[i,1]+2
        double <- FALSE
        if (is.na(etalon)) {   #эталон не найден
          double <- TRUE
        } else {
          for (j in omegaCoor[i,1]:omegaCoor[i,2]) {
            if (omega[j]==etalon) {   #такой эталон уже мог быть в списке, тогда добавлять не нужно
              double <- TRUE
            }
          }
        }
        if (double==FALSE) {   #добавляем объект в список текущего класса
          bonus <- c(omega[omegaCoor[i,1]:omegaCoor[i,2]],etalon)
        } else {   #оставляем список без изменений
          bonus <- omega[omegaCoor[i,1]:omegaCoor[i,2]]
          pos <- pos-1
        }
      } else {  #если список эталонов пуст, то найденный объект точно становится эталоном
        pos <- pos+1
        bonus <- etalon
      }
      omegaCoor[i,4] <- pos-1
      omega <- c(omega,bonus)
    }
    for (i in 1:clCount) {   #заменяем индексы текущих эталонов на новые
      omegaCoor[i,1:2] <- omegaCoor[i,3:4]
    }
    omega <- omega[omegaCoor[1,1]:omegaCoor[clCount,2]]   #заменяем текущие эталоны на новые
    val <- omegaCoor[1,1]-1   #сдвигаем координаты, чтобы индексы были верными
    for (i in 1:clCount) {
      omegaCoor[i,1] <- omegaCoor[i,1]-val
      omegaCoor[i,2] <- omegaCoor[i,2]-val
    }
    pos <- length(omega)   #индекс текущей последней ячейки в массиве эталонов
  }
  return (omega)
}


main <- function() {
  ptm <- proc.time()
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
  distMap <<- matrix(0.0,l,l)
  for (i in 1:l) {
    for (j in i:l) {
      distMap[i,j] <<- eucDist(xl[i,1:n],xl[j,1:n])
      distMap[j,i] <<- distMap[i,j]
    }
    distMap[i,i] <<- 1e9
  }
  lambda <- seq(from=0.0,to=1.0,by=0.05)   #массив значений 0<=lambda<=1
  lambdaLength <- length(lambda)
  theta <- seq(from=0.0,to=0.95,by=0.05)   #массив значений 0<=theta<=0.95
  thetaLength <- length(theta)
  resTable <- matrix(0.0,lambdaLength,thetaLength)
  n <- n+1
  classes <- names(colors)
  optimalError <- l
  optimal <- numeric()
  for (i in 1:lambdaLength) {
    print(lambda[i])
    for (j in 1:thetaLength) {
      omega <- FRiSSTOLP(xl,lambda[i],theta[j])   #находим набор эталонов для выбранных параметров
      error <- 0
      for (k in 1:l) {   #и считаем ошибку по этим эталонам
        class <- which(classes==xl[k,n],arr.ind=TRUE)
        neighbor <- omega[NN(omega,k)]
        neighbor <- which(classes==xl[neighbor,n],arr.ind=TRUE)
        if (neighbor!=class) {
          error <- error+1
        }
      }
      resTable[i,j] <- error/l   #заносим в массив
      if (resTable[i,j]<optimalError) {   #если значение лучше предшествующих
        optimalError <- resTable[i,j]   #то запоминаем его и список эталонов
        optimal <- omega
      }
    }
  }
  grid <- expand.grid(x=lambda,y=theta)
  grid$z <- c(resTable)
  coul <- brewer.pal(4,"YlGnBu")
  coul <- colorRampPalette(coul)(16)
  coul <- rev(coul)
  print(levelplot(
    z ~ x * y,grid,col.regions=coul,main="FRiS-STOLP with different lambda and theta",
    xlab="lambda",ylab="theta",asp=1
  ))
  
  error <- numeric()
  omega <- optimal
  items <- c(1:l)
  items <- items[-omega]   #число ошибок будем считать по выборке без объектов
  for (i in items) {
    class <- which(classes==xl[i,n],arr.ind=TRUE)
    neighbor <- omega[NN(omega,i)]
    neighbor <- which(classes==xl[neighbor,n],arr.ind=TRUE)
    if (neighbor!=class) {
      error <- c(error,i)
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
  
  print("FRiS Map build")
  btm <- proc.time()
  print(btm-ptm)
  
}
main()