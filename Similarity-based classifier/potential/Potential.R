colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1")
parOne <- 3   #чтобы можно было легко поменять параметры
parTwo <- 4
titles <- dimnames(iris)[2]
xname <- titles[[1]][parOne]   #названия для подписей на карте
yname <- titles[[1]][parTwo]
distMap <- numeric()



eucDist <- function(u,v) {  #функция расстояния между парой точек
  return (sqrt(sum((u-v)^2)))
}


#LIST OF KERNELS - START
triangleKernel <- function(dist,h) {
  if (h<=1e-7 && dist<=h) {
    return (1)
  }
  if (dist<=h) {
    return (1-dist/h)   #ядро нормированное: 0<=K(x)<=1
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
  if (h<=1e-7 && dist<=h) {
    return (1)
  }
  if (dist<=h) {
    return (0.75*(1.0-(dist/h)^2))
  } else {
    return (0.0)
  }
}


GaussianKernel <- function(dist,h) {
  return (exp(-0.5*(dist*h)^2)*sqrt(h/pi*0.5))
}
#LIST OF KERNELS - END
kernelList <<- c("Triangle"=triangleKernel,"Rectangle"=rectKernel,"Epanechnikov"=EpanechnikovKernel,"Gaussian"=GaussianKernel)

potentialClass <- function(xlClasses,zIndex,H,kernelType,brightness) {   #классификация объекта с помощью потенциальных функций
  classes <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0,"unknown"=1e-7)   #массив для всех классов и ошибочной классификации
  l <- length(xlClasses)
  brightness[zIndex] <- 0   #обнуляем собственную яркость, чтобы не влиять на себя
  for (i in 1:l) {
    dist <- distMap[i,zIndex]
    w <- kernelType(dist,H[i])*brightness[i]   #потенциал * Ker(rho)
    classes[xlClasses[i]] <- classes[xlClasses[i]]+w  #добавляем к весу класса, из которого взяли объект
  }
  class <- names(which.max(classes))   #выбираем класс с наибольшим суммарным потенциалом
  return (class)
}


getErrors <- function(xl,H,curKernel,bright) {   #определяет список ошибочно классифицируемых объектов
  errors <- numeric()
  l <- length(xl)
  for (i in 1:l) {
    if (xl[i]!=potentialClass(xl,i,H,curKernel,bright)) {
      errors <- c(errors,i)   #добавляем, если класс неверный
    }
  }
  return (errors)
}


potentials <- function(xl,H,kernelIndex,speedUp=FALSE,high=10) {   #построение потенциалов
  curKernel <- kernelList[[kernelIndex]]   #текущее ядро
  l <- dim(xl)[1]
  n <- dim(xl)[2]
  bright <- array(0.0,l)  #массив "яркости"
  optimalSolution <- bright
  optimalResult <- l
  errors <- c(1:l)   #исходно все объекты считаются ошибочно классифицируемыми
  steps <- 0
  perspective <- array(l,l)   #массив для метода оптимального шага
  while (TRUE) {
    if (speedUp==FALSE) {   #используется метод оптимального шага
      for (v in errors) {   #проверяем каждый из ошибочных объектов
        if (xl[v,n]!=potentialClass(xl[ ,n],v,H,curKernel,bright)) {   #если он действительно ошибочный
          bright[v] <- bright[v]+1
          perspective[v] <- length(getErrors(xl[ ,n],H,curKernel,bright))   #узнаём, сколько ошибок можно получить, если усилить именно этот объект
          bright[v] <- bright[v]-1
        }
      }
      mn <- min(perspective[errors])   #выбираем наименьшее число ошибок
      vals <- which(perspective[errors]==mn,arr.ind=TRUE)   #берём список всех объектов, дающих минимум
      optimal <- errors[sample(vals,1)]   #и выбираем случайный из них
      bright[optimal] <- bright[optimal]+1  #и делаем именно его ярче
    } else {   #используется стандартный метод со случайным выбором
      v <- sample(errors,1)   #выбираем случайный объект из ошибочных
      if (xl[v,n]!=potentialClass(xl[ ,n],v,H,curKernel,bright)) {   #если он ошибочный
        bright[v] <- bright[v]+1   #то делаем его ярче
      }
    }
    errors <- getErrors(xl[ ,n],H,curKernel,bright)   #обновляем список ошибочно классифицируемых объектов
    if (length(errors)<optimalResult) {   #если их стало меньше, чем на прошлых шагах
      optimalSolution <- bright   #запоминаем текущие потенциалы
      optimalResult <- length(errors)   #и ошибку
    }
    if (max(bright)==high || length(errors)<5) break   #чтобы не работать слишком долго
    steps <- steps+1
    print(paste0("On step ",steps," get error ",length(errors)))
  }
  return (c(optimalResult,optimalSolution))
}


main <- function(speedUp=FALSE) {
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
  distMap <<- matrix(0.0,l,l)   #расстояния между всеми парами объектов в исходной выборке
  n <- dim(xl)[2]-1
  for (i in 1:l) {
    for (j in i:l) {
      distMap[i,j] <<- eucDist(xl[i,1:n],xl[j,1:n])
      distMap[j,i] <<- distMap[i,j]
    }
  }
  kerCount <- length(kernelList)
  H <- matrix(0.0,kerCount,l)
  for (i in 1:l) {
    class <- xl[i,n+1]   #узнаём свой класс
    enemies <- which(xl[ ,n+1]!=class,arr.ind=TRUE)   #получаем список всех чужих объектов
    dist <- max(0.1,min(distMap[i,enemies])-0.1)   #берём значение окна исходя из ближайшего чужого
    H[ ,i] <- rep(dist,4)
  }
  for (i in 1:kerCount) {
    ptm <- proc.time()
    res <- potentials(xl,H,i,speedUp)   #находим потенциалы
    error <- res[1]   #число ошибок - первый элемент ответа
    res <- res[2:length(res)]   #остальное - потенциалы
    items <- which(res>0,arr.ind=TRUE)   #берём только те точки, у которых потенциалы больше нуля
    rad <- res[items]   #считаем радиусы кругов
    rad <- rad/max(rad)   #и нормируем их, чтобы не вылезали сильно за график
    message <- paste0("Potentials of objects for ",names(kernelList)[i]," kernel with error = ",error,"/",l," (",round(error/l,3),")")
    plot(xl[ ,1:2],pch=1,col=colors[xl[ ,3]],asp=1,main=message,xlab=xname,ylab=yname)
    points(xl[items,1:2],pch=21,bg=colors[xl[items,3]],col=colors[xl[items,3]],asp=1)
    symbols(xl[items,1:2],circles=rad,add=TRUE,inches=FALSE,fg=colors[xl[items,3]],bg=scales::alpha(colors[xl[items,3]],3.0/128.0),asp=1)
    print(paste0(names(kernelList)[i]," map"))
    btm <- proc.time()
    print(btm-ptm)
  }
}
main(FALSE)
