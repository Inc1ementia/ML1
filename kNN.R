eucDist <- function(u,v) {  #ôóíêöèÿ ðàññòîÿíèÿ ìåæäó ïàðîé òî÷åê
  return (sqrt(sum((u-v)^2)))
}
colors <- c("setosa"="pink1","versicolor"="gold","virginica"="skyblue1")
sortObjbyDist <- function(xl,z,metricFunc=eucDist) {  #ôóíêöèÿ ñîðòèðîâêè ìàññèâà ïî ðàññòîÿíèþ äî  z
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  dist <- matrix(NA,l,2)
  for (i in 1:l) {
    dist[i, ] <- c(i, metricFunc(xl[i,1:n],z))  #ñîçäàíèå ñïèñêà ïàð (íîìåð îáúåêòà, ðàññòîÿíèå äî z)
  }
  orderedXl <- xl[order(dist[ ,2]), ]   #ñîðòèðîâêà ñïèñêà îáúåêòîâ
  return (orderedXl)
}
kNN <- function(xl,z,k) {   #ôóíêöèÿ âûáîðà êëàññà ìåòîäîâ kNN
  orderedXl <- sortObjbyDist(xl,z)
  n <- dim(orderedXl)[2]-1
  classes <- orderedXl[1:k,n+1]   #ïîëó÷àåò ñïèñîê âèäîâ äëÿ áëèæàéøèõ k îáúåêòîâ
  counts <- table(classes)   #ñòðîèòü èç íèõ òàáëèöó êîëè÷åñòâî öâåòîâ êàæäîãî âèäà
  class <- names(which.max(counts))   #âûáèðàåò òîò âèä, ó êîòîðîãî áîëüøå âñåãî ïðåäñòàâèòåëåé
  return (class)
}
determKNN_LOO <- function(xl) {   #îïðåäåëåíèå îïòèìàëüíîãî k ìåòîäîì LOO
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  maxK <- l-1
  errorForK <- matrix(0.0,maxK,1)
  for (i in 1:l) {  #ïåðåáèðàåì êàæäûé ýëåìåíò
    obj <- xl[i, ]
    newXl <- xl[-i, ]  #óáèðàåì åãî èç âûáîðêè
    z <- obj[1:n]
    orderedXl <- sortObjbyDist(newXl,z)  #íàõîäèì ðàññòîÿíèÿ äî âñåõ îáúåêòîâ
    classes <- c("setosa"=0,"versicolor"=0,"virginica"=0)
    print(i)
    for (k in 1:maxK) {   #ïåðåáèðàåì çíà÷åíèå k
      classes[orderedXl[k,n+1]] <- classes[orderedXl[k,n+1]]+1  #äîáàâëÿåì k-ûé ýëåìåíò â ñâîþ ãðóïïó
      class <- names(which.max(classes))  #âûáèðàåò òîò âèä, ó êîòîðîãî áîëüøå âñåãî ïðåäñòàâèòåëåé
      if (class!=obj[n+1]) {   #åñëè ïðîèçîøëà îøèáêà êëàññèôèêàöèè
        errorForK[k]=errorForK[k]+1   #òî óâèëè÷èâàåì äëÿ äàííîãî k çíà÷åíèå îøèáêè
      }
    }
  }
  LOO <- matrix(NA,maxK,2)
  for (k in 1:maxK) {
    LOO[k, ] <- c(k,errorForK[k])   #ñòðîèì ìàññèâ ïàð (íîìåð, ÷èñëî îøèáîê)
  }
  optK <- which.min(errorForK)  #âûáèðàåì îïòèìàëüíîå k
  plot(LOO,pch=1,type="l",col="blue",xlab="k",ylab="LOO(k)",main="Find optimal value of k for kNN with LOO-algo")
  points(optK,LOO[optK,2],pch=21,bg="red",col="red")
  res <- paste("optK=",optK)   #ãîòîâèò ïîäïèñü äëÿ ãðàôèêà
  text(optK,LOO[optK,2]+7,labels=res)
  return (optK)
}
main <- function() {
  parOne <- 3   #÷òîáû ìîæíî áûëî ëåãêî ïîìåíÿòü ïàðàìåòðû
  parTwo <- 4
  titles <- dimnames(iris)[2]
  xname <- titles[[1]][parOne]   #íàçâàíèÿ äëÿ ïîäïèñåé íà êàðòå
  yname <- titles[[1]][parTwo]
  xl <- iris[ ,c(parOne,parTwo,5)]   #ïîñòðîåíèå âûáîðêè
  xMin <- xl[which.min(xl[ ,1]),1]   #íàõîæäåíèå ìèíèìàëüíîãî è ìàêñèìàëüíãî èêñîâ
  xMax <- xl[which.max(xl[ ,1]),1]
  X <- seq(from=xMin,to=xMax,by=0.1)   #ñïèñîê âñåõ èêñîâ íà êàðòå
  xLen <- length(X)
  yMin <- xl[which.min(xl[ ,2]),2]   #íàõîæäåíèå ìèíèìàëüíîãî è ìàêñèìàëüíîãî èãðèêîâ
  yMax <- xl[which.max(xl[ ,2]),2]
  Y <- seq(from=yMin,to=yMax,by=0.1)   #ñïèñîê âñåõ èãðèêîâ íà êàðòå
  yLen <-length(Y)
  flowers <- matrix(NA,xLen,yLen)
  positions <- matrix(NA,xLen*yLen,2)
  optK <- determKNN_LOO(xl)   #íàõîäèì îïòèìàëüíîå k
  cnt <- 1
  for (i in 1:yLen) {
    print(Y[i])
    for (j in 1:xLen) {
      z <- c(X[j],Y[i])
      positions[cnt, ] <- z   #çàïîìèíàåì êîîðäèíàòû îáúåêòà
      flowers[j,i] <- kNN(xl,z,optK)   #íàõîäèì ïðåäïîëîæèòåëüíûé êëàññ ìåòîäîì kNN
      cnt <- cnt+1
    }
  }
  plot(positions,pch=1,bg="white",col=colors[flowers],asp=1,main="Map of kNN for optimal K",xlab=xname,ylab=yname)
  points(iris[ ,c(3,4)],pch=21,bg=colors[iris$Species],col=colors[iris$Species],asp=1)
}
main()
