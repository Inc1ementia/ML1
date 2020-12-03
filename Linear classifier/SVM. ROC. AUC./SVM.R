library(shiny)
library(kernlab)
library(MASS)


ui <- basicPage(
  sidebarPanel(
    actionButton("clickFirst","Выбор центра первого класса"),  #после нажатия кликом по графику перемещаем центр первого класса
    actionButton("clickSecond","Выбор центра второго класса"),  #после нажатия кликом по графику перемещаем центр второго класса
    selectInput(   #выбор типа ковариационной матрицы
      inputId="typeFirst",
      label="Матрица ковариаций:",
      choices = list(
        "Сферическая матрица" = "1",
        "Элиптическая матрица" = "2",
        "Наклонная матрица" = "3"
      ),
      selected = "1"
    ),
    actionButton("newRandom","Новая матрица того же типа"),
    #бегунок для определения коэффициентоа C в оптимизационной формуле SVM
    sliderInput("Ccoeff","Значение коэффициента C",1,50,value=10),
    splitLayout(    #вывод mu и sigma в виде матриц
      htmlOutput("firstMU"),
      htmlOutput("secondMU")
    ),
    htmlOutput("SIG")
  ),
  mainPanel(   #основная панель с графиком
    plotOutput("plot1", click = "plot_click", width="600px", height="600px"),   #график с отслеживанием нажатия
    br(),
    plotOutput("plot2",width="600px",height="600px")   #график для ROC-кривой
  )
)


server <- function(input, output) {   #серверная часть
  colors <- c("gold","skyblue1","forestgreen","firebrick1","darkviolet","pink1")  #набор цветов
  values <- reactiveValues()     #массив внутренних переменных
  values[["lastPoint"]] <- matrix(c(0.2,0.5,0.2,0.7),2,2)   #два столбца с центрами множеств
  values[["isClickable"]] <- 1   #текущий класс для нажатия
  values[["xl"]] <- NULL
  values[["ROC"]] <- NULL
  
  
  buildMatrix <- function(matr) {   #красивое отображение матриц с использованием стилей в html
    height <- dim(matr)[1]
    width <- dim(matr)[2]
    res <- '<table style="border-left: 2px solid #000;
        border-right: 2px solid #000;
        margin-bottom: 5px;">'
    for (i in 1:height) {
      res <- paste0(res,'<tr height="25px"><td width="5px"></td>')
      for (j in 1:width) {
        res <- paste0(res,'<td align="center">',matr[i,j],'</td><td width="5px"></td>')
      }
      res <- paste0(res,'</tr>')
    }
    res <- paste0(res,'</table>')
    return (res)
  }
  
  
  muHat <- function(xl) {   #считаем значение мю по данным для класса
    n <- dim(xl)[2]
    mu <- array(NA,n)
    for (i in 1:n) {
      mu[i] <- mean(xl[ ,i])   #по каждой координате берём среднее
    }
    return (t(mu))
  }
  
  
  sigmaHat <- function(xl1,xl2,mu1,mu2) {   #считаем зачение сигма по данным для класса и полученному мю
    l <- dim(xl1)[1]
    ll <- dim(xl2)[1]
    n <- dim(xl1)[2]
    sigma <- matrix(0,n,n)
    for (i in 1:l) {
      sigma <- sigma+(t(xl1[i, ]-mu1) %*% (xl1[i, ]-mu1))/(l-1)
    }
    for (i in 1:ll) {
      sigma <- sigma+(t(xl2[i, ]-mu2) %*% (xl2[i, ]-mu2))/(ll-1)
    }
    return (sigma)
  }
  
  
  randomizeCovMatrix <- function(val) {
    if (val==1) {   #сферическая матрица
      d <- sample(1:10,1)
      values[["sig"]] <- matrix(c(d*0.001,0,0,d*0.001),2,2)
    } else if (val==2) {   #эллиптическая
      d <- sample(1:10,2)
      values[["sig"]] <- matrix(c(d[1]*0.001,0,0,d[2]*0.001),2,2)
    } else {   #наклонный эллипс
      d <- sample(1:10,2)
      int <- floor(sqrt(d[1]*d[2])-0.001)
      e <- sample(1:int,1)
      e <- ifelse(sample(1:2,1)==1,1,-1)*e
      values[["sig"]] <- matrix(c(d[1]*0.001,e*0.001,e*0.001,d[2]*0.001),2,2)
    }
  }
  
  
  quality <- function(w,xl) {  #функция качества
    Q <- 0
    l <- dim(xl)[1]
    n <- dim(xl)[2]-1
    for (i in 1:l) {
      sc <- corr(w,c(xl[i,1:n],-1))
      if (sc*xl[i,n+1]<=0) {
        Q <- Q+1
      }
    }
    return (Q)
  }
  
  
  corr <- function(w,x) {  #скалярное произведение <w,xi>
    return (sum(w*x))
  }
  
  
  buildROC <- function(w,xl) {   #построение ROC-кривой
    n <- dim(xl)[2]-1
    l <- dim(xl)[1]
    lMinus <- length(which(xl[ ,n+1]==-1,arr.ind=TRUE))   #число объектов класса -1
    lPlus <- l-lMinus   #число объектов класса +1
    corrVal <- matrix(NA,l,2)
    for (i in 1:l) {
      corrVal[i, ] <- c(i,corr(w,c(xl[i,1:n],-1)))
    }
    ordered <- xl[order(corrVal[ ,2],decreasing=TRUE),n+1]   #сортируем выборку по убыванию расстояния до прямой
    ROCPnt <- matrix(0,l+1,2)
    FPR <- 0
    TPR <- 0
    AUC <- 0
    for (i in 1:l) {
      yi <- ordered[i]
      if (yi==-1) {
        FPR <- FPR+(1.0/lMinus)
        AUC <- AUC+(1.0/lMinus)*TPR
      } else {
        TPR <- TPR+(1.0/lPlus)
      }
      ROCPnt[(i+1), ] <- c(FPR,TPR)
    }
    values[["ROC"]] <- ROCPnt   #запоминаем параметр ROC-кривой и AUC
    values[["AUC"]] <- AUC
  }
  
  
  #обработчик событий клиент-сервер
  observeEvent(input$clickFirst, {   #переключение на выбор центра первого класса
    
    values[["isClickable"]] <- 1
  })
  
  
  observeEvent(input$clickSecond, {   #переключение на выбор центра второго класса
    values[["isClickable"]] <- 2
  })
  
  
  observeEvent(input$newRandom, {   #запрос на обновление матрица ковариаций
    val <- strtoi(input$typeFirst)   #переводим параметр из сторки в число
    randomizeCovMatrix(val)
    values[["xl"]] <- NULL
  })
  
  
  observeEvent(input$typeFirst, {   #изменение первого выпадающего списка
    val <- strtoi(input$typeFirst)   #переводим параметр из сторки в число
    randomizeCovMatrix(val)
    values[["xl"]] <- NULL
  })
  
  
  output$plot1 <- renderPlot({
    if (is.null(input$plot_click$x)) {   #если ещё не было нажатия, или нажатие было обработанно в предыдущую итерацию
      z <- (values[["lastPoint"]])[ ,values[["isClickable"]]]   #снова обрабатываем предыдущую точку
    } else {
      z <- c(round(input$plot_click$x,2), round(input$plot_click$y,2))   #обрабатываем новое нажатие, округлив координаты
      if (values[["isClickable"]]==1) {  #запоминаем последнее нажатие
        if (z[1]!=(values[["lastPoint"]])[1,1] || z[2]!=(values[["lastPoint"]])[2,1]) {
          values[["xl"]] <- NULL
        }
        values[["lastPoint"]] <- matrix(c(z,(values[["lastPoint"]])[ ,2]),2,2)
      } else {
        if (z[1]!=(values[["lastPoint"]])[1,2] || z[2]!=(values[["lastPoint"]])[2,2]) {
          values[["xl"]] <- NULL
        }
        values[["lastPoint"]] <- matrix(c((values[["lastPoint"]])[ ,1],z),2,2)
      }
    }
    mu1 <- matrix((values[["lastPoint"]])[ ,1],1,2)   #по последнему нажатию строятся матрицы центров классов
    mu2 <- matrix((values[["lastPoint"]])[ ,2],1,2)
    C <- input$Ccoeff
    if (!is.null(values[["xl"]])) {   #данные сохранены с прошлого раза
      xl <- values[["xl"]]
      xl1 <- xl[which(xl[ ,3]==-1), ]
      xl2 <- xl[which(xl[ ,3]==1), ]
    } else {    #генерируем новые данные
      xl1 <- mvrnorm(100,mu1,values[["sig"]])    #многомерное распределение точек классов
      xl2 <- mvrnorm(100,mu2,values[["sig"]])
      xl <- rbind(cbind(xl1,-1),cbind(xl2,1))   #объединяем два множества точек, указав классы
      values[["xl"]] <- xl
    }
    mu1h <- muHat(xl1)
    mu2h <- muHat(xl2)
    sigh <- sigmaHat(xl1,xl2,mu1h,mu2h)
    x <- seq(0.0,1.0,by=0.01)
    y <- x
    svp <- ksvm(x=xl[ ,1:2],y=xl[ ,3],type="C-svc",kernel="vanilladot",C=C,scaled=c())  #вызов SVM
    bad <- SVindex(svp)   #индексы точек, ставших опорными
    w <- colSums(coef(svp)[[1]]*xl[bad,1:2])  #веса
    w0 <- b(svp)   #и добавочный коэффициент
    w[3] <- w0
    message <- paste0("SVM-алгоритм со значением ошибки ",round(quality(w,xl)/dim(xl)[1],7))
    plot(x,y,main=message,col="white",asp=1)
    points(xl[-bad,1],xl[-bad,2],pch=21,bg=colors[ifelse(xl[-bad,3]==-1,1,2)],asp=1)
    points(xl[bad,1],xl[bad,2],pch=21,bg=colors[ifelse(xl[bad,3]==-1,4,5)],asp=1)
    abline(w0/w[2],-w[1]/w[2],col=colors[3],lwd=1,asp=1)
    abline((w0+1)/w[2],-w[1]/w[2],lty=2,asp=1)
    abline((w0-1)/w[2],-w[1]/w[2],lty=2,asp=1)
    w <- w/w[2]
    l <- dim(xl)[1]
    n <- dim(xl)[2]-1
    d <- 0
    ind <- 1
    for (i in 1:l) {   #находим самую дальнюю от прямой точку
      di <- abs(corr(w,c(xl[i,1:n],-1)))
      if (di>d) {
        d <- di
        ind <- i
      }
    }
    d <- ifelse(corr(w,c(xl[ind,1:n],-1))<0,-1,1)
    if (d!=xl[ind,n+1]) w <- w*-1   #и если она неправильно классифицируется, то меняем знак прямой
    buildROC(w,xl)   #построение ROC-кривой
  })
  output$plot2 <- renderPlot({
    if (!is.null(values[["ROC"]])) {   #график ROC-кривой
      ROC <- values[["ROC"]]
      AUC <- values[["AUC"]]
      message <- "График ROC-кривой"
      plot(c(0,1),c(0,1),col="white",xlab="FPR",ylab="TPR",main=message,asp=1)
      segments(0,0,1,1,col=colors[2],lwd=1)
      lines(ROC[ ,1],ROC[ ,2],lwd=2,col=colors[5],asp=1)
      legend("bottomright",paste0("Площадь под кривой (AUC)=",round(AUC,8)),text.col=c("black","blue"),ncol=1)
    }
  })
  output$firstMU <- renderUI({    #рисуем матрицу координат центра первого класса
    HTML(buildMatrix(matrix((values[["lastPoint"]])[ ,1],1,2)))
  })
  output$secondMU <- renderUI({    #рисуем матрицу координат центра второго класса
    HTML(buildMatrix(matrix((values[["lastPoint"]])[ ,2],1,2)))
  })
  output$SIG <- renderUI({   #рисуем ковариационную матрицу первого класса
    HTML(buildMatrix(values[["sig"]]))
  })
}

shinyApp(ui, server)   #запуск приложения