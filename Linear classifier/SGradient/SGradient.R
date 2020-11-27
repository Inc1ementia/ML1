library(shiny)
library(MASS)
library(RColorBrewer)
coul1 <- brewer.pal(4,"YlGnBu")
coul1 <- colorRampPalette(coul1)(16)
coul1 <- rev(coul1)
coul1 <- coul1[6:16]
coul2 <- brewer.pal(4,"OrRd")
coul2 <- colorRampPalette(coul2)(16)
coul2 <- rev(coul2)
coul2 <- coul2[5:16]


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
    #бегунок для определения коэффициентов lambda1 и lambda2 из соотношения x/(100-x)
    sliderInput("domination","Значимость первого класса (в %)",1,99,value=50),
    splitLayout(    #вывод mu и sigma в виде матриц
      htmlOutput("firstMU"),
      htmlOutput("secondMU")
    ),
    htmlOutput("SIG")
  ),
  mainPanel(   #основная панель с графиком
    plotOutput("plot1", click = "plot_click", width="800px", height="600px"),   #график с отслеживанием нажатия
    br(),
    plotOutput("plot2",width="800px",height="600px")   #график для соотношения Q и числа шагов
  )
)


server <- function(input, output) {   #серверная часть
  colors <- c("pink1","skyblue1","forestgreen","firebrick1","darkviolet")  #набор цветов
  values <- reactiveValues()     #массив внутренних переменных
  values[["lastPoint"]] <- matrix(c(0.8,0.2,0.2,0.7),2,2)   #два столбца с центрами множеств
  values[["isClickable"]] <- 1   #текущий класс для нажатия
  values[["xl"]] <- NULL
  values[["weights"]] <- NULL
  values[["Q"]] <- NULL
  
  
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
  
  
  randomWeight <- function(n) {
    return (runif(n,min=-0.5/n,max=0.5/n))
  }
  
  
  zeroWeight <- function(n) {
    return (array(0.0,n))
  }
  
  
  adalineL <- function(w,x) {   #L для ADALINE
    n <- length(w)
    return ((c(w%*%x[1:n])*x[n+1]-1)^2)
  }
  
  
  adalineLL <- function(w,x) {   #L' для ADALINE
    n <- length(w)
    return (x[1:n]*(2.0*(c(w%*%x[1:n])*x[n+1]^2-x[n+1])))
  }
  
  
  hebbL <- function(w,x) {   #L для правила Хэбба
    n <- length(w)
    return (max(-c(w%*%x[1:n])*x[n+1],0))
  }
  
  
  hebbLL <- function(w,x) {   #L' для правила Хэбба
    n <- length(w)
    return (-x[1:n]*x[n+1])
  }
  
  
  logisticL <- function(w,x) {   #L для логистической регрессии
    n <- length(w)
    return (log2(1+exp(-c(w%*%x[1:n])*x[n+1])))
  }
  
  
  logisticLL <- function(w,x) {   #L' для логистической регрессии
    n <- length(w)
    return (x[1:n]*x[n+1]*(-1.0/(exp(c(w%*%x[1:n])*x[n+1])*log(2)+log(2))))
  }
  
  
  error <- function(w,objects,func) {   #функция подсчёта ошибки
    res <- 0
    l <- dim(objects)[1]
    for (i in 1:l) {
      res <- res+func(w,objects[i, ])
    }
    return (res)
  }
  
  
  findErrors <- function(w,objects,func) {   #функция поиска всех неправильно классифицируемых объектов для Хэбба
    res <- numeric()
    l <- dim(objects)[1]
    for (i in 1:l) {
      margin <- func(w,objects[i, ])
      if (margin>0 || c(w%*%objects[1:length(w)])==0)   #нулевое значение <w,x> или положительный отступ - ошибка
        res <- c(res,i)
    }
    return (res)
  }
  
  
  step <- function(w,obj,func) {   #градиентный шаг
    res <- func(w,obj)
    return (res)
  }
  
  
  gradient <- function(xl,lambda,funcL,funcLL,rule,weightInit=randomWeight) {
    l <- dim(xl)[1]
    n <- dim(xl)[2]-1
    w <- weightInit(n)   #инициализируем массив весов
    Q <- error(w,xl,funcL)    #считаем стартовое значение эмперического риска
    Qprev <- array(Q-100.0,10)   #массив для проверки нормализации эмпирического риска
    class <- sample(c(-1,1),1)    #один из случайных классов, для чередования
    steps <- 0
    QList <- Q
    while (TRUE) {
      steps <- steps+1
      if (rule=="H") {   #для правила Хэбба берём только плохие
        errored <- findErrors(w,xl,funcL)   #список плохих объектов
        if (length(errored)==0) break
        obj <- sample(errored,1)    #выбираем случайный
      } else {
        obj <- sample(which(xl[ ,n+1]==class,arr.ind=TRUE),1)   #выбираем случайный из предложенного класса
      }
      eps <- error(w,matrix(xl[obj, ],1,n+1),funcL)   #считаем ошибку на объекте
      if (rule=="L") {
        nu <- 1/sqrt(sum(xl[obj,1:n]*xl[obj,1:n]))
      } else {
        nu <- 1/steps
      }
      w <- w-nu*step(w,xl[obj, ],funcLL)   #и делаем градиентный спуск
      Q <- (1-lambda)*Q+lambda*eps    #пересчитываем значение эмперического риска
      QList <- c(QList,Q)
      class <- -class    #меняем класс на противоположный
      if (abs(mean(Qprev)-Q)<1e-3) {   #эмпирический риск стабилизировался
        break
      } else {
        Qprev <- c(Q,Qprev[1:9])
      }
      if (steps==2999) break    #слишком много шагов
    }
    return (c(w,QList))
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
    values[["weights"]] <- NULL
    values[["Q"]] <- NULL
  })
  observeEvent(input$typeFirst, {   #изменение первого выпадающего списка
    val <- strtoi(input$typeFirst)   #переводим параметр из сторки в число
    randomizeCovMatrix(val)
    values[["xl"]] <- NULL
    values[["weights"]] <- NULL
    values[["Q"]] <- NULL
  })
  
  
  output$plot1 <- renderPlot({
    if (is.null(input$plot_click$x)) {   #если ещё не было нажатия, или нажатие было обработанно в предыдущую итерацию
      z <- (values[["lastPoint"]])[ ,values[["isClickable"]]]   #снова обрабатываем предыдущую точку
    } else {
      z <- c(round(input$plot_click$x,2), round(input$plot_click$y,2))   #обрабатываем новое нажатие, округлив координаты
      if (values[["isClickable"]]==1) {  #запоминаем последнее нажатие
        if (z[1]!=(values[["lastPoint"]])[1,1] || z[2]!=(values[["lastPoint"]])[2,1]) {
          values[["xl"]] <- NULL
          values[["weights"]] <- NULL
          values[["Q"]] <- NULL
        }
        values[["lastPoint"]] <- matrix(c(z,(values[["lastPoint"]])[ ,2]),2,2)
      } else {
        if (z[1]!=(values[["lastPoint"]])[1,2] || z[2]!=(values[["lastPoint"]])[2,2]) {
          values[["xl"]] <- NULL
          values[["weights"]] <- NULL
          values[["Q"]] <- NULL
        }
        values[["lastPoint"]] <- matrix(c((values[["lastPoint"]])[ ,1],z),2,2)
      }
    }
    mu1 <- matrix((values[["lastPoint"]])[ ,1],1,2)   #по последнему нажатию строятся матрицы центров классов
    mu2 <- matrix((values[["lastPoint"]])[ ,2],1,2)
    lambda1 <- input$domination
    lambda2 <- 100-lambda1
    w0 <- log(lambda1/lambda2)   #отношение l1/l2 для логистической регрессии
    if (!is.null(values[["xl"]])) {   #данные сохранены с прошлого раза
      xl <- values[["xl"]]
      xl1 <- xl[which(xl[ ,4]==-1), ]
      xl2 <- xl[which(xl[ ,4]==1), ]
      Q <- values[["Q"]]
      weights <- values[["weights"]]
    } else {    #генерируем новые данные
      xl1 <- mvrnorm(100,mu1,values[["sig"]])    #многомерное распределение точек классов
      xl2 <- mvrnorm(100,mu2,values[["sig"]])
      xl <- rbind(cbind(xl1,-1),cbind(xl2,1))   #объединяем два множества точек, указав классы
      xl <- cbind(xl[ ,1:2],-1,xl[ ,3])
      values[["xl"]] <- xl
      values[["weights"]] <- NULL
      values[["Q"]] <- NULL
    }
    mu1h <- muHat(xl1)
    mu2h <- muHat(xl2)
    sigh <- sigmaHat(xl1,xl2,mu1h,mu2h)
    x <- seq(0.0,1.0,by=0.01)
    y <- x
    if (is.null(values[["weights"]])) {   #нужно заново запустить градиентные спуски
      Q <- matrix(0.0,4,3000)
      weights <- matrix(0.0,3,3)
      Q[1, ] <- 1:3000
      result <- gradient(xl,0.1,adalineL,adalineLL,"A")
      weights[1, ] <- result[1:3]
      Q[2,1:(length(result)-3)] <- result[4:length(result)]
      result <- gradient(xl,0.1,hebbL,hebbLL,"H")
      weights[2, ] <- result[1:3]
      Q[3,1:(length(result)-3)] <- result[4:length(result)]
      result <- gradient(xl,0.005,logisticL,logisticLL,"L")
      weights[3, ] <- result[1:3]
      Q[4,1:(length(result)-3)] <- result[4:length(result)]
      values[["Q"]] <- Q
      values[["weights"]] <- weights
    }
    zz <- outer(x,y,function(x,y) {1/(1+exp(-(weights[3,1]*x+weights[3,2]*y-weights[3,3]-w0)))})
    coul <- c(coul1,rev(coul2))
    message <- "Линейные классификаторы ADALINE, правило Хэбба и логистическая регрессия"
    filled.contour(   #линии уровней для логистической регрессии
      x,y,zz,
      col=coul,
      main=message,
      xlab="x",ylab="y",
      plot.axes={
        points(xl[ ,1],xl[ ,2],pch=21,bg=colors[ifelse(xl[ ,4]==1,1,2)],col="black",asp=1)   #рисуем сами точки классов
        abline(weights[1,3]/weights[1,2],-weights[1,1]/weights[1,2],col=colors[3],lwd=2,asp=1)
        abline(weights[2,3]/weights[2,2],-weights[2,1]/weights[2,2],col=colors[4],lwd=2,asp=1)
        abline((weights[3,3]+w0)/weights[3,2],-weights[3,1]/weights[3,2],col=colors[5],lwd=2,asp=1)
        legend(0.88,1,c("ADALINE","Hebb","LogIt"),pch=c("l","l","l"),col=colors[3:5])
      }
    )
  })
  
  
  output$plot2 <- renderPlot({
    if (!is.null(values[["Q"]])) {   #график соотношения Q и числа шагов
      Q <- values[["Q"]]
      last <- 1
      while (last<2995) {
        if (Q[2,last]<1e-3 && Q[3,last]<1e-3 && Q[4,last]<1e-3) {
          break
        } else {
          last <- last+1
        }
      }
      Q <- Q[ ,1:(last+5)]
      message <- "Зависимость Q от числа шагов"
      plot(Q[1, ],Q[2, ],col=colors[3],asp=1,type="l",main=message,ylab="Q",xlab="шаг")
      lines(Q[1, ],Q[3, ],col=colors[4],asp=1,type="l")
      lines(Q[1, ],Q[4, ],col=colors[5],asp=1,type="l")
      legend(last-150,max(Q[2:4, ])+10,c("ADALINE","Hebb","LogIt"),pch=c("l","l","l"),col=colors[3:5])
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
