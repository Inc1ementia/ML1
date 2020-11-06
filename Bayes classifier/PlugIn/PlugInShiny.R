library(shiny)
library(MASS)

ui <- basicPage(   #описание интерфейса
  sidebarPanel(
    actionButton("clickFirst","Выбор центра первого класса"),  #после нажатия кликом по графику перемещаем центр первого класса
    selectInput(   #выбор типа ковариационной матрицы первого класса
      inputId="typeFirst",
      label="Матрица ковариаций первого класса:",
      choices = list(
        "Сферическая матрица" = "1",
        "Элиптическая матрица" = "2",
        "Наклонная матрица" = "3"
      ),
      selected = "1"
    ),
    splitLayout(    #вывод mu и sigma в виде матриц
      htmlOutput("firstMU"),
      htmlOutput("firstSIG")
    ),
    actionButton("clickSecond","Выбор центра второго класса"),  #после нажатия кликом по графику перемещаем центр второго класса
    selectInput(   #выбор типа ковариационной матрицы второго класса
      inputId="typeSecond",
      label="Матрица ковариаций второго класса:",
      choices = list(
        "Сферическая матрица" = "1",
        "Элиптическая матрица" = "2",
        "Наклонная матрица" = "3"
      ),
      selected = "1"
    ),
    splitLayout(    #вывод mu и sigma в виде матриц
      htmlOutput("secondMU"),
      htmlOutput("secondSIG")
    ),
    splitLayout(    #значение P1 и P2 для классов
      numericInput("weight1","P1",100,1,500),
      numericInput("weight2","P2",100,1,500)
    ),
    sliderInput("domination","Процентная составляющая первого класса",1,99,value=50),   #установка параметра лямбда
    actionButton("workout","Точка для классификации")   #переключение на выбор точки для классификации
  ),
  mainPanel(   #основная панель с графиком
    plotOutput("plot1", click = "plot_click", width = "600px", height = "600px")   #график с отслеживанием нажатия
  )
)


server <- function(input, output) {   #серверная часть
  colors <- c("pink1","skyblue1","gold","purple4","yellow")  #набор цветов
  lastPoint1 <- reactiveVal(c(8,2))   #храним данные о последнем нажатие для первого класса
  lastPoint2 <- reactiveVal(c(-3,7))   #и для второго
  lastPoint3 <- reactiveVal(NULL)    #точка для классификации
  isClickable <- reactiveVal(1)    #номер того, чьи координаты мы сейчас меняем
  sig1 <- reactiveVal(matrix(c(1,0,0,1),2,2))   #матрица ковариаций первого класса
  sig2 <- reactiveVal(matrix(c(1,0,0,1),2,2))   #и второго
  objList <- reactiveVal(NULL)   #список точек (для классификации)
  lastLambda <- reactiveVal(50)    #последнее значение лямбды
  
  
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
  
  
  getLevelLine <- function(mu,sigma) {  #получение коэффициентов линии уровня (x-mu)^T %*% Sig^-1 %*% (x-mu)
    invSigma <- solve(sigma)
    a <- invSigma[1,1]
    b <- invSigma[1,2]+invSigma[2,1]
    c <- invSigma[2,2]
    d <- -2*mu[1]*invSigma[1,1]-mu[2]*b
    e <- -2*mu[2]*invSigma[2,2]-mu[1]*b
    f <- invSigma[1,1]*mu[1]^2+invSigma[2,2]*mu[2]^2+mu[1]*mu[2]*b
    return (c(a,b,c,d,e,f))
  }
  
  
  plugIn <- function(mu1,sigma1,lambda1,P1,mu2,sigma2,lambda2,P2) {   #получение коэффициентов разделяющей линии плагин-алгоритма
    invSigma1 <- solve(sigma1)
    invSigma2 <- solve(sigma2)
    alpha <- invSigma1-invSigma2
    betta <- invSigma1%*%t(mu1)-invSigma2%*%t(mu2)
    a <- alpha[1,1]
    b <- alpha[1,2]+alpha[2,1]
    c <- alpha[2,2]
    d <- -2*betta[1,1]
    e <- -2*betta[2,1]
    f <- mu1%*%invSigma1%*%t(mu1)-mu2%*%invSigma2%*%t(mu2)
    f <- f-(log(lambda1*P1)-log(lambda2*P2)-0.5*(log(det(invSigma2))-log(det(invSigma1))))
    return (c(a,b,c,d,e,f))
  }
  
  
  plugInAlgo <- function(mu1,sigma1,lambda1,P1,mu2,sigma2,lambda2,P2,x,y) {    #плагин-алгоритм
    params <- plugIn(mu1,sigma1,lambda1,P1,mu2,sigma2,lambda2,P2)
    z <- outer(x,y,function(x,y) {params[1]*x^2+params[2]*x*y+params[3]*y^2+params[4]*x+params[5]*y+params[6]})
    contour(x,y,z,levels=0,drawlabels=FALSE,lwd=3,col=colors[3],add=TRUE)
  }
  
  
  levelLine <- function(mu,sigma,x,y,levelColor) {    #построение линии уровня
    coeffs <- getLevelLine(mu,sigma)
    z <- outer(x,y,function(x,y) {coeffs[1]*x^2+coeffs[2]*x*y+coeffs[3]*y^2+coeffs[4]*x+coeffs[5]*y+coeffs[6]})
    contour(x,y,z,levels=c(5,10,15),drawlabels=FALSE,lwd=1,col=levelColor,add=TRUE)
    points(mu[1],mu[2],pch=19,bg=levelColor,asp=1)
  }
  
  
  muHat <- function(xl) {   #считаем значение мю по данным для класса
    n <- dim(xl)[2]
    mu <- array(NA,n)
    for (i in 1:n) {
      mu[i] <- mean(xl[ ,i])   #по каждой координате берём среднее
    }
    return (t(mu))
  }
  
  
  sigmaHat <- function(xl,mu) {   #считаем зачение сигма по данным для класса и полученному мю
    l <- dim(xl)[1]
    n <- dim(xl)[2]
    sigma <- matrix(0,n,n)
    for (i in 1:l) {
      sigma <- sigma+(t(xl[i, ]-mu) %*% (xl[i, ]-mu))/(l-1)
    }
    return (sigma)
  }
  
  
  #обработчик событий клиент-сервер
  observeEvent(input$clickFirst, {   #переключение на выбор центра первого класса
    isClickable(1)
  })
  
  
  observeEvent(input$clickSecond, {   #переключение на выбор центра второго класса
    isClickable(2)
  })
  
  
  observeEvent(input$workout, {   #переключение на классификацию
    isClickable(3)
  })
  
  
  observeEvent(input$typeFirst, {   #изменение первого выпадающего списка
    val <- strtoi(input$typeFirst)   #переводим параметр из сторки в число
    if (val==1) {   #сферическая матрица
      d <- sample(1:15,1)
      sig1(matrix(c(d,0,0,d),2,2))
    } else if (val==2) {   #эллиптическая
      d <- sample(1:15,2)
      sig1(matrix(c(d[1],0,0,d[2]),2,2))
    } else {   #наклонный эллипс
      d <- sample(1:15,2)
      int <- floor(sqrt(d[1]*d[2])-0.001)
      e <- sample(-int:int,1)
      sig1(matrix(c(d[1],e,e,d[2]),2,2))
    }
    isClickable(1)
    lastPoint3(NULL)
  })
  
  
  observeEvent(input$typeSecond, {   #изменение второго выпадающего списка
    val <- strtoi(input$typeSecond)   #переводим параметр из сторки в число
    if (val==1) {   #сферическая матрица
      d <- sample(1:15,1)
      sig2(matrix(c(d,0,0,d),2,2))
    } else if (val==2) {   #эллиптическая
      d <- sample(1:15,2)
      sig2(matrix(c(d[1],0,0,d[2]),2,2))
    } else {   #наклонный эллипс
      d <- sample(1:15,2)
      int <- floor(sqrt(d[1]*d[2])-0.001)
      e <- sample(-int:int,1)
      sig2(matrix(c(d[1],e,e,d[2]),2,2))
    }
    isClickable(2)
    lastPoint3(NULL)
  })
  
  
  output$plot1 <- renderPlot({
    if (is.null(input$plot_click$x)) {   #если ещё не было нажатия, или нажатие было обработанно в предыдущую итерацию
      if (isClickable()==1) { #снова обрабатываем предыдущую точку
        z <- lastPoint1()
      } else if (isClickable()==2) {
        z <- lastPoint2()
      } else {
        z <- lastPoint3()
      }
    } else {
      z <- c(round(input$plot_click$x,2), round(input$plot_click$y,2))   #обрабатываем новое нажатие, округлив координаты
      if (isClickable()==1) {  #запоминаем последнее нажатие
        lastPoint1(z)
        lastPoint3(NULL)
      } else if (isClickable()==2) {
        lastPoint2(z)
        lastPoint3(NULL)
      } else {
        lastPoint3(z)
      }
    }
    if (input$domination!=lastLambda()) {   #если менялось значение лямбды
      z <- lastPoint1()   #то выставляем выбор первого класса
      isClickable(1)
      lastPoint3(NULL)
    }
    if (isClickable()==3) {
      xl <- objList()
      if (dim(xl)[1]!=input$weight1+input$weight2) {   #если размер выборки не совпадает, то есть менялись P1 или P2
        z <- lastPoint1()   #то выставляем выбор первого класса
        isClickable(1)
        lastPoint3(NULL)
      }
    }
    if (isClickable()!=3) {
      lambda1 <- input$domination   #обновление графика
      lastLambda(lambda1)
      lambda2 <- 100-lambda1
      P1 <- input$weight1
      P2 <- input$weight2
      mu1 <- matrix(lastPoint1(),1,2)   #по последнему нажатию строятся матрицы центров классов
      mu2 <- matrix(lastPoint2(),1,2)
      xl1 <- mvrnorm(P1,mu1,sig1())    #многомерное распределение точек классов
      xl2 <- mvrnorm(P2,mu2,sig2())
      xl <- rbind(cbind(xl1,1),cbind(xl2,2))   #объединяем два множества точек, указав классы
      objList(xl)
      mu1h <- muHat(xl1)
      mu2h <- muHat(xl2)
      sig1h <- sigmaHat(xl1,mu1h)
      sig2h <- sigmaHat(xl2,mu2h)
    } else {    #классификация
      lambda1 <- lastLambda()
      lambda2 <- 100-lambda1
      P1 <- input$weight1
      P2 <- input$weight2
      xl <- objList()
      xl1 <- xl[which(xl[ ,3]==1,arr.ind=TRUE),1:2]
      xl2 <- xl[which(xl[ ,3]==2,arr.ind=TRUE),1:2]
      mu1h <- muHat(xl1)
      mu2h <- muHat(xl2)
      sig1h <- sigmaHat(xl1,mu1h)
      sig2h <- sigmaHat(xl2,mu2h)
    }
    x <- seq(-20,20,by=0.1)
    y <- x
    plot(seq(-15,15,by=0.1),seq(-15,15,by=0.1),type="n",xlab="x",ylab="y",asp=1)   #пустой график
    levelLine(mu1h,sig1h,x,y,colors[1])   #линия уровня первого класса
    levelLine(mu2h,sig2h,x,y,colors[2])   #и второго
    points(xl[ ,1],xl[ ,2],pch=21,bg=colors[xl[ ,3]],asp=1)   #рисуем сами точки классов
    plugInAlgo(mu1h,sig1h,lambda1,P1,mu2h,sig2h,lambda2,P2,x,y)   #plug-in алгоритм
    if (isClickable()==3 && !is.null(z)) {   #классификатор
      params <- plugIn(mu1h,sig1h,lambda1,P1,mu2h,sig2h,lambda2,P2)
      class <- params[1]*z[1]^2+params[2]*z[1]*z[2]+params[3]*z[2]^2+params[4]*z[1]+params[5]*z[2]+params[6]
      if (class<0) {
       class <- 1 
      } else {
        class <- 2
      }
      points(z[1],z[2],pch=22,bg=colors[class],asp=1,col="purple")
    }
  })
  
  
  output$firstMU <- renderUI({    #рисуем матрицу координат центра первого класса
    HTML(buildMatrix(matrix(lastPoint1(),1,2)))
  })
  
  
  output$secondMU <- renderUI({    #рисуем матрицу координат центра второго класса
    HTML(buildMatrix(matrix(lastPoint2(),1,2)))
  })
  
  
  output$firstSIG <- renderUI({   #рисуем ковариационную матрицу первого класса
    HTML(buildMatrix(sig1()))
  })
  
  
  output$secondSIG <- renderUI({   #рисуем ковариационную матрицу второго класса
    HTML(buildMatrix(sig2()))
  })
}

shinyApp(ui, server)   #запуск приложения
