library(shiny)



ui <- basicPage(   #описание интерфейса
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
    #бегунок для определения коэффициентов lambda1 и lambda2 из соотношения x/(100-x)
    sliderInput("domination","Процентная составляющая первого класса",1,99,value=50),
    splitLayout(    #вывод mu и sigma в виде матриц
      htmlOutput("firstMU"),
      htmlOutput("secondMU")
    ),
    htmlOutput("SIG")
  ),
  mainPanel(   #основная панель с графиком
    plotOutput("plot1", click = "plot_click")   #график с отслеживанием нажатия
  )
)


server <- function(input, output) {   #серверная часть
  colors <- c("pink1","skyblue1","gold","purple4","yellow")  #набор цветов
  lastPoint1 <- reactiveVal(c(8,2))   #храним данные о последнем нажатие для первого класса
  lastPoint2 <- reactiveVal(c(-3,7))   #и для второго
  isClickable <- reactiveVal(1)    #номер того, чьи координаты мы сейчас меняем
  sig <- reactiveVal(matrix(c(1,0,0,1),2,2))   #матрица ковариаций первого класса
  
  
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
  
  
  levelLine <- function(mu,sigma,x,y,levelColor) {    #построение линии уровня
    coeffs <- getLevelLine(mu,sigma)
    z <- outer(x,y,function(x,y) {coeffs[1]*x^2+coeffs[2]*x*y+coeffs[3]*y^2+coeffs[4]*x+coeffs[5]*y+coeffs[6]})
    contour(x,y,z,levels=c(5,10,15),drawlabels=FALSE,lwd=1,col=levelColor,add=TRUE)
    points(mu[1],mu[2],pch=19,bg=levelColor,asp=1)
  }
  
  
  linearFisher <- function(mu1,mu2,sigma,lambda1,lambda2) {    #построение разделяющей прямой Фишера
    invSigma <- solve(sigma);
    alpha <- invSigma%*%t(mu1-mu2);
    muSt <- (mu1*lambda2+mu2*lambda1)*0.5;
    betta <- muSt%*%alpha;
    abline(betta/alpha[2,1],-alpha[1,1]/alpha[2,1],col=colors[4],lwd=3)
  }
  
  
  
  #обработчик событий клиент-сервер
  observeEvent(input$clickFirst, {   #переключение на выбор центра первого класса
    isClickable(1)
  })
  
  
  observeEvent(input$clickSecond, {   #переключение на выбор центра второго класса
    isClickable(2)
  })
  
  
  observeEvent(input$typeFirst, {   #изменение первого выпадающего списка
    val <- strtoi(input$typeFirst)   #переводим параметр из сторки в число
    if (val==1) {   #сферическая матрица
      d <- sample(1:15,1)
      sig(matrix(c(d,0,0,d),2,2))
    } else if (val==2) {   #эллиптическая
      d <- sample(1:15,2)
      sig(matrix(c(d[1],0,0,d[2]),2,2))
    } else {   #наклонный эллипс
      d <- sample(1:15,2)
      int <- floor(sqrt(d[1]*d[2])-0.001)
      e <- sample(-int:int,1)
      sig(matrix(c(d[1],e,e,d[2]),2,2))
    }
  })
  
  
  output$plot1 <- renderPlot({
    if (is.null(input$plot_click$x)) {   #если ещё не было нажатия, или нажатие было обработанно в предыдущую итерацию
      if (isClickable()==1) { #снова обрабатываем предыдущую точку
        z <- lastPoint1()
      } else {
        z <- lastPoint2()
      }
    } else {
      z <- c(round(input$plot_click$x,2), round(input$plot_click$y,2))   #обрабатываем новое нажатие, округлив координаты
      if (isClickable()==1) {  #запоминаем последнее нажатие
        lastPoint1(z)
      } else {
        lastPoint2(z)
      }
    }
    mu1 <- matrix(lastPoint1(),1,2)   #по последнему нажатию строятся матрицы центров классов
    mu2 <- matrix(lastPoint2(),1,2)
    xl1 <- mvrnorm(250,mu1,sig())    #многомерное распределение точек классов
    xl2 <- mvrnorm(250,mu2,sig())
    xl <- rbind(cbind(xl1,1),cbind(xl2,2))   #объединяем два множества точек, указав классы
    x <- seq(-20,20,by=0.1)
    y <- x
    plot(x,y,type="n",xlab="x",ylab="y",asp=1)   #пустой график
    levelLine(mu1,sig(),x,y,colors[1])   #линия уровня первого класса
    levelLine(mu2,sig(),x,y,colors[2])
    points(xl[ ,1],xl[ ,2],pch=21,bg=colors[xl[ ,3]],asp=1)   #рисуем сами точки классов
    linearFisher(mu1,mu2,sig(),(input$domination)*0.01,(100-input$domination)*0.01)
    points(mu1[1,1],mu1[1,2],pch=21,bg=colors[5])
    points(mu2[1,1],mu2[1,2],pch=21,bg=colors[5])
    segments(mu1[1,1],mu1[1,2],mu2[1,1],mu2[1,2],col=colors[5],lwd=2)
  })
  
  
  output$firstMU <- renderUI({    #рисуем матрицу координат центра первого класса
    HTML(buildMatrix(matrix(lastPoint1(),1,2)))
  })
  
  
  output$secondMU <- renderUI({    #рисуем матрицу координат центра второго класса
    HTML(buildMatrix(matrix(lastPoint2(),1,2)))
  })
  
  
  output$SIG <- renderUI({   #рисуем ковариационную матрицу первого класса
    HTML(buildMatrix(sig()))
  })
}

shinyApp(ui, server)   #запуск приложения