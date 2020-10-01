library(shiny)

lastPoint <- c(1.4,2.1)
parOne <- 3   #чтобы можно было легко поменять параметры
parTwo <- 4
selectedItems <- c(3,4)
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
    dist[i, ] <- c(i, metricFunc(xl[i,1:n],z))  #создание списка пар (номер объекта, расстояние до z)
  }
  orderedXl <- xl[order(dist[ ,2]), ]   #сортировка списка объектов
  return (orderedXl)
}


findBorders <- function(xl) {
  xMin <- xl[which.min(xl[ ,1]),1]   #нахождение минимального и максимальнго иксов
  xMax <- xl[which.max(xl[ ,1]),1]
  yMin <- xl[which.min(xl[ ,2]),2]   #нахождение минимального и максимального игриков
  yMax <- xl[which.max(xl[ ,2]),2]
  return (c(xMin,xMax,yMin,yMax))
}


ui <- basicPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId="selectK",
        label="value of K",
        min=1,max=149,value=6),
      selectInput(
        inputId="irisMap",
        label="Measures of irises:",
        choices = list(
          "Sepal length x Sepal width" = "1",
          "Sepal length x Petal length" = "2",
          "Sepal length x Petal width" = "3",
          "Sepal width x Petal length" = "4",
          "Sepal width x Petal width" = "5",
          "Petal length x Petal width" = "6"
        ),
        selected = "6"
      )
    ),
    mainPanel(
      plotOutput("plot1", click = "plot_click"),
    )
  )
)


server <- function(input, output) {
  
  observeEvent(input$irisMap, {
    val <- strtoi(input$irisMap)
    if (val<=3) {
      parOne <<- 1
      parTwo <<- val+1
    } else if (val<=5) {
      parOne <<- 2
      parTwo <<- val-1
    } else {
      parOne <<- 3
      parTwo <<- 4
    }
    xname <<- titles[[1]][parOne]   #названия для подписей на карте
    yname <<- titles[[1]][parTwo]
  })
  
  output$plot1 <- renderPlot({
      xl <- iris[ ,c(parOne,parTwo,5)]   #построение выборки
      place <- findBorders(xl)
      
      if (is.null(input$plot_click$x)) {
        z <- lastPoint
      } else {
        z <- c(round(input$plot_click$x,2), round(input$plot_click$y,2))
        lastPoint <<- z
      }
      k <- input$selectK
      l <- dim(xl)[1]
      orderedXl <- sortObjbyDist(xl,z)
      n <- dim(orderedXl)[2]-1
      classes <- orderedXl[1:k,n+1]   #получает список классов для ближайших k объектов
      counts <- table(classes)   #строит из них таблицу количества цветов каждого класса
      class <- names(which.max(counts))   #выбирает тот класс, у которого больше всего представителей
      remove <- strtoi(dimnames(orderedXl[1:k, ])[[1]])  #получить номера тех, кто является ближайшими соседями
      newXl <- xl[-remove, ]    #убрать ближайших соседей из общего списка
      message <- paste("Point (",z[1],",",z[2],") is of class",class)
      plot(place[1:2],place[3:4],col="white",main=message,xlab=xname,ylab=yname,asp=1)
      points(newXl[ ,1:n],pch=21,bg=colors[newXl[ ,n+1]],col=colors[newXl[ ,n+1]],asp=1)
      points(z[1],z[2],pch=22,bg=colors[class],col=colors[class],asp=1)
      points(orderedXl[1:k,1:n],pch=21,bg=colors[orderedXl[1:k,n+1]],col="black",asp=1)
  })
}

shinyApp(ui, server)