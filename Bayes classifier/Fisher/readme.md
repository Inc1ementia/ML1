[К меню](https://github.com/Inc1ementia/ML1)

# Метод линейного дискриминанта Фишера

*Байесовский подход* является классическим в теории распознавания образов и лежит в основе многих методов. Он опирается на теорему о том, что если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.

На практике плотности распределения классов, как правило, не известны. Их приходится оценивать (восстанавливать) по обучающей выборке. В результате байесовский алгоритм перестаёт быть оптимальным, так как восстановить плотность по выборке можно только с некоторой погрешностью. Чем короче выборка, тем выше шансы подогнать распределение под конкретные данные и столкнуться с эффектом переобучения.

Линейный дискриминант Фишера в первоначальном значении - метод, определяющий расстояние между распределениями двух разных классов объектов или событий. Предположим, что обучающая выборка удовлетворяет помимо базовых гипотез байесовского классификатора также следующим гипотезам: классы распределены по нормальному закону и матрицы ковариаций классов равны Такой случай соответствует наилучшему разделению классов по дискриминанту Фишера (в первоначальном значении). Тогда статистический подход приводит к линейному дискриминанту, и именно этот алгоритм классификации в настоящее время часто понимается под термином линейный дискриминант Фишера.

<img src="https://render.githubusercontent.com/render/math?math=a(u%3B%5C%3B%7BX%7D%5E%7Bl%7D)%20%3D%20%5Carg%5Cmax_%7By%5Cin%20Y%7D%20%5Csum_%7Bi%3A%7By%7D_%7Bi%7D%3Dy%7D%20%5Cgamma_%7Bi%7D%20K(%5Cfrac%7B%5Crho(%20x%2C%7Bx%7D_%7Bi%7D)%7D%7Bh_%7Bi%7D%7D)a(x)%20%3D%5Carg%5Cmax_%7By%5Cin%20Y%7D%20%7B%5Clambda%7D_%7By%7D%7BP%7D_%7By%7D%7Bp%7D_%7By%7D(x)%20%3D%5Carg%5Cmax_%7By%5Cin%20Y%7D(%20%5Cunderbrace%7B%5Cln(%7B%5Clambda%7D_%7By%7D%7BP%7D_%7By%7D)%20-%5Cfrac%7B1%7D%7B2%7D%20%7B%5Cwidehat%7B%5Cmu%7D_%7By%7D%7D%5E%7BT%7D%20%7B%5CSigma%7D%5E%7B-1%7D%20%7B%5Cwidehat%7B%5Cmu%7D_%7By%7D%7D%7D_%7B%7B%5Cbeta%7D_%7By%7D%7D%20%2B%7Bx%7D%5E%7BT%7D%5Cunderbrace%7B%7B%5CSigma%7D%5E%7B-1%7D%20%7B%5Cwidehat%7B%5Cmu%7D%7D_%7By%7D%7D_%7B%7B%5Calpha%7D_%7By%7D%7D)%20%3D%5Carg%5Cmax_%7By%5Cin%20Y%7D(%7Bx%7D%5E%7BT%7D%7B%5Calpha%7D_%7By%7D%20%2B%7B%5Cbeta%7D_%7By%7D)">

Он неплохо работает, когда формы классов действительно близки к нормальным и не слишком сильно различаются. В этом случае линейное решающее правило близко к оптимальному байесовскому, но существенно более устойчиво, чем квадратичное, и часто обладает лучшей обобщающей способностью. Вероятность ошибки линейного дискриминанта Фишера выражается через расстояние Махаланобиса между классами, в случае, когда классов два:

<img src="https://render.githubusercontent.com/render/math?math=R(a)%20%3D%5CPhi(%20-%5Cfrac%7B1%7D%7B2%7D%5ClVert%20%7B%5Cmu%7D_%7B1%7D-%7B%5Cmu%7D_%7B2%7D%5CrVert%5CSigma)">

### Программная реализация алгоритма

```R
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
  
  
linearFisher <- function(mu1,mu2,sigma,lambda1,lambda2,P1,P2) {    #построение разделяющей прямой Фишера
  invSigma <- solve(sigma)
  muSt <- mu1-mu2
  b <- invSigma[1,2]+invSigma[2,1]
  alpha <- c((2*invSigma[1,1]*muSt[1,1]+b*muSt[1,2]),(2*invSigma[2,2]*muSt[1,2]+b*muSt[1,1]))
  betta <- mu1%*%invSigma%*%t(mu1)-mu2%*%invSigma%*%t(mu2)
  betta <- betta-(log(lambda1*P1)-log(lambda2*P2))
  abline(betta/alpha[2],-alpha[1]/alpha[2],col=colors[4],lwd=3)
}
```

### Результат работы алгоритма с использованием [shiny](https://inc1ementia.shinyapps.io/fisherShiny/)

Результатом работы алгоритма будет следующий график:

![Fisher](Fisher.png)

[К меню](https://github.com/Inc1ementia/ML1)
