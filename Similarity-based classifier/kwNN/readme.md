[К меню](https://github.com/Inc1ementia/ML1)

# Метод kwNN или Метод k взвешенных ближайших соседей

### Суть метода

*Метрические методы обучения* — методы, основанные на анализе сходства объектов. (similarity-based learning, distance-based learning). Для формализации понятия сходства вводится *функция расстояния* в пространстве объектов X.

Метрические алгоритмы относятся к методам ленивого обучения (lazy learning), а также к методам рассуждения по прецедентам (case-based reasoning, CBR).

Метод kwNN или Метод k взвешенных ближайших соседей находит k ближайших (т.е. с наименьшим значением функции расстояния) соседей в пространстве признаков и присваивает точку *x* к тому классу, объектов которого больше среди них, при этом оценивая степень их важности для классифицируемой точки с помощью *весовой функции*, т.е. 

<img src="https://render.githubusercontent.com/render/math?math=\omega (i, u) = [i \le k]w(i)">

<img src="https://render.githubusercontent.com/render/math?math=%5Calpha%20(u%3B%7BX%7D%5E%7Bl%7D%2Ck)%3D%5Carg%5Cmax_%7By%5Cin%20Y%7D%5Csum_%7Bi%3D1%7D%5E%7Bk%7D%5B%7By%7D_%7Bu%7D%5E%7B(i)%7D%20%3Dy%5Dw(i)">


### Алгоритм

1. Вычислить расстояние до каждого объекта обучающей выборки
2. Объекты обучающей выборки отсортировать по расстоянию до рассматриваемой точки
3. Найти k объектов с минимальным расстоянием
4. Посчитать веса для k ближайших объектов
5. Класс классифицируемого объекта - это класс, для которого значения весов максимальное

### Программная реализация алгоритма

```R
eucDist <- function(u,v) {  #функция расстояния между парой точек
  return (sqrt(sum((u-v)^2)))
}


sortObjbyDist <- function(xl,z,metricFunc=eucDist) {  #функция сортировки массива по расстоянию до z
  l <- dim(xl)[1]
  n <- dim(xl)[2]-1
  dist <- matrix(NA,l,2)
  for (i in 1:l) {
    dist[i, ] <- c(i,metricFunc(xl[i,1:n],z))  #создание списка пар (номер объекта, расстояние до z)
  }
  orderedXl <- xl[order(dist[ ,2]), ]   #сортировка списка расстояний
  return (orderedXl)
}


kwNN <- function(xl,z,optK,optQ) {
  orderedXl <- sortObjbyDist(xl,z)   #получает отсортированный список объектов
  n <- dim(orderedXl)[2]
  classes <- orderedXl[1:optK,n]   #выбирает классы ближайших k объектов
  counts <- c("setosa"=0.0,"versicolor"=0.0,"virginica"=0.0)
  for (i in 1:optK) {
    counts[classes[i]] <- counts[classes[i]]+qPow[i,optQ];
  }
  class <- names(which.max(counts))   #выбирает тот класс, у которого больше всего представителей
  return (class)
}
```

### Результат работы алгоритма

Результатом работы алгоритма будет следующий график

![kwNN](kwNN.png)

График ошибок для пар (k,q)

![kwNNLOO](kwNNLOO.png)

График параметров

![kwNNParametrs](kwNNParametrs.png)

Подсветка соседей для случайной точки

![kwNNLightup](kwNNLightup.png)

### Демонстрация преимущества алгоритма kwNN над kNN

![kNNComp](kNNComp.png)

![kwNNComp](kwNNComp.png)

[К меню](https://github.com/Inc1ementia/ML1)
