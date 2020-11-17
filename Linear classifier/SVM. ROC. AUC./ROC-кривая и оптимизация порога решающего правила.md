# ROC-кривая и оптимизация порога решающего правила

Рассмотрим задачу классификации на два класса, Y = {−1, +1}, и модель алгоритмов

<img src="https://render.githubusercontent.com/render/math?math=a(x%2C%20w)%3Dsign(f(x%2Cw)-w_0)">

где 

<img src="https://render.githubusercontent.com/render/math?math=w_0%5Cin%5Cmathbb%7BR%7D">

аддитивный параметр дискриминантной функции. В теории нейронных сетей его называют *порогом активации*.

В случае линейной дискриминантной функции параметр определяется отношением потерь:

<img src="https://render.githubusercontent.com/render/math?math=%7Bw%7D_%7B0%7D%3D%5Cln%5Cfrac%7B%7B%5Clambda%7D_%7B-%7D%7D%7B%7B%5Clambda%7D_%7B%2B%7D%7D">

где λ+ и λ− — величина потери при ошибке на объекте класса «+1» и «−1» соответственно.

На практике отношение потерь может многократно пересматриваться. Поэтому вводится специальная характеристика — *ROC-кривая*, которая показывает, что происходит с числом ошибок обоих типов, если изменяется отношение потерь. Термин *операционная характеристика приёмника* (receiver operating characteristic, ROC curve) пришёл из теории обработки сигналов.

Каждая точка на ROC-кривой соответствует некоторому алгоритму. В общем случае это даже не обязательно кривая — дискретное множество алгоритмов может быть отображено в тех же координатах в виде точечного графика.

По оси X откладывается доля *ошибочных положительных классификаций* (false positive rate, FPR)

<img src="https://render.githubusercontent.com/render/math?math=%5Ctext%7BFPR%7D(%20a%2C%7BX%7D%5E%7Bl%7D)%20%3D%5Cfrac%7B%7B%5Csum%7D_%7Bi%3D1%7D%5E%7Bl%7D%5B%7By%7D_%7Bi%7D%3D-1%5D%5Ba(%7Bx%7D_%7Bi%7D)%20%3D%2B1%5D%7D%7B%7B%5Csum%7D_%7Bi%3D1%7D%5E%7Bl%7D%5B%20%7By%7D_%7Bi%7D%3D-1%5D%7D">

Величина 1 − FPR(a) равна доле *правильных отрицательных классификаций* (true negative rate, TNR) и называется *специфичностью* алгоритма. Поэтому на горизонтальной оси иногда пишут «1 − специфичность».

По оси Y откладывается доля *правильных положительных классификаций* (true positive rate, TPR), называемая также *чувствительностью* алгоритма:

<img src="https://render.githubusercontent.com/render/math?math=%5Ctext%7BTPR%7D(%20a%2C%7BX%7D%5E%7Bl%7D)%20%3D%5Cfrac%7B%7B%5Csum%7D_%7Bi%3D1%7D%5E%7Bl%7D%5B%7By%7D_%7Bi%7D%3D%2B1%5D%5Ba(%7Bx%7D_%7Bi%7D)%20%3D%2B1%5D%7D%7B%7B%5Csum%7D_%7Bi%3D1%7D%5E%7Bl%7D%5B%7By%7D_%7Bi%7D%3D%2B1%5D%7D">

Каждая точка ROC-кривой соответствует определённому значению параметра. ROC-кривая монотонно не убывает и проходит из точки (0, 0) в (1, 1).

Для построения ROC-кривой нет необходимости вычислять FPR и TPR суммированием по всей выборке при каждом параметре. Более эффективный алгоритм основан на простой идее, что в качестве значений порога достаточно перебрать только значения дискриминантной функции

<img src="https://render.githubusercontent.com/render/math?math=f(%7Bx%7D_%7Bi%7D)%3D%5Clangle%20w%2C%20%7Bx%7D_%7Bi%7D%20%5Crangle">

которые она принимает на объектах выборки.

Чем выше проходит ROC-кривая, тем выше качество классификации. Идеальная ROC-кривая проходит через левый верхний угол — точку (0, 1). Наихудший алгоритм соответствует диагональной прямой, соединяющей точки (0, 0) и (1, 1); её также изображают на графике как ориентир.

В роли общей характеристики качества классификации, не зависящей от конъюнктурного параметра, выступает *площадь под ROC-кривой* (area under curve, AUC). 

### Алгоритм

------

**Вход:** 

​	обучающая выборка:

<img src="https://render.githubusercontent.com/render/math?math=%7BX%7D%5E%7Bl%7D">

​	дискриминантная функция:

<img src="https://render.githubusercontent.com/render/math?math=f(x)%3D%5Clangle%20w%2C%20x%20%5Crangle">


**Выход:** 

​	последовательность точек ROC-кривой:

<img src="https://render.githubusercontent.com/render/math?math=%7B%5C%7B(%7B%5Ctext%7BFPR%7D%7D_%7Bi%7D%2C%7B%5Ctext%7BTPR%7D%7D_%7Bi%7D)%5C%7D%7D_%7Bi%3D0%7D%5E%7Bl%7D">

​	площадь под ROC-кривой: AUC

------

 1. число объектов класса −1:

    <img src="https://render.githubusercontent.com/render/math?math=%7Bl%7D_%7B-%7D%3A%3D%7B%5Csum%7D_%7Bi%3D1%7D%5E%7Bl%7D%5B%7By%7D_%7Bi%7D%3D-1%5D">

    число объектов класса +1:
    
    <img src="https://render.githubusercontent.com/render/math?math=%7Bl%7D_%7B%2B%7D%3A%3D%7B%5Csum%7D_%7Bi%3D1%7D%5E%7Bl%7D%5B%7By%7D_%7Bi%7D%3D%2B1%5D">
    

 2. упорядочить выборку по убыванию значений
    
    <img src="https://render.githubusercontent.com/render/math?math=f(%7Bx%7D_%7Bi%7D)">
    

 3. поставить первую точку в начало координат:
    
    <img src="https://render.githubusercontent.com/render/math?math=(%7B%5Ctext%7BFPR%7D%7D_%7B0%7D%2C%7B%5Ctext%7BTPR%7D%7D_%7B0%7D)%3A%3D(0%2C0)%3B%5Cquad%20%5Ctext%7BAUC%7D%3A%3D0">
    

 4. 
    <img src="https://render.githubusercontent.com/render/math?math=i%3A%3D1%2C%5Cdots%2Cl">
    

    если 
    
    <img src="https://render.githubusercontent.com/render/math?math=%7By%7D_%7Bi%7D%3D-1">
    
    то сместиться на один шаг вправо:
    
    <img src="https://render.githubusercontent.com/render/math?math=%7B%5Ctext%7BFPR%7D%7D_%7Bi%7D%20%3A%3D%20%7B%5Ctext%7BFPR%7D%7D_%7Bi-1%7D%20%2B%5Cfrac%7B1%7D%7B%7Bl%7D_%7B-%7D%7D%3B%20%5Cquad%20%7B%5Ctext%7BTPR%7D%7D_%7Bi%7D%20%3A%3D%20%7B%5Ctext%7BTPR%7D%7D_%7Bi-1%7D">
    

    
    <img src="https://render.githubusercontent.com/render/math?math=%5Ctext%7BAUC%7D%20%3A%3D%20%5Ctext%7BAUC%7D%2B%5Cfrac%7B1%7D%7B%7Bl%7D_%7B-%7D%7D%20%7B%5Ctext%7BTPR%7D%7D_%7Bi%7D">
    

    иначе сместиться на один шаг вверх:
    
    <img src="https://render.githubusercontent.com/render/math?math=%7B%5Ctext%7BFPR%7D%7D_%7Bi%7D%20%3A%3D%20%7B%5Ctext%7BFPR%7D%7D_%7Bi-1%7D%3B%5C%3B%5C%3B%7B%5Ctext%7BTPR%7D%7D_%7Bi%7D%20%3A%3D%7B%5Ctext%7BTPR%7D%7D_%7Bi-1%7D%20%2B%5Cfrac%7B1%7D%7B%7Bl%7D_%7B%2B%7D%7D">
    
