# Linear Algebra

## Линейная алгебра на Clojure

* Разработайте функции для работы с объектами линейной алгебры, которые представляются следующим образом:
  + скаляры – числа
  + векторы – векторы чисел;
  + матрицы – векторы векторов чисел.
  
* Функции над векторами:
  - v+/v-/v*/vd – покоординатное сложение/вычитание/умножение/деление;
  - scalar/vect – скалярное/векторное произведение;
  - v*s – умножение на скаляр.
  
* Функции над матрицами:
  - m+/m-/m*/md – поэлементное сложение/вычитание/умножение/деление;
  - m*s – умножение на скаляр;
  - m*v – умножение на вектор;
  - m*m – матричное умножение;
  - transpose – транспонирование;

---

### Сложный вариант.
* Ко всем функциям должны быть указаны контракты. Например, нельзя складывать вектора разной длины.
* Все функции должны поддерживать произвольное число аргументов. Например (v+ [1 2] [3 4] [5 6]) должно быть равно [9 12].

---

При выполнении задания следует обратить внимание на:\
Применение функций высшего порядка.\
Выделение общего кода для операций.

## Модификации
 * *Cuboid*
    * Назовем _кубоидом_ трехмерную прямоугольную таблицу чисел.
    * Добавьте операции поэлементного 
        сложения (`c+`), вычитания (`c-`), умножения (`c*`) и деления (`cd`) 
        кубоидов.
        Например, `(с+ [[[1] [2]] [[3] [4]]] [[[5] [6]] [[7] [8]]])` 
        должно быть равно `[[[6] [8]] [[10] [12]]]`.
