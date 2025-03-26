% Лабораторная работа № 1 «Введение в функциональное
  программирование на языке Scala»
% 12 февраля 2025 г.
% Вениамин Шемякин, ИУ9-62Б

# Цель работы
Целью данной работы является ознакомление с программированием на языке Scala на основе чистых функций.

# Индивидуальный вариант
Закаренная функция ```
sorted: ((Int, Int) => Boolean) => (List[Int] => Boolean)```
, принимающая функцию сравнения двух целых чисел и возвращающая функцию,
 определяющую, является ли список целых чисел отсортированным в соответствии с функцией сравнения.

# Реализация и тестирование
```
val is_sorted: ((Int, Int) => Boolean) => (List[Int] => Boolean) = {
  case comparison => {
    case Nil => true
    case _ :: Nil => true
    case x :: y :: tail => comparison(x, y) && is_sorted(comparison)(y :: tail)
  }
}

object Main extends App {
  val asc = (x: Int, y: Int) => x <= y
  val desc = (x: Int, y: Int) => x >= y

  val list1 = List(1, 2, 3, 4)
  val list2 = List(4, 3, 2, 1)
  val list3 = List(1, 3, 2, 4)
  val list4 = List(1)
  val list5 = List()

  println(s"${is_sorted(asc)(list1)}")
  println(s"${is_sorted(asc)(list2)}")
  println(s"${is_sorted(asc)(list3)}")
  println(s"${is_sorted(asc)(list4)}")
  println(s"${is_sorted(asc)(list5)}")

  println("---------------------")

  println(s"${is_sorted(desc)(list1)}")
  println(s"${is_sorted(desc)(list2)}")
  println(s"${is_sorted(desc)(list3)}")
  println(s"${is_sorted(desc)(list4)}")
  println(s"${is_sorted(desc)(list5)}")

}
```

```
true
false
false
true
true
---------------------
false
true
false
true
true
```
# Вывод
В ходе данной лабораторной работы я ознакомился с программированием на языке Scala на основе чистых функций,
 реализовав вариант с проверкой на то, является ли массив отсортированным в соответствии
  с заданной функцией сортировки.