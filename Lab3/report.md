% Лабораторная работа № 3 «Обобщённые классы в Scala»
% 26 марта 2025 г.
% Вениамин Шемякин, ИУ9-62Б

# Цель работы
Целью данной работы является приобретение навыков разработки обобщённых классов на 
языке Scala с использованием неявных преобразований типов.

# Индивидуальный вариант
Класс Formula[T], представляющий формулу, состоящую из имён переменных, констант типа T 
и бинарных операций. Ассортимент операций зависит от типа T: если определён Numeric[T], 
то должны присутствовать четыре базовых арифметических операций; если T — это string, 
то должна присутствовать только операция сложения, обозначающая конкатенацию.

Для создания объектов класса Formula[T] доступны два конструктора: первый принимает 
имя переменной и создаёт формулу, состоящую из единственной переменной; второй конструктор принимает 
значение типа T и создаёт формулу, состоящую из единственной константы. 
Все остальные формулы возвращаются бинарными операциями.

В классе Formula[T] должен быть определён метод, принимающий отображение имён 
переменных в их значения и считающий значение формулы.
# Реализация

```scala
trait Additive[T] {
  def plus(x: T, y: T): T
}

object Additive {
  implicit def numericAdditive[T](implicit num: Numeric[T]): Additive[T] =
    new Additive[T] {
      def plus(x: T, y: T): T = num.plus(x, y)
    }
  implicit val stringAdditive: Additive[String] =
    new Additive[String] {
      def plus(x: String, y: String): String = x + y
    }
}

trait Formula[T] {
  def eval(env: Map[String, T]): T
}

class Constant[T](val value: T) extends Formula[T] {
  def eval(env: Map[String, T]): T = value
}

class Variable[T](val name: String) extends Formula[T] {
  def eval(env: Map[String, T]): T =
    env.getOrElse(name, throw new NoSuchElementException(s"Переменная '$name' не определена"))
}

class Add[T](val left: Formula[T], val right: Formula[T])(implicit val additive: Additive[T])
    extends Formula[T] {
  def eval(env: Map[String, T]): T =
    additive.plus(left.eval(env), right.eval(env))
}

class Subtract[T](val left: Formula[T], val right: Formula[T])(implicit val num: Numeric[T])
    extends Formula[T] {
  def eval(env: Map[String, T]): T =
    num.minus(left.eval(env), right.eval(env))
}

class Multiply[T](val left: Formula[T], val right: Formula[T])(implicit val num: Numeric[T])
    extends Formula[T] {
  def eval(env: Map[String, T]): T =
    num.times(left.eval(env), right.eval(env))
}

class Divide[T](val left: Formula[T], val right: Formula[T])(implicit val frac: Fractional[T])
    extends Formula[T] {
  def eval(env: Map[String, T]): T =
    frac.div(left.eval(env), right.eval(env))
}

object FormulaOps {
  implicit class AddOps[T](val left: Formula[T]) extends AnyVal {
    def +(right: Formula[T])(implicit additive: Additive[T]): Formula[T] =
      new Add(left, right)
  }
  
  implicit class NumericOps[T](val left: Formula[T]) extends AnyVal {
    def -(right: Formula[T])(implicit num: Numeric[T]): Formula[T] =
      new Subtract(left, right)
    def *(right: Formula[T])(implicit num: Numeric[T]): Formula[T] =
      new Multiply(left, right)
    def /(right: Formula[T])(implicit frac: Fractional[T]): Formula[T] =
      new Divide(left, right)
  }
}

object Formula {
  def variable[T](name: String): Formula[T] = new Variable[T](name)
  def constant[T](value: T): Formula[T] = new Constant[T](value)
}

object Example extends App {
  import FormulaOps._
  val numFormula: Formula[Int] =
    (Formula.variable[Int]("x") + Formula.constant(10)) * Formula.constant(2)
  val envNum = Map("x" -> 5)
  println(s"Формула 1: ${numFormula.eval(envNum)}")
  val strFormula: Formula[String] =
    Formula.variable[String]("s") + Formula.constant[String](" world")
  val envStr = Map("s" -> "Hello")
  println(s"Формула 2: ${strFormula.eval(envStr)}")
}

```

# Тестирование

Результат запуска программы:

```
Формула 1: 30
Формула 2: Hello world
```

# Вывод
В ходе данной лабораторной работы я научился работать с обобщенными классами, а также изучил 
механику неявного преобразования типов. Было интересно создать класс, который позволяет 
непосредственно подставлять значения в формулу и считать результат.